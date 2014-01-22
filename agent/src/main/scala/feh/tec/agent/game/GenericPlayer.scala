package feh.tec.agent.game

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.ExecutionContext
import feh.tec.util._
import feh.tec.agent.AgentDecision.ExplainedActionStub
import scala.collection
import scala.util.Random
import scala.collection.mutable
import feh.tec.agent.game.AbstractGenericGame.Game2
import feh.tec.agent.{Past => APast, AgentWithActor, DecisiveAgent}
import feh.util.{ValueSumInUnitInterval, MutableHashMapValueSumInUnitInterval, InUnitInterval}

abstract class GenericPlayer[Game <: GenericGame, Env <: GenericGameEnvironment[Game, Env]](
                             val player: Game#Player,
                             val executionLoop: PlayerAgent.Exec[Game, Env], 
                             val env: Env#Ref,
                             decisionTaken: GenericPlayer[Game, Env]#ActionExplanation => Unit = (_: Any) => {}
                            ) 
  extends PlayerAgent[Game, Env] with PlayerAgent.Resettable[Game, Env]
{
  agent: DecisiveAgent[Env, PlayerAgent.Exec[Game, Env]] =>
  
  final type Player = Game#Player
  final type Strategy = Player#Strategy
  final type Utility = Game#Utility

  protected def actionToStrategicChoice(a: Env#Action): Option[StrategicChoice[Game#Player]] = PartialFunction.condOpt(a){
    case ch: StrategicChoice[Game#Player] => ch
  }

  def lastDecision: Option[ActionExplanation] = None
  def notifyDecision(a: ActionExplanation): Unit = decisionTaken(a)

  def resetSeq: List[Lifted[Unit]] = Nil
  def reset(): Unit = resetSeq.foreach(_())
}

trait GenericSimplePlayer[Game <: GenericGame, Env <: GenericGameEnvironment[Game, Env]]
  extends GenericPlayer[Game, Env] with SimplePlayer[Game, Env]

trait GenericWiserPlayer[Game <: GenericGame, Env <: GenericGameEnvironment[Game, Env]]
  extends WiserPlayer[Game , Env] with PlayerAgent.Resettable[Game, Env]

trait GenericStatefulPlayer[Game <: GenericGame, Env <: GenericGameEnvironment[Game, Env], State]
  extends StatefulPlayer[Game, Env, State]
{
  self: AgentWithActor[Env, PlayerAgent.Exec[Game, Env]] =>
}

class GenericExecutor[Game <: GenericGame, Env <: GenericGameEnvironment[Game, Env]]( val execControlTimeout: FiniteDuration,
                                                                                      val onSuccess: () => Unit = () => {})
                                                                                    (implicit val executionContext: ExecutionContext) extends ByTurnExec[Game, Env]

object GenericPlayer{
  trait BestStrategyChooser[Game <: GenericGame, Env <: GenericGameEnvironment[Game, Env]]
    extends GenericPlayer[Game, Env]
  {
    self: DecisiveAgent[Env, PlayerAgent.Exec[Game, Env]] =>

    val game: Game
    lazy val gameLayout = game.layout.asInstanceOf[Map[Player, Strategy] => Map[Player, Utility]]

    lazy val opponents = game.players.filter(player !=).map(_.asInstanceOf[Game#Player])

    final def chooseBestStrategy(expectedUtil: Map[Strategy, Double]) =
      StrategicChoice[Player](
        player,
        (game.target match{
          case game.Max => expectedUtil.filterMax(_._2).randomChoose
          case game.Min => expectedUtil.filterMin(_._2).randomChoose
        })._1
      )

  }

  trait SimpleBestStrategyChooser[Game <: GenericGame,
                                 Env <: GenericGameEnvironment[Game, Env] ]
    extends BestStrategyChooser[Game, Env] with GenericSimplePlayer[Game, Env]
  {
    lazy val game = sense(env)

    protected def expectedUtility: Map[Strategy, Double]

    def decide(currentPerception: Perception): ActionExplanation = ExplainedActionStub{
      chooseBestStrategy(expectedUtility)
    }
  }

  trait SimpleBestStrategyChooser2[Game <: Game2,
                                  Env <: GenericGameEnvironment[Game, Env]]
    extends SimpleBestStrategyChooser[Game, Env]
  {
    lazy val opponent = opponents.head

    protected def expectedUtility: Map[Strategy, Double] ={
      player.availableStrategies.map{
        myStrategy => myStrategy ->
          (.0 /: opponent.availableStrategies){
            case (acc, opStrategy) =>
              val k = Map(player -> myStrategy, opponent -> opStrategy)
              val utility = gameLayout(k)(player)
              acc + utility
          }
      }.toMap
    }
  }

  /**
   * Should be mixed in AFTER all other decision traits
   */
  sealed trait AbstractRandomBehaviour[Game <: GenericGame, Env <: GenericGameEnvironment[Game, Env]]
    extends GenericPlayer[Game, Env] with PlayerAgent.RandomBehaviour[Game, Env]
  {
    agent: DecisiveAgent[Env, PlayerAgent.Exec[Game, Env]] =>

    var randomChance: InUnitInterval = 0
    var preference = new MutableHashMapValueSumInUnitInterval[Strategy](initPreference: _*)

    def preference_=(pref: collection.Map[Strategy, Double] with ValueSumInUnitInterval[Strategy]){ ??? /* todo */ }
    protected def initPreference = player.availableStrategies.zipMap(_ => 0d).toSeq

    def updatePreference(strategy: Strategy, prob: Double){
      preference(strategy) = prob
    }

    override def resetSeq = super.resetSeq ::: List(
      liftUnit(randomChance = 0),
      liftUnit(preference ++= initPreference)
    )

    protected val orderedStrategies = player.availableStrategies.toList

    def minimumPreferenceSumThreshold = .1

    protected def buildActionExplanation(s: StrategicChoice[Player]): ActionExplanation

    def irrationalBehaviour = buildActionExplanation{
      def choose: Strategy = {
        val rand = Random.nextDouble()
        Y[(Double, List[(Strategy, Double)]), Strategy](
          rec => {
            case (acc, Nil) => choose
            case (acc, (str, prob) :: tail) =>
              val next = acc + prob
              if(acc < rand && next > rand) str
              else rec(next, tail)
          }
        )(0d -> orderedStrategies.zipMap(preference).toList)
      }

      val choice = if(preference.values.sum < minimumPreferenceSumThreshold) orderedStrategies.randomChoice else choose
      StrategicChoice[Game#Player](player, choice)
    }
  }
  
  trait SimpleRandomBehaviour[Game <: GenericGame, Env <: GenericGameEnvironment[Game, Env]]
    extends AbstractRandomBehaviour[Game, Env] with SimplePlayer[Game, Env]
  {
    self: GenericSimplePlayer[Game, Env] =>

    protected def buildActionExplanation(s: StrategicChoice[Player]) = ExplainedActionStub(s)

    abstract override def decide(currentPerception: Perception): ActionExplanation = {
      val r = Random.nextDouble()
      if(r < randomChance) irrationalBehaviour else super.decide(currentPerception)
    }
  }

  trait WiserRandomBehaviour[Game <: GenericGame, Env <: GenericGameEnvironment[Game, Env]]
    extends AbstractRandomBehaviour[Game, Env] with WiserPlayer[Game, Env]
  {
    self: GenericWiserPlayer[Game, Env] =>

    abstract override def decide(past: P, currentPerception: Perception): ActionExplanation = {
      val r = Random.nextDouble()
      if(r < randomChance) irrationalBehaviour else super.decide(past, currentPerception)
    }
  }

  trait OpponentsStatistics[Game <: GenericGame,
                         Env <: GenericGameEnvironment[Game, Env]]
    extends BestStrategyChooser[Game, Env]
  {
    self: DecisiveAgent[Env, PlayerAgent.Exec[Game, Env]] =>
    
    type OpponentsChoicesCount = Map[Game#Player, Map[Game#Player#Strategy, Int]]
    
    def opponentsChoicesCount: OpponentsChoicesCount = adversaryChoiceFrequency.toMap.mapValues(_.toMap)

    private def initAdversaryChoiceFrequency: Set[(Game#Player, mutable.Map[Game#Player#Strategy, Int])] = opponents.zipMap{
                                                    // if 0, all expected probabilities will be 0 and the first move will be random, to avoid it set 1
      opponent => mutable.Map(opponent.availableStrategies.toSeq.map(_ -> 0): _*)
    }
    protected val adversaryChoiceFrequency =
      mutable.Map.empty[Game#Player, mutable.Map[Game#Player#Strategy, Int]]  $$ {
      _ ++= initAdversaryChoiceFrequency
    }
    private def updateProbabilities(choices: Game#PlayersChoices) = choices.foreach{
      case (`player`, _) =>
      case (pl, str) => adversaryChoiceFrequency(pl)(str) += 1
    }

    env.listenToEndOfTurn{
      case (_, choices, _) => updateProbabilities(choices)
    }

    override def resetSeq: List[() => Unit] = super.resetSeq ::: List({
      adversaryChoiceFrequency ++= initAdversaryChoiceFrequency
    }.liftUnit)
  }

  /** A player for playing <b>fictitious</b> games
   * A [[feh.tec.agent.WiserAgent]] that uses statistics gathered by [[feh.tec.agent.game.GenericPlayer.OpponentsStatistics]]'s as the past;
   * ignores current perception.
   */
  trait ExpectedUtilityByPastOnly[Game <: GenericGame,
                        Env <: GenericGameEnvironment[Game, Env]]
    extends BestStrategyChooser[Game, Env] with GenericWiserPlayer[Game, Env] with OpponentsStatistics[Game, Env]
  {
    object Past extends APast{
      type History = OpponentsChoicesCount

      def history: History = opponentsChoicesCount
    }
    type P = Past.type
    def past = Past

    lazy val game: Game = sense(env)._1

    protected def probabilityByCount(c: Int) = InUnitInterval(env.turn match{ // returns count as probability on first move, this can be used to setup 1 move
      case Turn(0) if c <= 1 => c.toDouble
      case Turn(x) => c.toDouble / (x + 1)
    })

    def opponentsStrategiesCombinations: Set[Set[(Player, Player#Strategy)]]

    protected def expectedUtility(past: P): Map[Strategy, Double] = player.availableStrategies.zipMap{
      myStrategy =>
        def probability(player: Player, strategy: Player#Strategy) = past.history(player)(strategy) |> probabilityByCount
        val myChoice = player -> myStrategy
        val probableOpponentChoices = opponentsStrategiesCombinations map{
          _.map{ // possible choices options
            case (opponent, strategy) => (opponent, strategy) -> probability(opponent, strategy)
          }.toMap
        }
        val probableUtilities = probableOpponentChoices.zipMap{
          possibleChoicesLayout =>
            val opponentsChoices = possibleChoicesLayout.keySet.toMap
            val prob = possibleChoicesLayout.values.product
            val allUtilities = gameLayout(opponentsChoices + myChoice)
            val myUtility = allUtilities(player)
            myUtility -> prob
        }.toMap

        probableUtilities.zipWithIndex.map{
          case ((variant, (ut, prob)), i) =>
            val probUtil = ut * prob
            println(s"probable utility of $player selecting strategy $myStrategy (var$i): ut=$ut, prob=$prob, probUtil=$probUtil, verbose: $variant")
            probUtil
        }.sum
    }.toMap

    protected def buildActionExplanation(ch:  StrategicChoice[Player]): ActionExplanation

    def decide(past: P, currentPerception: Perception): ActionExplanation =
      buildActionExplanation(chooseBestStrategy(expectedUtility(past)))
  }

  trait ExpectedUtilityByPastOnly2[Game <: Game2,
                         Env <: GenericGameEnvironment[Game, Env]]
    extends ExpectedUtilityByPastOnly[Game, Env]
  {
    lazy val opponent = opponents.head

    def opponentsStrategiesCombinations: Set[Set[(Player, Player#Strategy)]] =
      opponent.availableStrategies.map(s => Set(opponent -> s.asInstanceOf[Strategy]))

  }
}