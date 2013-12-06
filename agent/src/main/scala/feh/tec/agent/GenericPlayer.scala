package feh.tec.agent

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.ExecutionContext
import feh.tec.agent.GenericDeterministicGame.Game2
import feh.tec.util._
import feh.tec.agent.DeterministicMutableGenericGameEnvironment.Env2
import feh.tec.agent.AgentDecision.ExplainedActionStub
import scala.collection

abstract class GenericPlayer[Game <: GenericGame, Env <: GenericGameEnvironment[Game, Env]](
                             val player: Game#Player, 
                             val executionLoop: PlayerAgent.Exec[Game, Env], 
                             val env: Env#Ref,
                             decisionTaken: GenericPlayer[Game, Env]#ActionExplanation => Unit = (_: GenericPlayer[Game, Env]#ActionExplanation) => {}
                            ) 
  extends DummyPlayer[Game, Env] with PlayerAgent.Resettable[Game, Env] with PlayerAgent.RandomBehaviour[Game, Env]
{
  final type Player = Game#Player
  final type Strategy = Player#Strategy
  final type Utility = Game#Utility

  def lastDecision: Option[ActionExplanation] = None
  def notifyDecision(a: GenericPlayer[Game, Env]#ActionExplanation): Unit = decisionTaken(a)

  var randomChance: InUnitInterval = 0

  def preference_=(pref: collection.Map[Strategy, Double] with ValueSumInUnitInterval[Strategy]){
    ??? // todo
  }

  var preference = new MutableHashMapValueSumInUnitInterval[Strategy](initPreference: _*)
  protected def initPreference = player.availableStrategies.zipMap(_ => 0d).toSeq

  def updatePreference(strategy: Strategy, prob: Double){
    preference(strategy) = prob
  }

  def reset(): Unit = {
    randomChance = 0
    preference ++= initPreference
  }
}

class GenericExecutor[Game <: GenericGame, Env <: GenericGameEnvironment[Game, Env]]( val execControlTimeout: FiniteDuration,
                                                                                      val onSuccess: () => Unit = () => {})
                                                                                    (implicit val executionContext: ExecutionContext) extends ByTurnExec[Game, Env]

object GenericPlayer{
  trait DummyBestStrategyChooser2[Game <: Game2] extends GenericPlayer[Game, Env2[Game]]{
    lazy val game = sense(env)
    lazy val opponent = game.players.filter(player !=).head.asInstanceOf[Player]

    def probableUtility: Map[Strategy, Double] ={
      player.availableStrategies.map{
        myStrategy => myStrategy ->
          (.0 /: opponent.availableStrategies){
            case (acc, opStrategy) =>
              val k = Map(player -> myStrategy, opponent -> opStrategy)
              val utility = game.layout.asInstanceOf[Map[Player, Strategy] => Map[Player, Utility]](k)(player)
              acc + utility
          }
      }.toMap
    }


    def decide(currentPerception: Perception): ActionExplanation = ExplainedActionStub{
      val expectedUtil = probableUtility
      StrategicChoice[Player](
        player,
        (game.target match{
          case game.Max => expectedUtil.maxBy(_._2)
          case game.Min => expectedUtil.minBy(_._2)
        })._1
      )
    }
  }
}