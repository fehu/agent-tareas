package feh.tec.agent.game

import feh.tec.util._
import concurrent.{Promise, ExecutionContext, Future, Await}
import java.util.UUID
import akka.pattern._
import scala.concurrent.duration._
import scala.collection.mutable
import akka.actor.{ActorSystem, Props, Actor, ActorRef}
import feh.tec.util.HasUUID.AsyncSendMsgHasUUIDWrapper
import akka.event.Logging
import feh.tec.agent._
import scala.Some
import feh.tec.agent.AgentDecision.ExplainedActionStub
import feh.tec.agent.AgentId
import akka.util.Timeout

trait GameEnvironment[Game <: AbstractGame, Env <: GameEnvironment[Game, Env]] extends Environment[Env]{
  self : Env =>

  final type Coordinate = Null
  final type State = Null
  final type Global = GameScore[Game]
  type Action <: GameAction

  type Ref <: GameRef[Game, Env]
  final type Choice = StrategicChoice[Game#Player]
  final type Score = GameScore[Game]

  def game: Game
  def play(choices: StrategicChoices[Game#Player]): Score

  implicit def utilityIsNumeric = game.utilityIsNumeric.asInstanceOf[Numeric[Game#Utility]]

  def updateScores(scoresUpdate: Score)
  def setScore(score: Score)

  def bulkAffected(act: StrategicChoices[Game#Player]): SideEffect[Env]

  // those are not used
  def states: PartialFunction[Null, Null] = PartialFunction.empty
  def definedAt = Nil
  def stateOf(c: Null) = None
  def visibleStates = Map()
  def agentPosition(ag: AgentId) = None
  def effects: PartialFunction[GameAction, Env => Env] = PartialFunction.empty
}

trait GameAction extends AbstractAction
case class StrategicChoice[P <: AbstractGame#Player](player: P, strategy: P#Strategy) extends GameAction
case class StrategicChoices[P <: AbstractGame#Player](choices: Set[StrategicChoice[P]]) extends GameAction{
  def toMap: Map[P, P#Strategy] = choices.map(ch => ch.player -> ch.strategy).toMap
}
case class GameScore[Game <: AbstractGame](utility: Map[Game#Player, Game#Utility])(implicit num: Numeric[Game#Utility]){
  def update(scoreUpdates: Map[Game#Player, Game#Utility]): GameScore[Game] =
    GameScore(utility.zipByKey(scoreUpdates).mapValues((num.plus _).tupled))
  def update(scoreUpdate: GameScore[Game]): GameScore[Game] = update(scoreUpdate.utility)
}
object GameScore{
  def zero[Game <: AbstractGame](strategy: Game) =
    GameScore[Game](strategy.players.map(_ -> strategy.utilityIsNumeric.zero).toMap)(strategy.utilityIsNumeric.asInstanceOf[Numeric[Game#Utility]])
}

trait DeterministicGameEnvironment[Game <: AbstractDeterministicGame, Env <: DeterministicGameEnvironment[Game, Env]]
  extends GameEnvironment[Game, Env] with Deterministic[Env]
{
  self: Env =>

  //  Map[PlayersChoices, PlayersUtility]
//  type PlayersChoices = Map[Player, Player#Game]
//  type PlayersUtility = Map[Player, Utility]
  //
  final type Player = Game#Player

  private def strategies = game.layout.asInstanceOf[PartialFunction[Map[Player, Player#Strategy], Map[Player, Game#Utility]]]

  def play(choices: StrategicChoices[Game#Player]): Score = GameScore[Game](strategies(choices.toMap))
}

trait MutableGameEnvironmentImpl[Game <: AbstractGame, Env <: MutableGameEnvironmentImpl[Game, Env]]
  extends GameEnvironment[Game, Env] with MutableEnvironment[Env]
{
  self: Env =>

  def initGlobalState: GameScore[Game] = GameScore.zero(game)

  def affected(act: Env#Action): SideEffect[Env] = ???
  def bulkAffected(act: StrategicChoices[Game#Player]): SideEffect[Env] = SideEffect{
    val score = play(act)
    _lastScore = Option(score)
    updateScores(score)
    this
  }

  protected var _lastScore: Option[GameScore[Game]] = None
  def lastScore: Option[GameScore[Game]] = _lastScore

  def updateScores(scoresUpdate: Score){
    globalState = globalState.update(scoresUpdate)
  }

  def setScore(score: Score){
    globalState = score
  }

  def initStates: PartialFunction[Null, Null] = null
  override def states = super[GameEnvironment].states
}

case class Turn protected (id: Int){
  def next: Turn = copy(id+1)
}

object Turn{
  def first = Turn(0)
}

trait GameRef[Game <: AbstractGame, Env <: GameEnvironment[Game, Env]] extends EnvironmentRef[Env]
{
  def turn: Turn
  //  def asyncTurn: Future[Turn]
  def choose(choice: StrategicChoice[Game#Player])
  def chooseAndWait(choice: StrategicChoice[Game#Player])
  def strategies: Game
  def listenToEndOfTurn(f: (Turn, Game#PlayersChoices, Game#PlayersUtility) => Unit)
//  def blocking: BlockingApi = ???
//  def async: AsyncApi = ???
}

trait GameCoordinator[Game <: AbstractGame, Env <: GameEnvironment[Game, Env]] extends EnvironmentOverseer[Env]{
  def currentTurn: Turn
  def registerChoice(choice: StrategicChoice[Game#Player])
  def registerChoiceAndWait(choice: StrategicChoice[Game#Player])
  def reset()

  // no snapshots
  def snapshot: EnvironmentSnapshot[Env] with Env = ???
}

trait GameCoordinatorWithActor[Game <: AbstractGame, Env <: GameEnvironment[Game, Env]]
  extends GameCoordinator[Game, Env] with EnvironmentOverseerWithActor[Env]
{
  coordinator =>

  import GameCoordinatorActor._

  def awaitEndOfTurnTimeout: FiniteDuration

  def bulkAffect = env.bulkAffected _

  def currentTurn: Turn = Await.result(asyncCurrentTurn, defaultBlockingTimeout millis)

  def asyncCurrentTurn: Future[Turn] = GetTurn() |> {
    msg => actorRef.send(msg).awaitingResponse[TurnResponse](defaultFutureTimeout millis).map(_.turn)
    //(actorRef ? msg)(defaultFutureTimeout).mapTo[TurnResponse].havingSameUUID(msg).map(_.turn)
  }

  def registerChoice(choice: StrategicChoice[Game#Player]): Unit = actorRef ! RegisterChoice(choice)
  def registerChoiceAndWait(choice: StrategicChoice[Game#Player]): Unit = RegisterChoiceAndWait(choice) |> {
    msg =>
      implicit def timeout = awaitEndOfTurnTimeout: Timeout
      Await
      .result(actorRef ? msg, awaitEndOfTurnTimeout)
      .tryAs[TurnEnded].havingSameUUID(msg).ensuring(_.nonEmpty)
  }

  trait GameRefBaseImpl extends GameRef[Game, Env] with BaseEnvironmentRef{
    def turn: Turn = currentTurn
    def choose(choice: StrategicChoice[Game#Player]): Unit = registerChoice(choice)
    def chooseAndWait(choice: StrategicChoice[Game#Player]): Unit = registerChoiceAndWait(choice)
    def strategies: Game = coordinator.env.game
    def listenToEndOfTurn(f: (Turn, Game#PlayersChoices, Game#PlayersUtility) => Unit): Unit = coordinator.listenToEndOfTurn(f)
  }

  def listenToEndOfTurn(f: (Turn, Game#PlayersChoices, Game#PlayersUtility) => Unit) {
    _endOfTurnListeners :+=  f
  }
  protected var _endOfTurnListeners: Seq[(Turn, Game#PlayersChoices, Game#PlayersUtility) => Unit] = Nil
  def endOfTurnListeners = _endOfTurnListeners

  protected def actorProps = Props(classOf[GameCoordinatorActor[Game, Env]], coordinator)

  def actorSystem: ActorSystem

  lazy val actorRef: ActorRef = actorSystem.actorOf(actorProps)

  def lastScore: Option[GameScore[Game]]

  def reset(): Unit = Await.ready((actorRef ? Reset)(defaultFutureTimeout), defaultFutureTimeout millis)
}

object GameCoordinatorActor{
  case class GetTurn() extends UUIDed
  case class TurnResponse(uuid: UUID, turn: Turn) extends HasUUID
  case class RegisterChoice[Game <: AbstractGame](choice: StrategicChoice[Game#Player]) extends UUIDed
  case class RegisterChoiceAndWait[Game <: AbstractGame](choice: StrategicChoice[Game#Player]) extends UUIDed
  @deprecated case class AwaitEndOfTurn() extends UUIDed
  case class TurnEnded(uuid: UUID) extends HasUUID
  case object Reset
}

class GameCoordinatorActor[Game <: AbstractGame, Env <: GameEnvironment[Game, Env] { type Action = StrategicChoices[Game#Player] }]
  (coordinator: GameCoordinatorWithActor[Game, Env])
  extends Actor
{
  import GameCoordinatorActor._

  val log = Logging(context.system, this)

  private var turn = Turn.first
  private val currentTurnChoicesMap = mutable.HashMap.empty[Game#Player, Game#Player#Strategy]
  private val awaitingEndOfTurn = mutable.HashMap.empty[ActorRef, UUID]

  protected def currentTurnChoices = currentTurnChoicesMap.map((StrategicChoice.apply[Game#Player] _).tupled).toSet
  protected def newChoice(choice: StrategicChoice[Game#Player]) = {
    assert(! currentTurnChoicesMap.keySet.contains(choice.player), s"${choice.player}'s choice has already been registered")
    currentTurnChoicesMap += choice.player -> choice.strategy
  }
  protected def awaiting(waiting: ActorRef, id: UUID) = {
    assert(!awaitingEndOfTurn.keySet.contains(waiting), s"$waiting is already waiting end of turn")
    awaitingEndOfTurn += waiting -> id
  }

  protected def nextTurn() = {
    val next = turn.next
    turn = next
    currentTurnChoicesMap.clear()
    awaitingEndOfTurn.clear()
    next
  }

  protected def turnFinished_? = currentTurnChoicesMap.keySet == coordinator.env.game.players

  protected def endTurn() = {
    coordinator.bulkAffect(StrategicChoices(currentTurnChoices)).flatExec
    coordinator.lastScore
  } 
  protected def notifyAwaiting() = awaitingEndOfTurn.foreach{ case (waiting, id) => waiting ! TurnEnded(id) }

  protected val history = mutable.Map.empty[Turn, (Game#PlayersChoices, Game#PlayersUtility)]
  protected def guardHistory(score: GameScore[Game]){
    val choices = currentTurnChoicesMap.toMap.asInstanceOf[Game#PlayersChoices]
    val utility = score.utility.asInstanceOf[Game#PlayersUtility]
    history += turn-> ( choices -> utility)
    lastHistory = (turn, choices, utility)
  }
  private var lastHistory: (Turn, Game#PlayersChoices, Game#PlayersUtility) = _

  protected def notifyEndOfTurnListeners() =
    coordinator.endOfTurnListeners.foreach(t => Future{ t.tupled(lastHistory) }(context.dispatcher))

  private case class TurnFinished(id: Long)

  def receive: Actor.Receive = {
    case msg@GetTurn() => sender ! TurnResponse(msg.uuid, turn)
    case TurnFinished(id) if id == turn.id =>
      val Some(score) = endTurn()
      guardHistory(score)
      notifyAwaiting()
      notifyEndOfTurnListeners()
      nextTurn()
    case RegisterChoice(choice) =>
      newChoice(choice.asInstanceOf[StrategicChoice[Game#Player]])
    case msg@RegisterChoiceAndWait(choice) =>
      newChoice(choice.asInstanceOf[StrategicChoice[Game#Player]])
      awaiting(sender, msg.uuid)
      if(turnFinished_?) self ! TurnFinished(turn.id)
    case Reset =>
      notifyAwaiting()
      turn = Turn.first
      currentTurnChoicesMap.clear()
      awaitingEndOfTurn.clear()
      history.clear()
      lastHistory = null
      coordinator.env.setScore(GameScore.zero(coordinator.env.game))
      notifyEndOfTurnListeners()
      sender ! Reset
  }
}


trait MutableGameCoordinator[Game <: AbstractGame, Env <: MutableGameEnvironmentImpl[Game, Env]]
  extends GameCoordinator[Game, Env]
  with MutableEnvironmentOverseer[Env]
{


  def lastScore: Option[GameScore[Game]] = env.lastScore
}

trait AbstractGame{
  type Utility
  implicit def utilityIsNumeric: Numeric[Utility]

  lazy val playerNameRegex = """.*\$(\w+)\$(\w+)\$.*""".r

  trait Player{
    def name: String
    type Strategy
    def availableStrategies: Set[Strategy]

    override def toString: String = playerNameRegex.findAllIn(getClass.getName).matchData.toSeq.head |> {
      mtch => mtch.group(1) + "#" + mtch.group(2)
    }
  }

  type PlayersChoices = Map[Player, Player#Strategy]
  type PlayersUtility = Map[Player, Utility]

//  case class PlayerChoice[P <: PlayerRef](player: P, strategy: P#Strategy)
//  case class PlayerUtility[P <: PlayerRef](player: P, utility: Utility)
  sealed trait Target
  case object Max extends Target
  case object Min extends Target

  def target: Target

  def nPlayers: Int
  def players: Set[Player]
  def layout: PlayersChoices => PlayersUtility
}

trait AbstractDeterministicGame extends AbstractGame{
//  override val layout: Map[PlayersChoices, PlayersUtility]
}

trait AbstractTurnBasedGame extends AbstractGame{
  def playersTurnOrdering: Ordering[Player]
  def playersInTurnOrder = players.toList.sorted(playersTurnOrdering)
}

object PlayerAgent {
  type Exec[Game <: AbstractGame, Env <: GameEnvironment[Game, Env]] = SimultaneousAgentsExecutor
  trait Resettable[Game <: AbstractGame, Env <: GameEnvironment[Game, Env]] extends PlayerAgent[Game, Env]{
    agent: DecisiveAgent[Env, PlayerAgent.Exec[Game, Env]] =>

    def reset()
  }

  trait RandomBehaviour[Game <: AbstractGame, Env <: GameEnvironment[Game, Env]] extends PlayerAgent[Game, Env]{
    agent: DecisiveAgent[Env, PlayerAgent.Exec[Game, Env]] =>

    def randomChance: InUnitInterval
    def randomChance_=(p: InUnitInterval)
    def preference: collection.Map[Game#Player#Strategy, Double] with ValueSumInUnitInterval[Game#Player#Strategy]
    def preference_=(pref: collection.Map[Game#Player#Strategy, Double] with ValueSumInUnitInterval[Game#Player#Strategy])
    def updatePreference(strategy: Game#Player#Strategy, prob: Double)
  }
}

trait PlayerAgent[Game <: AbstractGame, Env <: GameEnvironment[Game, Env]]
  extends Agent[Env, PlayerAgent.Exec[Game, Env]]
  with SimultaneousAgentExecution[Env, PlayerAgent.Exec[Game, Env]]
{
  agent: DecisiveAgent[Env, PlayerAgent.Exec[Game, Env]] =>

  def player: Game#Player    
    
  executionLoop.register(this)

  protected def actionToStrategicChoice(a: Env#Action): Option[StrategicChoice[Game#Player]]

  override def act(a: Env#Action): SideEffect[Env#Ref] = SideEffect{
    actionToStrategicChoice(a).map(env.chooseAndWait)
    env
  }.flatExec
}

trait DummyPlayer[Game <: AbstractGame, Env <: GameEnvironment[Game, Env]]
  extends PlayerAgent[Game, Env]
  with DummyAgent[Env, PlayerAgent.Exec[Game, Env]]
{
  type ActionExplanation = ExplainedActionStub[Env#Action]
  type DetailedPerception = AbstractDetailedPerception
  type Perception = Game

  def player: Game#Player

  def sense(env: EnvRef): Perception = env.strategies

  def detailed(env: EnvRef, c: Null): Option[AbstractDetailedPerception] = None
}

trait ByTurnExec[Game <: AbstractGame, Env <: GameEnvironment[Game, Env]] extends PlayerAgent.Exec[Game, Env]{
  type Execution = Exec

  trait Exec{
    def nextTurn(): Future[Exec]
  }

  implicit def executionContext: ExecutionContext

  def isCurrentlyExecuting: Boolean = executing_?
  private var executing_? = false

  lazy val execution = new Exec {
    exc =>

    def nextTurn(): Future[Exec] = {
      if (executing_?) return Promise.failed[Exec](GameException("still waiting for all players to finish the previous turn")).future
      println("executing next turn")
      executing_? = true
      val f = exec()
      f onComplete {
        case _ => executing_? = false
      }
      f onFailure { case thr => throw thr }
      f map { _ =>
        onSuccess()
        exc
        //Await.result(
      }
    }
  }

  def onSuccess: () => Unit

  protected def exec() = Future.sequence[SideEffect[Env#Ref], Set](agents.map(ag => {
    Future {
      ag.lifetimeCycle(ag.env).andThen(_.asInstanceOf[Env#Ref]) }
  }))

  private val _agents = mutable.HashSet.empty[Ag]
  def register(agent: Ag*) { _agents ++= agent }
  protected def agents: Set[Ag] = _agents.toSet

  def pauseBetweenExecs: FiniteDuration = null
}

case class GameException(msg: String) extends Exception(msg)