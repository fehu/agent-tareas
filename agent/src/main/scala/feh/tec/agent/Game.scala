package feh.tec.agent

import feh.tec.util._
import concurrent.{Promise, ExecutionContext, Future, Await}
import java.util.UUID
import akka.pattern._
import scala.concurrent.duration._
import akka.util.Timeout
import scala.collection.mutable
import akka.actor.{ActorSystem, Props, Actor, ActorRef}
import feh.tec.util.HasUUID.AsyncSendMsgHasUUIDWrapper
import akka.event.Logging
import feh.tec.agent.AgentDecision.ExplainedActionStub
import util.{Success, Failure}

trait GameEnvironment[Game <: AbstractGame, Env <: GameEnvironment[Game, Env]] extends Environment[Null, Null, GameScore[Game], GameAction, Env]{
  self : Env =>

  type Ref <: GameRef[Game, Env]
  final type Choice = StrategicChoice[Game#Player]
  final type Score = GameScore[Game]

  def game: Game
  def play(choices: StrategicChoices[Game#Player]): Score

  implicit def utilityIsNumeric = game.utilityIsNumeric.asInstanceOf[Numeric[Game#Utility]]

  def updateScores(scoresUpdate: Score)
  def setScore(score: Score)

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
  extends GameEnvironment[Game, Env] with Deterministic[Null, Null, GameScore[Game], GameAction, Env]
{
  self: Env =>

  //  Map[PlayersChoices, PlayersUtility]
//  type PlayersChoices = Map[Player, Player#Game]
//  type PlayersUtility = Map[Player, Utility]
  //
  final type Player = Game#Player

  private def strategies = game.layout.asInstanceOf[Map[Map[Player, Player#Strategy], Map[Player, Game#Utility]]]

  def play(choices: StrategicChoices[Game#Player]): Score = GameScore(strategies(choices.toMap))
}

trait MutableGameEnvironmentImpl[Game <: AbstractGame, Env <: MutableGameEnvironmentImpl[Game, Env]]
  extends GameEnvironment[Game, Env] with MutableEnvironment[Null, Null, GameScore[Game], GameAction, Env]
{
  self: Env =>

  def initGlobalState: GameScore[Game] = GameScore.zero(game)

  def affected(act: GameAction): SideEffect[Env] = affected(act.asInstanceOf[StrategicChoices[Game#Player]])
  def affected(act: StrategicChoices[Game#Player]): SideEffect[Env] = SideEffect{
    val  score = play(act)
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

case class Turn(id: Int){
  def next: Turn = copy(id+1)
}

object Turn{
  def first = Turn(0)
}

trait GameRef[Game <: AbstractGame, Env <: GameEnvironment[Game, Env]] extends EnvironmentRef[Null, Null, GameScore[Game], GameAction, Env]
{
  def turn: Turn
  //  def asyncTurn: Future[Turn]
  def choose(choice: StrategicChoice[Game#Player])
  def awaitEndOfTurn()
  def strategies: Game
  def lastChoices: Option[Game#PlayersChoices]

//  def blocking: BlockingApi = ???
//  def async: AsyncApi = ???
}

trait GameCoordinator[Game <: AbstractGame, Env <: GameEnvironment[Game, Env]]
  extends EnvironmentOverseer[Null, Null, GameScore[Game], GameAction, Env]
{

  def currentTurn: Turn
  def registerChoice(choice: StrategicChoice[Game#Player])
//  protected def allChoicesRegistered(): SideEffect[Env]
  def awaitEndOfTurn()
  def reset()

  // no snapshots
  def snapshot: EnvironmentSnapshot[Null, Null, GameScore[Game], GameAction, Env] = ???
}

trait GameCoordinatorWithActor[Game <: AbstractGame, Env <: GameEnvironment[Game, Env]]
  extends GameCoordinator[Game, Env] with EnvironmentOverseerWithActor[Null, Null, GameScore[Game], GameAction, Env]
{
  coordinator =>

  import GameCoordinatorActor._

  def awaitEndOfTurnTimeout: FiniteDuration

  def currentTurn: Turn = Await.result(asyncCurrentTurn, defaultBlockingTimeout millis)

  def asyncCurrentTurn: Future[Turn] = GetTurn() |> {
    msg => actorRef.send(msg).awaitingResponse[TurnResponse](defaultFutureTimeout millis).map(_.turn)
    //(actorRef ? msg)(defaultFutureTimeout).mapTo[TurnResponse].havingSameUUID(msg).map(_.turn)
  }

  def registerChoice(choice: StrategicChoice[Game#Player]): Unit = actorRef ! RegisterChoice(choice)

  protected[agent] var _lastChoices: Game#PlayersChoices = null
  def lastChoices: Option[Game#PlayersChoices] = Option(_lastChoices)

//  protected def allChoicesRegistered(): SideEffect[G] = updateEnvironment(_.updateScores())

  def awaitEndOfTurn(): Unit = AwaitEndOfTurn() |> {
    msg => Await
      .result((actorRef ? msg)(awaitEndOfTurnTimeout), awaitEndOfTurnTimeout)
      .tryAs[TurnEnded].havingSameUUID(msg).ensuring(_.nonEmpty)
  }

  trait GameRefBaseImpl extends GameRef[Game, Env] with BaseEnvironmentRef{
    def turn: Turn = currentTurn
    def choose(choice: StrategicChoice[Game#Player]): Unit = registerChoice(choice)
    def awaitEndOfTurn(): Unit = coordinator.awaitEndOfTurn()
    def strategies: Game = coordinator.env.game
    def lastChoices: Option[Game#PlayersChoices] = coordinator.lastChoices

//    override lazy val blocking: BlockingApi = ???
//    override lazy val async: AsyncApi = ???
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

  def reset(): Unit = actorRef ! Reset
}

object GameCoordinatorActor{
  case class GetTurn() extends UUIDed
  case class TurnResponse(uuid: UUID, turn: Turn) extends HasUUID
  case class RegisterChoice[Game <: AbstractGame](choice: StrategicChoice[Game#Player]) extends UUIDed
  case class AwaitEndOfTurn() extends UUIDed
  case class TurnEnded(uuid: UUID) extends HasUUID
  case object Reset
}

class GameCoordinatorActor[Game <: AbstractGame, Env <: GameEnvironment[Game, Env]](coordinator: GameCoordinatorWithActor[Game, Env]) extends Actor{
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
    coordinator.affect(StrategicChoices(currentTurnChoices)).flatExec
    coordinator.lastScore
  } 
  protected def notifyAwaiting() = awaitingEndOfTurn.foreach{ case (waiting, id) => waiting ! TurnEnded(id) }

  protected val history = mutable.Map.empty[Turn, (Game#PlayersChoices, Game#PlayersUtility)]
  protected def guardHistory(score: GameScore[Game]){
    val choices = currentTurnChoicesMap.toMap.asInstanceOf[Game#PlayersChoices]
    val utility = score.utility.asInstanceOf[Game#PlayersUtility]
    coordinator._lastChoices = choices
    history += turn-> ( choices -> utility)
    lastHistory = (turn, choices, utility)
  }
  private var lastHistory: (Turn, Game#PlayersChoices, Game#PlayersUtility) = _

  protected def notifyEndOfTurnListeners() =
    coordinator.endOfTurnListeners.foreach(t => Future{ t.tupled(lastHistory) }(context.dispatcher))

  def receive: Actor.Receive = {
    case msg@GetTurn() => sender ! TurnResponse(msg.uuid, turn)
    case RegisterChoice(choice) =>
      newChoice(choice.asInstanceOf[StrategicChoice[Game#Player]])
      if(turnFinished_?) {
        val Some(score) = endTurn()
        notifyAwaiting()
        guardHistory(score)
        notifyEndOfTurnListeners()
        nextTurn()
      }
    case msg@AwaitEndOfTurn() => awaiting(sender, msg.uuid)
    case Reset =>
      turn = Turn.first
      currentTurnChoicesMap.clear()
      awaitingEndOfTurn.clear()
      history.clear()
      lastHistory = null
      coordinator.env.setScore(GameScore.zero(coordinator.env.game))
      notifyAwaiting()
      notifyEndOfTurnListeners()
  }
}


trait MutableGameCoordinator[Game <: AbstractGame, Env <: MutableGameEnvironmentImpl[Game, Env]]
  extends GameCoordinator[Game, Env]
  with MutableEnvironmentOverseer[Null, Null, GameScore[Game], GameAction, Env]
{


  def lastScore: Option[GameScore[Game]] = env.lastScore
}

trait AbstractGame{
  type Utility
  implicit def utilityIsNumeric: Numeric[Utility]

  lazy val playerNameRegex = """.*\$(\w+)\$(\w+)\$.*""".r

  trait Player{
    trait Strategy
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
  override val layout: Map[PlayersChoices, PlayersUtility]
}

trait AbstractTurnBasedGame extends AbstractGame{
  def playersTurnOrdering: Ordering[Player]
  def playersInTurnOrder = players.toList.sorted(playersTurnOrdering)
}

object PlayerAgent {
  type Exec[Game <: AbstractGame, Env <: GameEnvironment[Game, Env]] = SimultaneousAgentsExecutor[Null, Null, GameScore[Game], GameAction, Env]
}

trait PlayerAgent[Game <: AbstractGame, Env <: GameEnvironment[Game, Env]]
  extends Agent[Null, Null, GameScore[Game], GameAction, Env, PlayerAgent.Exec[Game, Env]]
  with SimultaneousAgentExecution[Null, Null, GameScore[Game], GameAction, Env, PlayerAgent.Exec[Game, Env]]
{
  agent: DecisiveAgent[Null, Null, GameScore[Game], GameAction, Env, PlayerAgent.Exec[Game, Env]] =>

  executionLoop.register(this)

  override def act(a: GameAction): SideEffect[Env#Ref] = SideEffect{
    env.choose(a.asInstanceOf[StrategicChoice[Game#Player]])
    env
  }.flatExec
}

trait DummyPlayer[Game <: AbstractGame, Env <: GameEnvironment[Game, Env]]
  extends PlayerAgent[Game, Env]
  with DummyAgent[Null, Null, GameScore[Game], GameAction, Env, PlayerAgent.Exec[Game, Env]]
{
  type ActionExplanation = ExplainedActionStub[GameAction]
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
      println("nextTurn")
      if (executing_?) return Promise.failed[Exec](GameException("still waiting for all players to finish the previous turn")).future
      println("nextTurn: executing")
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

  protected def exec() = Future.sequence(agents.map(ag => {
    Future {
      ag.lifetimeCycle(ag.env) }
  }))

  private val _agents = mutable.HashSet.empty[Ag]
  def register(agent: Ag*) { _agents ++= agent }
  protected def agents: Set[Ag] = _agents.toSet

  def pauseBetweenExecs: FiniteDuration = null
}

case class GameException(msg: String) extends Exception(msg)