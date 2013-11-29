package feh.tec.agent

import feh.tec.util._
import scala.concurrent.{Future, Await}
import java.util.UUID
import akka.pattern._
import scala.concurrent.duration._
import akka.util.Timeout
import scala.collection.mutable
import akka.actor.{ActorSystem, Props, Actor, ActorRef}
import feh.tec.util.HasUUID.AsyncSendMsgHasUUIDWrapper
import akka.event.Logging
import feh.tec.agent.AgentDecision.ExplainedActionStub

trait GameEnvironment[Strategy <: Game, G <: GameEnvironment[Strategy, G]] extends Environment[Null, Null, GameScore[Strategy], GameAction, G]{
  self : G =>

  type Ref <: GameRef[Strategy, G]
  final type Choice = StrategicChoice[Strategy#Player]
  final type Score = GameScore[Strategy]

  def strategicLayout: Strategy
  def play(choices: StrategicChoices[Strategy#Player]): Score

  implicit def utilityIsNumeric = strategicLayout.utilityIsNumeric.asInstanceOf[Numeric[Strategy#Utility]]

  def updateScores(scoresUpdate: Score)

  // those are not used
  def states: PartialFunction[Null, Null] = PartialFunction.empty
  def definedAt = Nil
  def stateOf(c: Null) = None
  def visibleStates = Map()
  def agentPosition(ag: AgentId) = None
  def effects: PartialFunction[GameAction, G => G] = PartialFunction.empty
}

trait GameAction extends AbstractAction
case class StrategicChoice[P <: Game#Player](player: P, strategy: P#Strategy) extends GameAction
case class StrategicChoices[P <: Game#Player](choices: Set[StrategicChoice[P]]) extends GameAction{
  def toMap: Map[P, P#Strategy] = choices.map(ch => ch.player -> ch.strategy).toMap
}
case class GameScore[Strategy <: Game](score: Map[Strategy#Player, Strategy#Utility])(implicit num: Numeric[Strategy#Utility]){
  def update(scoreUpdates: Map[Strategy#Player, Strategy#Utility]): GameScore[Strategy] =
    GameScore(score.zipByKey(scoreUpdates).mapValues((num.plus _).tupled))
  def update(scoreUpdate: GameScore[Strategy]): GameScore[Strategy] = update(scoreUpdate.score)
}
object GameScore{
  def zero[Strategy <: Game](strategy: Strategy) =
    GameScore[Strategy](strategy.players.map(_ -> strategy.utilityIsNumeric.zero).toMap)(strategy.utilityIsNumeric.asInstanceOf[Numeric[Strategy#Utility]])
}

trait DeterministicGameEnvironment[Strategy <: DeterministicGame, G <: DeterministicGameEnvironment[Strategy, G]]
  extends GameEnvironment[Strategy, G] with Deterministic[Null, Null, GameScore[Strategy], GameAction, G]
{
  self: G =>

  //  Map[PlayersChoices, PlayersUtility]
//  type PlayersChoices = Map[Player, Player#Strategy]
//  type PlayersUtility = Map[Player, Utility]
  //
  final type Player = Strategy#Player

  private def strategies = strategicLayout.layout.asInstanceOf[Map[Map[Player, Player#Strategy], Map[Player, Strategy#Utility]]]

  def play(choices: StrategicChoices[Strategy#Player]): Score = GameScore(strategies(choices.toMap))
}

trait MutableGameEnvironmentImpl[Strategy <: Game, G <: MutableGameEnvironmentImpl[Strategy, G]]
  extends GameEnvironment[Strategy, G]
  with MutableEnvironment[Null, Null, GameScore[Strategy], GameAction, G]
{
  self: G =>

  def initGlobalState: GameScore[Strategy] = GameScore.zero(strategicLayout)

  def affected(act: GameAction): SideEffect[G] = affected(act.asInstanceOf[StrategicChoices[Strategy#Player]])
  def affected(act: StrategicChoices[Strategy#Player]): SideEffect[G] = SideEffect{
    updateScores(play(act))
    this
  }

  def updateScores(scoresUpdate: Score){
    globalState = globalState.update(scoresUpdate)
  }

  def initStates: PartialFunction[Null, Null] = null
  override def states = super[GameEnvironment].states
}

case class Turn(id: Long){
  def next: Turn = copy(id+1)
}

object Turn{
  def first = Turn(0)
}

trait GameRef[Strategy <: Game, G <: GameEnvironment[Strategy, G]] extends EnvironmentRef[Null, Null, GameScore[Strategy], GameAction, G]
{
  def turn: Turn
  //  def asyncTurn: Future[Turn]
  def choose(choice: StrategicChoice[Strategy#Player])
  def awaitEndOfTurn()
  def strategies: Strategy

  def blocking: BlockingApi = ???
  def async: AsyncApi = ???
}

trait GameCoordinator[Strategy <: Game, G <: GameEnvironment[Strategy, G]]
  extends EnvironmentOverseer[Null, Null, GameScore[Strategy], GameAction, G]
{

  def currentTurn: Turn
  def registerChoice(choice: StrategicChoice[Strategy#Player])
//  protected def allChoicesRegistered(): SideEffect[G]
  def awaitEndOfTurn()

  // no snapshots
  def snapshot: EnvironmentSnapshot[Null, Null, GameScore[Strategy], GameAction, G] = ???
}

trait GameCoordinatorWithActor[Strategy <: Game, G <: GameEnvironment[Strategy, G]]
  extends GameCoordinator[Strategy, G] with EnvironmentOverseerWithActor[Null, Null, GameScore[Strategy], GameAction, G]
{
  coordinator =>

  case class GetTurn() extends UUIDed
  case class TurnResponse(uuid: UUID, turn: Turn) extends HasUUID
  case class RegisterChoice(choice: StrategicChoice[Strategy#Player]) extends UUIDed
  case class AwaitEndOfTurn() extends UUIDed
  case class TurnEnded(uuid: UUID) extends HasUUID

  def awaitEndOfTurnTimeout: FiniteDuration

  def currentTurn: Turn = Await.result(asyncCurrentTurn, defaultBlockingTimeout millis)

  def asyncCurrentTurn: Future[Turn] = GetTurn() |> {
    msg => actorRef.send(msg).awaitingResponse[TurnResponse](defaultFutureTimeout millis).map(_.turn)
    //(actorRef ? msg)(defaultFutureTimeout).mapTo[TurnResponse].havingSameUUID(msg).map(_.turn)
  }

  def registerChoice(choice: StrategicChoice[Strategy#Player]): Unit = actorRef ! RegisterChoice(choice)

//  protected def allChoicesRegistered(): SideEffect[G] = updateEnvironment(_.updateScores())

  def awaitEndOfTurn(): Unit = AwaitEndOfTurn() |> {
    msg => Await
      .result((actorRef ? msg)(awaitEndOfTurnTimeout), awaitEndOfTurnTimeout)
      .tryAs[TurnEnded].havingSameUUID(msg).ensuring(_.nonEmpty)
  }

  trait GameRefBaseImpl extends GameRef[Strategy, G] with BaseEnvironmentRef{
    def turn: Turn = currentTurn
    def choose(choice: StrategicChoice[Strategy#Player]): Unit = registerChoice(choice)
    def awaitEndOfTurn(): Unit = coordinator.awaitEndOfTurn()
    def strategies: Strategy = coordinator.env.strategicLayout

    override lazy val blocking: BlockingApi = ???
    override lazy val async: AsyncApi = ???
  }

  protected class GameCoordinatorActor extends Actor{
    val log = Logging(context.system, this)

    private var turn = Turn.first
    private val currentTurnChoicesMap = mutable.HashMap.empty[Strategy#Player, Strategy#Player#Strategy]
    private val awaitingEndOfTurn = mutable.HashMap.empty[ActorRef, UUID]

    protected def currentTurnChoices = currentTurnChoicesMap.map((StrategicChoice.apply[Strategy#Player] _).tupled).toSet
    protected def newChoice(choice: StrategicChoice[Strategy#Player]) = {
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

    protected def turnFinished_? = currentTurnChoicesMap.keySet == coordinator.env.strategicLayout.players

    protected def endTurn() = coordinator.affect(StrategicChoices(currentTurnChoices)) // [Strategy#Player]
    protected def notifyAwaiting() = awaitingEndOfTurn.foreach{
        case (waiting, id) => waiting ! TurnEnded(id)
      }

    def receive: Actor.Receive = {
      case msg@GetTurn() => sender ! TurnResponse(msg.uuid, turn)
      case RegisterChoice(choice) =>
        newChoice(choice)
        if(turnFinished_?) {
          endTurn()
          notifyAwaiting()
          nextTurn()
        }
      case msg@AwaitEndOfTurn() => awaiting(sender, msg.uuid)
    }
  }

  protected def actorProps = Props(classOf[GameCoordinatorActor])

  def actorSystem: ActorSystem

  def actorRef: ActorRef = actorSystem.actorOf(actorProps)
}

trait MutableGameCoordinator[Strategy <: Game, G <: MutableGameEnvironmentImpl[Strategy, G]]
  extends GameCoordinator[Strategy, G]
  with MutableEnvironmentOverseer[Null, Null, GameScore[Strategy], GameAction, G]
{

}

trait Game{
  type Utility
  implicit def utilityIsNumeric: Numeric[Utility]

  trait Player{
    trait Strategy
    def availableStrategies: Set[Strategy] 
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

trait DeterministicGame extends Game{
  override val layout: Map[PlayersChoices, PlayersUtility]
}

trait TurnBasedGame extends Game{
  def playersTurnOrdering: Ordering[Player]
  def playersInTurnOrder = players.toList.sorted(playersTurnOrdering)
}

object PlayerAgent {
  type Exec[Strategy <: Game, G <: GameEnvironment[Strategy, G]] = SimultaneousAgentsExecutor[Null, Null, GameScore[Strategy], GameAction, G]
}

trait PlayerAgent[Strategy <: Game, G <: GameEnvironment[Strategy, G]]
  extends Agent[Null, Null, GameScore[Strategy], GameAction, G, PlayerAgent.Exec[Strategy, G]]
  with SimultaneousAgentExecution[Null, Null, GameScore[Strategy], GameAction, G, PlayerAgent.Exec[Strategy, G]]
{
  agent: DecisiveAgent[Null, Null, GameScore[Strategy], GameAction, G, PlayerAgent.Exec[Strategy, G]] =>
}

trait DummyPlayer[Strategy <: Game, G <: GameEnvironment[Strategy, G]]
  extends PlayerAgent[Strategy, G] 
  with DummyAgent[Null, Null, GameScore[Strategy], GameAction, G, PlayerAgent.Exec[Strategy, G]] 
{
  type ActionExplanation = ExplainedActionStub[GameAction]
  type DetailedPerception = AbstractDetailedPerception
  type Perception = Strategy

  def player: Strategy#Player

  def sense(env: EnvRef): Perception = env.strategies

  def detailed(env: EnvRef, c: Null): Option[AbstractDetailedPerception] = None
}