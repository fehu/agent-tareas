package feh.tec.agent

import feh.tec.util.{HasUUID, UUIDed, MapZipperWrapper, SideEffect}
import scala.concurrent.Future
import java.util.UUID

trait Game[Layout <: GameStrategicLayout, G <: Game[Layout, G]] extends Environment[Null, Null, GameScore[Layout], StrategicChoice[Layout#PlayerRef], G]{
  self : G =>

  type Ref <: GameRef[Layout, G]
  final type Choice = StrategicChoice[Layout#PlayerRef]

  def strategicLayout: Layout
  def choose(choice: Choice)
  def await()

  implicit def utilityIsNumeric = strategicLayout.utilityIsNumeric.asInstanceOf[Numeric[Layout#Utility]]

  def affected(act: StrategicChoice[Layout#PlayerRef]): SideEffect[G] = SideEffect{
    choose(act)
    await()
    self
  }

  def effects: PartialFunction[Choice, G => G] = PartialFunction.empty
/*
  {
    case choice => game =>
      game.choose(choice)
      game.await()
      this
  }
*/

  // those are not used
  def states: PartialFunction[Null, Null] = PartialFunction.empty
  def definedAt = Nil
  def stateOf(c: Null) = None
  def visibleStates = Map()
  def agentPosition(ag: AgentId) = None
}

trait GameRef[Layout <: GameStrategicLayout, G <: Game[Layout, G]] extends EnvironmentRef[Null, Null, GameScore[Layout], StrategicChoice[Layout#PlayerRef], G]
{
  def turn: Turn
  def asyncTurn: Future[Turn]
}

case class StrategicChoice[P <: GameStrategicLayout#PlayerRef](player: P, strategy: P#Strategy) extends AbstractAction
case class GameScore[Layout <: GameStrategicLayout](score: Map[Layout#PlayerRef, Layout#Utility])(implicit num: Numeric[Layout#Utility]){
  def update(scoreUpdates: Map[Layout#PlayerRef, Layout#Utility]) =
    GameScore(score.zipByKey(scoreUpdates).mapValues((num.plus _).tupled))
}
object GameScore{
  def zero[Layout <: GameStrategicLayout](layout: Layout) =
    GameScore[Layout](layout.players.map(_ -> layout.utilityIsNumeric.zero).toMap)(layout.utilityIsNumeric.asInstanceOf[Numeric[Layout#Utility]])
}

trait DeterministicGame[Layout <: DeterministicGameStrategicLayout, G <: DeterministicGame[Layout, G]]
  extends Game[Layout, G] with Deterministic[Null, Null, GameScore[Layout], StrategicChoice[Layout#PlayerRef], G]
{
  self: G =>
  override def affected(act: StrategicChoice[Layout#PlayerRef]): SideEffect[G] = super[Game].affected(act)
}


trait OverseenGameImpl[Layout <: GameStrategicLayout, G <: MutableGameImpl[Layout, G]] extends Game[Layout, G]
{
  self: G =>

  def overseer: Option[GameOverseer[Layout, G]]
  def connect(overseer: GameOverseer[Layout, G]): G

  def choose(choice: Choice): Unit = overseer.foreach(_.registerChoice(choice))
  def await(): Unit = overseer.foreach(_.awaitEndOfTurn())
}

trait MutableGameImpl[Layout <: GameStrategicLayout, G <: MutableGameImpl[Layout, G]]
  extends Game[Layout, G]
  with MutableEnvironment[Null, Null, GameScore[Layout], StrategicChoice[Layout#PlayerRef], G]
{
  self: G =>

  def initGlobalState: GameScore[Layout] = GameScore.zero(strategicLayout)

  // for OverseenGameImpl: should be called by overseer in the end of the turn to update global state
  def updateScores(scoresUpdate: Map[Layout#PlayerRef, Layout#Utility]){
    globalState = globalState.update(scoresUpdate)
  }

  def initStates: PartialFunction[Null, Null] = null
  override def states = super[Game].states
//  override def globalState: GameScore[Layout] = super[Game].globalState
}

trait Turn{
  def id: Long
  def next: Turn
}

trait GameOverseer[Layout <: GameStrategicLayout, G <: Game[Layout, G]]
  extends EnvironmentOverseer[Null, Null, GameScore[Layout], StrategicChoice[Layout#PlayerRef], G]
{
  self: G =>

  def currentTurn: Turn
  def registerChoice(choice: StrategicChoice[Layout#PlayerRef])
  protected def allChoicesRegistered(): SideEffect[G]
  def awaitEndOfTurn()

}

trait GameOverseerWithActor[Layout <: GameStrategicLayout, G <: Game[Layout, G]]
  extends GameOverseer[Layout, G] with EnvironmentOverseerWithActor[Null, Null, GameScore[Layout], StrategicChoice[Layout#PlayerRef], G]
{
  self: G =>

  case class GetTurn() extends UUIDed
  case class Turn(uuid: UUID, turn: Turn) extends HasUUID

  protected def gameOverseerResponses: PartialFunction[Any, () => Unit] = ???
}

trait MutableGameOverseer[Layout <: GameStrategicLayout, G <: MutableGameImpl[Layout, G]]
  extends GameOverseer[Layout, G]
  with MutableEnvironmentOverseer[Null, Null, GameScore[Layout], StrategicChoice[Layout#PlayerRef], G]
{
  self: G =>

}

trait GameStrategicLayout{
  type Utility
  implicit def utilityIsNumeric: Numeric[Utility]

  trait PlayerRef{
    trait Strategy
    def availableStrategies: Set[Strategy] 
  }

  type PlayersChoices = Map[PlayerRef, PlayerRef#Strategy]
  type PlayersUtility = Map[PlayerRef, Utility]

//  case class PlayerChoice[P <: PlayerRef](player: P, strategy: P#Strategy)
//  case class PlayerUtility[P <: PlayerRef](player: P, utility: Utility)
  sealed trait Target
  case object Max extends Target
  case object Min extends Target

  def target: Target

  def nPlayers: Int
  def players: Set[PlayerRef]
  def strategicLayout: PlayersChoices => PlayersUtility
}

trait DeterministicGameStrategicLayout extends GameStrategicLayout{
  override val strategicLayout: Map[PlayersChoices, PlayersUtility]
}

trait TurnBasedGameStrategicLayout extends GameStrategicLayout{
  def playersTurnOrdering: Ordering[PlayerRef]
  def playersInTurnOrder = players.toList.sorted(playersTurnOrdering)
}

object Player {
  type Exec[Layout <: GameStrategicLayout, G <: Game[Layout, G]] = SimultaneousAgentsExecutor[Null, Null, GameScore[Layout], StrategicChoice[Layout#PlayerRef], G]
}

trait Player[Layout <: GameStrategicLayout, G <: Game[Layout, G]]
  extends Agent[Null, Null, GameScore[Layout], StrategicChoice[Layout#PlayerRef], G, Player.Exec[Layout, G]]
  with SimultaneousAgentExecution[Null, Null, GameScore[Layout], StrategicChoice[Layout#PlayerRef], G, Player.Exec[Layout, G]]
{
  agent: DecisiveAgent[Null, Null, GameScore[Layout], StrategicChoice[Layout#PlayerRef], G, Player.Exec[Layout, G]] =>


}

