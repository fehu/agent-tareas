package feh.tec.agent

import feh.tec.util.{HasUUID, UUIDed, MapZipperWrapper, SideEffect}
import scala.concurrent.Future
import java.util.UUID

trait Game[Strategy <: GameStrategicLayout, G <: Game[Strategy, G]] extends Environment[Null, Null, GameScore[Strategy], StrategicChoice[Strategy#Player], G]{
  self : G =>

  type Ref <: GameRef[Strategy, G]
  final type Choice = StrategicChoice[Strategy#Player]

  def strategicLayout: Strategy
  def choose(choice: Choice)
  def await()

  implicit def utilityIsNumeric = strategicLayout.utilityIsNumeric.asInstanceOf[Numeric[Strategy#Utility]]

  def affected(act: StrategicChoice[Strategy#Player]): SideEffect[G] = SideEffect{
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

trait GameRef[Strategy <: GameStrategicLayout, G <: Game[Strategy, G]] extends EnvironmentRef[Null, Null, GameScore[Strategy], StrategicChoice[Strategy#Player], G]
{
  def turn: Turn
  def asyncTurn: Future[Turn]
}

case class StrategicChoice[P <: GameStrategicLayout#Player](player: P, strategy: P#Strategy) extends AbstractAction
case class GameScore[Strategy <: GameStrategicLayout](score: Map[Strategy#Player, Strategy#Utility])(implicit num: Numeric[Strategy#Utility]){
  def update(scoreUpdates: Map[Strategy#Player, Strategy#Utility]) =
    GameScore(score.zipByKey(scoreUpdates).mapValues((num.plus _).tupled))
}
object GameScore{
  def zero[Strategy <: GameStrategicLayout](strategy: Strategy) =
    GameScore[Strategy](strategy.players.map(_ -> strategy.utilityIsNumeric.zero).toMap)(strategy.utilityIsNumeric.asInstanceOf[Numeric[Strategy#Utility]])
}

trait DeterministicGame[Strategy <: DeterministicGameStrategicLayout, G <: DeterministicGame[Strategy, G]]
  extends Game[Strategy, G] with Deterministic[Null, Null, GameScore[Strategy], StrategicChoice[Strategy#Player], G]
{
  self: G =>
  override def affected(act: StrategicChoice[Strategy#Player]): SideEffect[G] = super[Game].affected(act)
}


trait OverseenGameImpl[Strategy <: GameStrategicLayout, G <: MutableGameImpl[Strategy, G]] extends Game[Strategy, G]
{
  self: G =>

  def overseer: Option[GameOverseer[Strategy, G]]
  def connect(overseer: GameOverseer[Strategy, G]): G

  def choose(choice: Choice): Unit = overseer.foreach(_.registerChoice(choice))
  def await(): Unit = overseer.foreach(_.awaitEndOfTurn())
}

trait MutableGameImpl[Strategy <: GameStrategicLayout, G <: MutableGameImpl[Strategy, G]]
  extends Game[Strategy, G]
  with MutableEnvironment[Null, Null, GameScore[Strategy], StrategicChoice[Strategy#Player], G]
{
  self: G =>

  def initGlobalState: GameScore[Strategy] = GameScore.zero(strategicLayout)

  // for OverseenGameImpl: should be called by overseer in the end of the turn to update global state
  def updateScores(scoresUpdate: Map[Strategy#Player, Strategy#Utility]){
    globalState = globalState.update(scoresUpdate)
  }

  def initStates: PartialFunction[Null, Null] = null
  override def states = super[Game].states
//  override def globalState: GameScore[Strategy] = super[Game].globalState
}

trait Turn{
  def id: Long
  def next: Turn
}

trait GameOverseer[Strategy <: GameStrategicLayout, G <: Game[Strategy, G]]
  extends EnvironmentOverseer[Null, Null, GameScore[Strategy], StrategicChoice[Strategy#Player], G]
{
  self: G =>

  def currentTurn: Turn
  def registerChoice(choice: StrategicChoice[Strategy#Player])
  protected def allChoicesRegistered(): SideEffect[G]
  def awaitEndOfTurn()

}

trait GameOverseerWithActor[Strategy <: GameStrategicLayout, G <: Game[Strategy, G]]
  extends GameOverseer[Strategy, G] with EnvironmentOverseerWithActor[Null, Null, GameScore[Strategy], StrategicChoice[Strategy#Player], G]
{
  self: G =>

  /*case class GetTurn() extends UUIDed
  case class Turn(uuid: UUID, turn: Turn) extends HasUUID

  protected def gameOverseerResponses: PartialFunction[Any, () => Unit] = ???
  */

}

trait MutableGameOverseer[Strategy <: GameStrategicLayout, G <: MutableGameImpl[Strategy, G]]
  extends GameOverseer[Strategy, G]
  with MutableEnvironmentOverseer[Null, Null, GameScore[Strategy], StrategicChoice[Strategy#Player], G]
{
  self: G =>

}

trait GameStrategicLayout{
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
  def strategicLayout: PlayersChoices => PlayersUtility
}

trait DeterministicGameStrategicLayout extends GameStrategicLayout{
  override val strategicLayout: Map[PlayersChoices, PlayersUtility]
}

trait TurnBasedGameStrategicLayout extends GameStrategicLayout{
  def playersTurnOrdering: Ordering[Player]
  def playersInTurnOrder = players.toList.sorted(playersTurnOrdering)
}

object PlayerAgent {
  type Exec[Strategy <: GameStrategicLayout, G <: Game[Strategy, G]] = SimultaneousAgentsExecutor[Null, Null, GameScore[Strategy], StrategicChoice[Strategy#Player], G]
}

trait PlayerAgent[Strategy <: GameStrategicLayout, G <: Game[Strategy, G]]
  extends Agent[Null, Null, GameScore[Strategy], StrategicChoice[Strategy#Player], G, PlayerAgent.Exec[Strategy, G]]
  with SimultaneousAgentExecution[Null, Null, GameScore[Strategy], StrategicChoice[Strategy#Player], G, PlayerAgent.Exec[Strategy, G]]
{
  agent: DecisiveAgent[Null, Null, GameScore[Strategy], StrategicChoice[Strategy#Player], G, PlayerAgent.Exec[Strategy, G]] =>


}

