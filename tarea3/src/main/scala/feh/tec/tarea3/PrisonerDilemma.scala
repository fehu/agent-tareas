package feh.tec.tarea3

import feh.tec.agent._
import Ordered._
import feh.tec.util.SideEffect
import feh.tec.agent.StrategicChoice
import scala.math.Numeric.IntIsIntegral
import akka.actor.{ActorSystem, Props, ActorRef, Scheduler}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import feh.tec.visual.{SwingFrameAppCreation, SwingAppFrame}
import feh.tec.visual.api.{AppBasicControlApi, AgentApp}
import java.awt.Component

class PrisonerDilemma extends AbstractDeterministicGame{
  type Utility = Int
  implicit def utilityIsNumeric = IntIsIntegral

  def nPlayers = 2

  trait PrisonerPlayer extends Player{
    case object Betray extends Strategy
    case object Refuse extends Strategy

    def availableStrategies = Set(Betray, Refuse)
  }

  object Players{
    object A extends PrisonerPlayer
    object B extends PrisonerPlayer
  }
  import Players._

  def players = Set(A, B)

  def strategicLayoutBuilder2[P1 <: Player, P2 <: Player](p1: P1, p2: P2)
                                                         (strategy1: P1 => P1#Strategy, strategy2: P2 => P2#Strategy)
                                                         (utility1: Utility, utility2: Utility): (PlayersChoices, PlayersUtility) =
    Map(p1 -> strategy1(p1), p2 -> strategy2(p2)) -> Map(p1 -> utility1, p2 -> utility2)

  def strategy = strategicLayoutBuilder2(A, B) _

  val layout: Map[PlayersChoices, PlayersUtility] = Map(
    strategy(_.Betray, _.Betray)(8, 8),
    strategy(_.Betray, _.Refuse)(0, 12),
    strategy(_.Refuse, _.Betray)(12, 0),
    strategy(_.Refuse, _.Refuse)(6, 6)
  )

  def target = Min
}

class PrisonerDilemmaGameEnvironment(val game: PrisonerDilemma = new PrisonerDilemma)
  extends DeterministicGameEnvironment[PrisonerDilemma, PrisonerDilemmaGameEnvironment]
  with MutableGameEnvironmentImpl[PrisonerDilemma, PrisonerDilemmaGameEnvironment]
{
  type Ref = GameRef[PrisonerDilemma, PrisonerDilemmaGameEnvironment]

  override def affected(act: GameAction): SideEffect[PrisonerDilemmaGameEnvironment] = super[MutableGameEnvironmentImpl].affected(act)
}

class PrisonerDilemmaGameCoordinator(environment: PrisonerDilemmaGameEnvironment,
                                     val actorSystem: ActorSystem,
                                     val awaitEndOfTurnTimeout: FiniteDuration,
                                     val defaultFutureTimeout: Int,
                                     val defaultBlockingTimeout: Int)
                                    (implicit val executionContext: ExecutionContext = actorSystem.dispatcher)
  extends MutableGameCoordinator[PrisonerDilemma, PrisonerDilemmaGameEnvironment]
  with GameCoordinatorWithActor[PrisonerDilemma, PrisonerDilemmaGameEnvironment]
{
  val scheduler: Scheduler = actorSystem.scheduler
  def ref: PrisonerDilemmaGameEnvironment#Ref = new GameRefBaseImpl{}
  override val currentEnvironment: PrisonerDilemmaGameEnvironment = environment
}

class PrisonerPlayer(val executionLoop: PlayerAgent.Exec[PrisonerDilemma, PrisonerDilemmaGameEnvironment],
                     val env: PrisonerPlayer#EnvRef,
                     val player: PrisonerDilemma#Player)
  extends DummyPlayer[PrisonerDilemma, PrisonerDilemmaGameEnvironment]
{
  def notifyDecision(a: ActionExplanation) {}
  def lastDecision: Option[ActionExplanation] = None

  def decide(currentPerception: Perception): ActionExplanation = ???
}

class PrisonersExec(val execControlTimeout: FiniteDuration,
                    val onSuccess: () => Unit)
                   (implicit val executionContext: ExecutionContext) extends ByTurnExec[PrisonerDilemma, PrisonerDilemmaGameEnvironment]{
  type Ag = PrisonerPlayer
}

class PrisonerDilemmaApp(implicit val actorSystem: ActorSystem = ActorSystem.create()) extends SwingAppFrame
  with SwingFrameAppCreation.Layout9PositionsDSL with SwingFrameAppCreation.Frame9PositionsLayoutBuilderImpl
  with SwingFrameAppCreation.LayoutDSLDefaultImpl
{
  frame =>

  implicit def executionContext = actorSystem.dispatcher

  val game = new PrisonerDilemma
  val env = new PrisonerDilemmaGameEnvironment(game)
  val coordinator = new PrisonerDilemmaGameCoordinator(env, actorSystem,
    awaitEndOfTurnTimeout = 50 millis,
    defaultFutureTimeout = 10,
    defaultBlockingTimeout = 10
    )

  var msg = ""

  def createMsg() = ""

  val gameExec = new PrisonersExec(50 millis, () => msg = createMsg)

  def player(sel: (game.Players.type => game.Player)*) = sel.map(s => new PrisonerPlayer(gameExec, coordinator.ref, s(game.Players)))

  val players = player(_.A, _.B)
  val Seq(playerA, playerB) = players

  def start(): Unit = {
    buildLayout()
    frame.open()
//    app.start()
  }
  def stop(): Unit = {
//    app.stop()
    frame.close()
  }

  val layout/*: List[SwingFrameAppCreation.AbstractLayoutSetting]*/ = ???

}
