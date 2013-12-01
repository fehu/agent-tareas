package feh.tec.tarea3

import feh.tec.agent._
import Ordered._
import feh.tec.util.{PipeWrapper, I, ConditionalChainingWrapper, SideEffect}
import feh.tec.agent.StrategicChoice
import scala.math.Numeric.IntIsIntegral
import akka.actor.{ActorSystem, Props, ActorRef, Scheduler}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import feh.tec.visual.{SwingFrameAppCreation, SwingAppFrame}
import feh.tec.visual.api.{AppBasicControlApi, AgentApp}
import scala.swing.{Orientation, Component, Swing, MainFrame}
import feh.tec.util.RandomWrappers._
import feh.tec.agent.AgentDecision.ExplainedActionStub
import scala.util.{Random, Failure}
import scala.collection.mutable
import scala.collection.immutable.NumericRange
import scala.swing.GridBagPanel.Fill

class PrisonerDilemma extends AbstractDeterministicGame{
  type Utility = Int
  implicit def utilityIsNumeric = IntIsIntegral

  def nPlayers = 2

  trait PrisonerPlayer extends Player{
    case object Betray extends Strategy
    case object Refuse extends Strategy

    def availableStrategies = Set(Betray, Refuse)
  }

  object Prisoner{
    object A extends PrisonerPlayer
    object B extends PrisonerPlayer
  }
  import Prisoner._

  def players = Set(A, B)

  protected def strategicLayoutBuilder2[P1 <: Player, P2 <: Player](p1: P1, p2: P2)
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
                     val player: PrisonerDilemma#Player,
                     decisionTaken: PrisonerPlayer#ActionExplanation => Unit = _ => {})
  extends DummyPlayer[PrisonerDilemma, PrisonerDilemmaGameEnvironment]
{
  def notifyDecision(a: ActionExplanation) { decisionTaken(a) }
  def lastDecision: Option[ActionExplanation] = None

  lazy val game: PrisonerDilemma = sense(env)

  lazy val accomplice = (game.players - player.asInstanceOf[game.Player]).head

  protected lazy val accompliceChoicesStats = mutable.HashMap(accomplice.availableStrategies.toSeq.map(_ -> .0): _*)

  def accompliceChoiceProb: Map[game.Player#Strategy, Double] ={
    val n = accompliceChoicesStats.map(_._2).sum
    if(n == 0) Map()
    else accompliceChoicesStats.mapValues(_ / n).toMap
  }

  def gPlayer = player.asInstanceOf[game.Player]

//  def utilityProbabilities:

  def probableUtility: Map[PrisonerDilemma#Player#Strategy, Double] ={
    val prob = accompliceChoiceProb.`case`(_.isEmpty)(_ => accomplice.availableStrategies.map(_ -> 1.0).toMap)
    gPlayer.availableStrategies.asInstanceOf[Set[game.Player#Strategy]].map{
      myStrategy => myStrategy ->
        (.0 /: prob){
          case (acc, (opStrategy, opStrategyProb)) =>
            val utility = game.layout(Map(gPlayer -> myStrategy, accomplice -> opStrategy))(gPlayer)
            acc + (utility * opStrategyProb)
        }
    }.toMap
  }

  var irrationalProbability: Double = 0

  def irrationalBehaviour: I[StrategicChoice[PrisonerDilemma#Player]] =
    if(Random.nextDouble() < irrationalProbability) _ => StrategicChoice(gPlayer, gPlayer.availableStrategies.toSeq.randomChoose)
    else identity

  def decide(perception: Perception): ActionExplanation = ExplainedActionStub{
    val expectedUtil = probableUtility
    println("expectedUtil = " + expectedUtil)

    StrategicChoice(
      gPlayer,
      (game.target match{
        //        case game.Max => probableUtility.maxBy(_._2) todo: use thi in generic case
        case game.Min if expectedUtil.isEmpty => expectedUtil.minBy(_._2)
        case game.Min => expectedUtil.minBy(_._2)
      })._1
    ) |> irrationalBehaviour

  }
}

class PrisonersExec(val execControlTimeout: FiniteDuration,
                    val onSuccess: () => Unit)
                   (implicit val executionContext: ExecutionContext) extends ByTurnExec[PrisonerDilemma, PrisonerDilemmaGameEnvironment]

class PrisonerDilemmaApp(implicit val actorSystem: ActorSystem = ActorSystem.create())
  extends MainFrame with SwingAppFrame with SwingFrameAppCreation.Frame9PositionsLayoutBuilder
{
  frame =>

  implicit def executionContext = actorSystem.dispatcher

  val game = new PrisonerDilemma
  val env = new PrisonerDilemmaGameEnvironment(game)
  val coordinator = new PrisonerDilemmaGameCoordinator(env, actorSystem,
    awaitEndOfTurnTimeout = 100 millis,
    defaultFutureTimeout = 100,
    defaultBlockingTimeout = 100
    )

  coordinator.listenToEndOfTurn{
    (turn, choices, utility) =>
      println("turn = " + turn)
      println("choices = " + choices)
      println("utility = " + utility)
      Messages.addHistory(turn, choices.asInstanceOf[game.PlayersChoices], utility.asInstanceOf[game.PlayersUtility])
      updateMsgs()
      println("Messages.scoreMessageA = " +  Messages.scoreMessageA)
      println("Messages.scoreMessageB = " +  Messages.scoreMessageB)
      updateForms()
  }

  object Messages{
    var scoreMessageA = ""
    var scoreMessageB = ""
    val _history = mutable.HashMap.empty[Int, (String, String)]
    def history = _history.toMap

    def addHistory(turn: Turn, choices: game.PlayersChoices, utility: game.PlayersUtility) ={
      assert(!_history.contains(turn.id))

      def buildHistoryEntry(player: game.Prisoner.type => game.PrisonerPlayer) = {
        val pl = player(game.Prisoner)
        val (ch, ut) = choices(pl) -> utility(pl)
        s"$ch: $ut"
      }

      _history += turn.id -> (buildHistoryEntry(_.A) -> buildHistoryEntry(_.B))
    }
  }


  def updateMsgs(): Unit = coordinator.ref.blocking.globalState.utility.map{
    case (p@game.Prisoner.A, u) => Messages.scoreMessageA =  s"$p: $u"
    case (p@game.Prisoner.B, u) => Messages.scoreMessageB =  s"$p: $u"
  }

  val gameExec = new PrisonersExec(100 millis, () => {})

  def execTurn() = gameExec.execution.nextTurn()

  def player(sel: (game.Prisoner.type => game.Player)*) = sel.map(s => new PrisonerPlayer(gameExec, coordinator.ref, s(game.Prisoner)))

  val players = player(_.A, _.B)
  val Seq(playerA, playerB) = players

  def start(): Unit = {
    buildLayout()
    frame.minimumSize = 50 -> 50
    frame.preferredSize = 300 -> 200
    frame.pack()
    frame.open()
    updateForms()
    println()
//    app.start()
  }
  def stop(): Unit = {
//    app.stop()
    frame.close()
  }

  val scoreLabelA = monitorFor(Messages.scoreMessageA).text
  val scoreLabelB = monitorFor(Messages.scoreMessageB).text
  val historyList = monitorFor(Messages.history.toMap).list
  val turnButton = triggerFor(execTurn()).button("play a turn")
    .sizes(min = 50 -> 20, max = Int.MaxValue -> 100, preferred = 200 -> 30)
    .fillHorizontally
  val irrationalityA = numericControlFor(playerA.irrationalProbability)(playerA.irrationalProbability = _)
    .slider(NumericRange.inclusive(.0, 1.0, 0.01)(Numeric.DoubleAsIfIntegral)).affect(_.orientation = Orientation.Vertical)

  val scorePanel = panel.box(_.Horizontal)(scoreLabelA -> "score-A", scoreLabelB -> "score-B").layout(_.fill = Fill.Horizontal)
//  val scorePanel = panel.box(_.Horizontal)().glue.elem(scoreLabelA.toComponent -> "score-A")

//  val historyPanel = List(
//    Scrollable(),
//    place(historyList, "history") in theCenter
//  )


  val layout = List(
    place(boxed(turnButton, "turn")) in theCenter,
    place(scorePanel/*.affect(_.xLayoutAlignment = Component)*/) on Top of "turn",
    place(scrollable()(historyList, "history")) on theBottom of "turn",
    place(irrationalityA, "rand-A") to theRight of "turn"
  )

}

object PrisonerDilemmaExecutable extends App{
  val app = new PrisonerDilemmaApp
  app.start()
}