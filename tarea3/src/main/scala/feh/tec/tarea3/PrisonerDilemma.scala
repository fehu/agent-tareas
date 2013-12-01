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
import scala.swing._
import feh.tec.util.RandomWrappers._
import feh.tec.agent.AgentDecision.ExplainedActionStub
import scala.util.{Random, Failure}
import scala.collection.mutable
import scala.collection.immutable.NumericRange
import scala.swing.GridBagPanel.{Anchor, Fill}
import java.awt.{Font, Color}
import feh.tec.agent.AgentDecision.ExplainedActionStub
import feh.tec.agent.StrategicChoice
import scala.xml.Text

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

  def gPlayer = player.asInstanceOf[game.PrisonerPlayer]

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

  var randomChance = 0.0
  var preference: Double = .5
  val preferred1 = gPlayer.Betray
  val preferred2 = gPlayer.Refuse

  def irrationalBehaviour: I[StrategicChoice[PrisonerDilemma#Player]] =
    if(Random.nextDouble() < randomChance) {
      val chosen = if(Random.nextDouble() < preference) preferred1 else preferred2
      _ => StrategicChoice(gPlayer, chosen)
    }
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
    var scoreMessageA = s"${playerA.player}"
    var scoreMessageB = s"${playerB.player}"
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
//    frame.minimumSize = 50 -> 50
//    frame.preferredSize = 300 -> 200
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

  class HistoryListEntry extends GridBagPanel{
    val turn = new Label()
    val playerA = new Label()
    val playerB = new Label()

    def set(a: (Int, (String, String))){
      val (turn, (playerA, playerB)) = a
      this.turn.text = turn.toString
      this.playerA.text = playerA
      this.playerB.text = playerB
    }

    turn.font = new Font(turn.font.getName, Font.BOLD, turn.font.getSize * 2)

    layout ++= Map(
      turn -> (1 -> 0 : Constraints).pipe{
        c =>
          c.anchor = Anchor.Center
          c.weightx = 0
          c
      },
      playerA -> (0 -> 0 : Constraints).pipe{
        c =>
          c.anchor = Anchor.West
          c.weightx = 0.5
          c
      },
      playerB -> (2 -> 0 : Constraints).pipe{
        c =>
          c.anchor = Anchor.East
          c.weightx = 0.5
          c
      }
    )
  }

  val historyListRenderer = new ListView.AbstractRenderer[(Int, (String, String)), HistoryListEntry](new HistoryListEntry) {
    def configure(list: ListView[_], isSelected: Boolean, focused: Boolean, a: (Int, (String, String)), index: Int){
      component.set(a)
    }
  }

  val scoreLabelA = monitorFor(Messages.scoreMessageA).text
  val scoreLabelB = monitorFor(Messages.scoreMessageB).text
  val historyList = monitorFor(Messages.history.toMap).list(Ordering.Int.reverse)
    .fillBoth.renderKeys(historyListRenderer)
  val turnButton = triggerFor(execTurn()).button("play a turn")
    .sizes(min = 50 -> 20, max = Int.MaxValue -> 100, preferred = 200 -> 30)
    .fillHorizontally

  def sliderControl(get: => Double, set: Double => Unit) =
    numericControlFor(get)(set)
      .slider(NumericRange.inclusive(.0, 1.0, 0.01)(Numeric.DoubleAsIfIntegral))
      .affect(_.orientation = Orientation.Vertical)

  def irrationalityControl(get: => Double, set: Double => Unit) = sliderControl(get, set).defaultLabels(0.1)

  val irrationalityA = irrationalityControl(playerA.randomChance, playerA.randomChance = _)
  val irrationalityB = irrationalityControl(playerB.randomChance, playerB.randomChance = _)

  def preferenceControl(get: => Double, set: Double => Unit) =
    sliderControl(get, set).labels(
      (for(i <- 0 to 10) yield i*10 -> label(i*10 + "/" + (100 - i*10))).toMap
    )

  val preferenceA = preferenceControl(playerA.preference, playerA.preference = _)
  val preferenceB = preferenceControl(playerB.preference, playerB.preference = _)

  val scorePanel = panel.box(_.Horizontal)(scoreLabelA -> "score-A", scoreLabelB -> "score-B")
    .layout(_.fill = Fill.Horizontal, _.weightx = 1)

  def referLabel = label(<html>random action<br/>preference<br/><br/><center>Betray/Refuse</center></html>)
  def randLabel = label(<html>random<br/>action<br/>chance</html>)

  val setupA = panel.gridBag(
    place(irrationalityA, "rand-A") in theCenter,
    place(preferenceA, "prefer-A") to theWest,
    place(randLabel, "-l-rand-A") to theSouth,
    place(referLabel, "-l-refer-A") to theNorthWest
  ).layout(_.gridheight = 2)

  val setupB = panel.gridBag(
    place(irrationalityB, "rand-B") in theCenter,
    place(preferenceB, "prefer-B") to theEast,
    place(randLabel, "-l-rand-B") to theSouth,
    place(referLabel, "-l-refer-B") to theNorthEast
  ).layout(_.gridheight = 2)

  val layout = List(
    place(turnButton, "turn") in theCenter,
    place(scorePanel/*.affect(_.xLayoutAlignment = Component)*/) on Top of "turn",
    place(scrollable()(historyList, "history")) on theBottom of "turn",
    place(setupA, "setup-A") to theLeft of "turn",
    place(setupB, "setup-B") to theRight of "turn"
//      place(irrationalityPanel/*, "rand-P"*/) to theRight of "turn"
  )

  frame.minimumSize = 740 -> 520
  frame.preferredSize = 740 -> 520
}

object PrisonerDilemmaExecutable extends App{
  val app = new PrisonerDilemmaApp
  app.start()
}