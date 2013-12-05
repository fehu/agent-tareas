 package feh.tec.tarea3

import feh.tec.agent._
import feh.tec.util._
import scala.math.Numeric.IntIsIntegral
import akka.actor.{ActorSystem, Scheduler}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.Random
import scala.collection.mutable
import feh.tec.agent.AgentDecision.ExplainedActionStub
import feh.tec.agent.StrategicChoice

class PrisonerDilemma extends AbstractDeterministicGame{
  type Utility = Int
  implicit def utilityIsNumeric = IntIsIntegral

  def nPlayers = 2

  abstract class PrisonerPlayer(val name: String) extends Player{
    trait Choice
    final type Strategy = Choice
    case object Betray extends Strategy
    case object Refuse extends Strategy

    def availableStrategies = Set(Betray, Refuse)
  }

  object Prisoner{
    object A extends PrisonerPlayer("A")
    object B extends PrisonerPlayer("B")
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
                     val player: PrisonerDilemma#PrisonerPlayer,
                     decisionTaken: PrisonerPlayer#ActionExplanation => Unit = _ => {})
  extends DummyPlayer[PrisonerDilemma, PrisonerDilemmaGameEnvironment]
{
  def notifyDecision(a: ActionExplanation) { decisionTaken(a) }
  def lastDecision: Option[ActionExplanation] = None

  lazy val game: PrisonerDilemma = sense(env)

  lazy val accomplice = (game.players - player.asInstanceOf[game.Player]).head

  protected def buildAccompliceChoicesStats = accomplice.availableStrategies.toSeq.map(_ -> .0)
  protected lazy val accompliceChoicesStats = mutable.HashMap(buildAccompliceChoicesStats: _*)

  def guardAccompliceChoice(strategy: accomplice.Strategy) =
    accompliceChoicesStats(strategy) = accompliceChoicesStats(strategy) + 1

  def accompliceChoiceProb: Map[game.Player#Strategy, Double] ={
    val n = accompliceChoicesStats.map(_._2).sum
    if(n == 0) Map()
    else accompliceChoicesStats.mapValues(_ / n).toMap
  }

  def gPlayer = player.asInstanceOf[game.PrisonerPlayer]

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

  protected def defaultRandomChoice = 0d
  protected def defaultPreference = .5

  var randomChance = defaultRandomChoice
  var preference: Double = defaultPreference
  val preferred1 = player.Betray.asInstanceOf[player.Strategy]
  val preferred2 = player.Refuse.asInstanceOf[player.Strategy]

  def irrationalBehaviour: I[StrategicChoice[PrisonerDilemma#Player]] =
    if(Random.nextDouble() < randomChance) {
      val chosen = if(Random.nextDouble() < preference) preferred1 else preferred2
      _ => StrategicChoice(gPlayer, chosen)
    }
    else identity

  def decide(perception: Perception): ActionExplanation = ExplainedActionStub{
    env.lastChoices.map(ch =>
      guardAccompliceChoice(ch.asInstanceOf[game.PlayersChoices](accomplice).asInstanceOf[accomplice.Strategy])
    )
    val expectedUtil = probableUtility
    println(s"expectedUtil $player = " + expectedUtil)

    StrategicChoice(
      gPlayer,
      (game.target match{
        //        case game.Max => probableUtility.maxBy(_._2) todo: use thi in generic case
        case game.Min if expectedUtil.isEmpty => expectedUtil.minBy(_._2)
        case game.Min => expectedUtil.minBy(_._2)
      })._1
    ) |> irrationalBehaviour

  }

  def reset(){
    randomChance = defaultRandomChoice
    preference = defaultPreference
    accompliceChoicesStats.clear()
    accompliceChoicesStats ++= buildAccompliceChoicesStats
  }
}

class PrisonersExec(val execControlTimeout: FiniteDuration,
                    val onSuccess: () => Unit)
                   (implicit val executionContext: ExecutionContext) extends ByTurnExec[PrisonerDilemma, PrisonerDilemmaGameEnvironment]

//class PrisonerDilemmaApp(implicit val actorSystem: ActorSystem = ActorSystem.create())
//  extends PrisonerDilemmaSwingFrame
//{
//  frame =>

//  implicit def executionContext = actorSystem.dispatcher

//  val game = new PrisonerDilemma
//  val env = new PrisonerDilemmaGameEnvironment(game)
//  val coordinator = new PrisonerDilemmaGameCoordinator(env, actorSystem,
//    awaitEndOfTurnTimeout = 100 millis,
//    defaultFutureTimeout = 100,
//    defaultBlockingTimeout = 100
//    )
//
//  coordinator.listenToEndOfTurn{
//    (turn, choices, utility) =>
//      println("turn = " + turn)
//      println("choices = " + choices)
//      println("utility = " + utility)
//      Messages.addHistory(turn, choices.asInstanceOf[game.PlayersChoices], utility.asInstanceOf[game.PlayersUtility])
//      updateMsgs()
//      println("Messages.scoreMessageA = " +  Messages.scoreMessageA)
//      println("Messages.scoreMessageB = " +  Messages.scoreMessageB)
//      updateForms()
//  }
//
//  object Messages{
//    var scoreMessageA = "0"
//    var scoreMessageB = "0"
//    val _history = mutable.HashMap.empty[Int, (String, String)]
//    def history = _history.toMap
//
//    def addHistory(turn: Turn, choices: game.PlayersChoices, utility: game.PlayersUtility) ={
//      assert(!_history.contains(turn.id))
//
//      def buildHistoryEntry(player: game.Prisoner.type => game.PrisonerPlayer) = {
//        val pl = player(game.Prisoner)
//        val (ch, ut) = choices(pl) -> utility(pl)
//        s"$ch: $ut"
//      }
//
//      _history += turn.id -> (buildHistoryEntry(_.A) -> buildHistoryEntry(_.B))
//    }
//  }
//
//
//  def updateMsgs(): Unit = coordinator.ref.blocking.globalState.utility.map{
//    case (p@game.Prisoner.A, u) => Messages.scoreMessageA =  u.toString
//    case (p@game.Prisoner.B, u) => Messages.scoreMessageB =  u.toString
//  }
//
//  val gameExec = new PrisonersExec(100 millis, () => {})
//
//  def execTurn() = gameExec.execution.nextTurn()
//
//  def player(sel: (game.Prisoner.type => game.PrisonerPlayer)*) = sel.map(s => new PrisonerPlayer(gameExec, coordinator.ref, s(game.Prisoner)))
//
//  val players = player(_.A, _.B)
//  val Seq(playerA, playerB) = players
//
//  def start(): Unit = {
//    buildLayout()
//    frame.pack()
//    frame.open()
//    updateForms()
//    println()
////    build.start()
//  }
//  def stop(): Unit = {
////    build.stop()
//    frame.close()
//  }
//  def isRunning = visible
//
//  def reset(){
//    coordinator.reset()
//    playerA.reset()
//    playerB.reset()
//    Messages._history.clear()
//    Messages.scoreMessageA = "0"
//    Messages.scoreMessageB = "0"
//    updateForms()
//  }
//}


/*trait PrisonerDilemmaSwingFrame extends AbstractGameSwingFrame{
  frame =>

//  def player(sel: (game.Prisoner.type => game.PrisonerPlayer)*) = sel.map(s => new PrisonerPlayer(gameExec, coordinator.ref, s(game.Prisoner))) todo: part of dsl

//  val Seq(playerA, playerB) = players todo: depends on players number

  def start(): Unit = {
    buildLayout()
    frame.pack()
    frame.open()
    updateForms()
    println()
    //    build.start()
  }
  def stop(): Unit = {
    //    build.stop()
    frame.close()
  }
  def isRunning = visible

  def reset(){
    coordinator.reset()
//    playerA.reset()    todo: depends on players number
//    playerB.reset()
    Messages._history.clear()
    Messages.scoreMessageA = "0"
    Messages.scoreMessageB = "0"
    updateForms()
  }


  val historyListRenderer = new ListView.AbstractRenderer[(Int, (String, String)), HistoryListEntry](new HistoryListEntry) {
    def configure(list: ListView[_], isSelected: Boolean, focused: Boolean, a: (Int, (String, String)), index: Int){
      component.set(a)
    }
  }

  val scoreLabelA = monitorFor(Messages.scoreMessageA).text
  val scoreLabelB = monitorFor(Messages.scoreMessageB).text
  val historyList = monitorFor(Messages.history.toMap).list(Ordering.Int.reverse)
    .fillBoth.renderKeys(historyListRenderer).maxXWeight.maxYWeight

  val turnButton = triggerFor(execTurn()).button("play a turn")
    .sizes(min = 50 -> 20, max = Int.MaxValue -> 100, preferred = 200 -> 30)
    .fillHorizontally
  val resetButton = triggerFor(reset).button("Reset")
    .sizes(min = 50 -> 20)
    .anchor(_.West)

  def sliderControl(get: => Double, set: Double => Unit) =
    numericControlFor(get)(set)
      .slider(NumericRange.inclusive(.0, 1.0, 0.01)(Numeric.DoubleAsIfIntegral))
      .affect(_.orientation = Orientation.Vertical)

  def irrationalityControl(get: => Double, set: Double => Unit) = sliderControl(get, set).defaultLabels(0.1)

  val irrationalityA = irrationalityControl(playerA.randomChance, playerA.randomChance = _)                             //todo: depends on players number
  val irrationalityB = irrationalityControl(playerB.randomChance, playerB.randomChance = _)

  def preferenceControl(get: => Double, set: Double => Unit) =
    sliderControl(get, set).labels(
      (for(i <- 0 to 10) yield i*10 -> new Label(i*10 + "/" + (100 - i*10))).toMap
    )

  val preferenceA = preferenceControl(playerA.preference, playerA.preference = _)
  val preferenceB = preferenceControl(playerB.preference, playerB.preference = _)

  listenTo(irrationalityA, irrationalityB)
  reactions += {
    case ValueChanged(irrationalityA.formMeta.form) =>
      if(irrationalityA.value == 0) preferenceA.enabled = false
      else if(!preferenceA.enabled) preferenceA.enabled = true
    case ValueChanged(irrationalityB.formMeta.form) =>
      if(irrationalityB.value == 0) preferenceB.enabled = false
      else if(!preferenceB.enabled) preferenceB.enabled = true
  }

  val scorePanel = panel.box(_.Horizontal)(scoreLabelA -> "score-A", scoreLabelB -> "score-B")
    .layout(_.fill = Fill.Horizontal, _.weightx = 1)

  def referLabel = html(<html><center>random action<br/>preference<br/><br/>Betray/Refuse</center></html>)
  def randLabel = html(<html>random<br/>action<br/>chance</html>)

  val setupA = panel.gridBag(
    place(irrationalityA, "rand-A") in theCenter,
    place(preferenceA, "prefer-A") in theWest,
    place(randLabel, "-l-rand-A") in theSouth,
    place(referLabel, "-l-refer-A") in theNorthWest
  ).layout(_.gridheight = 2).insets(20)().maxYWeight

  val setupB = panel.gridBag(
    place(irrationalityB, "rand-B") in theCenter,
    place(preferenceB, "prefer-B") in theEast,
    place(randLabel, "-l-rand-B") in theSouth,
    place(referLabel, "-l-refer-B") in theNorthEast
  ).layout(_.gridheight = 2).insets(20)().maxYWeight

  val appPanel = panel.gridBag(
    place(turnButton, "turn") in theCenter,
    place(scorePanel) on Top of "turn",
    place(scrollable()(historyList, "history")) on theBottom of "turn",
    place(setupA, "setup-A") to theLeft of "turn",
    place(setupB, "setup-B") to theRight of "turn",
    place(label(playerA.player.toString), "a-label") to theNorthWest of "turn",
    place(label(playerB.player.toString), "b-label") to theNorthEast of "turn"
  ).fillBoth.maxXWeight.yWeight(.9)

  title = "Prisoner Dilemma Game"

  val gameTitle = <h3><b>{title}</b></h3>
  def grayI(str: String) = <i><font color="gray">{str}</font></i>
  val description =
    <html>
      in order to make a decision each agent
      <ol>
        <li>calculates expected utility for each available choice, taking in accont previous opponent's moves</li>
        <li>selects a {grayI("random")} strategy with probability set in the interface</li>
        <li>chooses the {grayI("random")} strategy with preference, set in the interface</li>
      </ol>
    </html>

  val layout = List(
    place(appPanel, "app") in theCenter,
    place(label(gameTitle), "title") on Top of "app",
    place(
      panel.box(_.Horizontal)(resetButton.component -> "reset", html(description).form -> "description").width(3).fillBoth
    ) to SouthWest of "app"
  )

  frame.minimumSize = 740 -> 520
  frame.preferredSize = 900 -> 600
}*/

//object PrisonerDilemmaExecutable extends App{
//  val app = new PrisonerDilemmaApp
//  app.start()
//}