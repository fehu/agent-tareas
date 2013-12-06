package feh.tec.visual

import feh.tec.agent._
import feh.tec.visual.AbstractGameSwingFrame._
import scala.swing.{Label, GridBagPanel, ListView}
import java.awt.Font
import feh.tec.util._
import scala.swing.GridBagPanel.Anchor
import feh.tec.agent.AbstractGenericGame.Game2
import scala.xml.NodeSeq
import scala.collection.mutable
import feh.tec.agent.conf.AppConfig
import feh.tec.visual.GenericGameSwingFrame.HistoryList.{EntryRenderer2, EntryRenderer}
import scala.swing.Slider
import scala.swing.Reactor
import scala.swing.event.ValueChanged

abstract class GenericGameSwingFrame extends AbstractGameSwingFrame {
  type Game <: GenericGame
  type Env <: GenericMutableGameEnvironment[Game, Env]
  type Coord <: GenericMutableActorGameCoordinator[Game, Env]
  type Agent <: PlayerAgent[Game, Env] with PlayerAgent.Resettable[Game, Env]

  protected def config: AppConfig

  implicit def actorSystem = config.actorSystem
  implicit def executionContext = config.executionContext

  val agents = agentsRef.keySet
  val players = game.players.asInstanceOf[Set[Game#Player]]
  val playersRef = agentsRef.map(_.swap)

}

object GenericGameSwingFrame{
  abstract class App extends GenericGameSwingFrame with GUI with ResettableGUI{
    self: Execution =>

    protected def historyRenderer: EntryRenderer[Game]

    import Description._

    def titleElem: Elem[LabelBuilder[String]] = label(game.name)
    def turnButton: Elem[ButtonBuilder] = triggerFor(execTurn()).button("Play Turn")

    def history: Elem[KeyedListBuilder[Int, List[HistoryEntry]]] =
      monitorFor(messages.history.toMap).list(Ordering.Int.reverse).renderKeys(historyRenderer)

    def playerLabels: Map[Game#Player, Elem[LabelBuilder[Game#Player]]] =
      game.players.map(_.asInstanceOf[Game#Player]).zipMap{
        q =>
          Elem(html(q)(p => <center><b>{p.toString}</b><br/>{messages.score(p).toString}</center>))
      }.toMap //: Map[Game#Player, Elem[LabelBuilder[Game#Player]]]


    def resetButton = Elem(triggerFor(reset()).button("Reset"))
    def description = Elem(html(messages.description))

    lazy val messages: Messages = new Messages{
      val score = mutable.HashMap[Game#Player, Game#Utility](initScore: _*)
      val history = mutable.HashMap.empty[Int, List[HistoryEntry]]
      var description: NodeSeq = Nil
      def reset() = {
        score ++= initScore
        history.clear()
        description = Nil
      }

      protected def initScore = players.zipMap(_ => utilityIsNumeric.zero).toSeq
    }
  }

  trait Exec extends Execution with Resettable{
    frame: App =>

    override def startSeq = super.startSeq ::: List(
      buildLayout().lift,
      frame.pack().liftUnit,
      frame.open().lift,
      updateForms().lift,
      println("Started").lift
    )

    override def stopSeq = super.stopSeq ::: List(frame.close().lift)

    override def resetSeq = super.resetSeq ::: List(
      agents.foreach(_.reset()).lift,
      messages.reset().lift,
      coordinator.reset().lift,
      updateForms().lift)
  }

  trait RandomnessPlayerControls extends App{
    self: Execution =>
    import Description._

    type Agent <: PlayerAgent[Game, Env] with PlayerAgent.Resettable[Game, Env] with PlayerAgent.RandomBehaviour[Game, Env]

    type PlayerControlsBuilder = BoxPanelBuilder
    
    class ConnectedSliders(val max: Int){
      protected var listener: Reactor = _
      protected[GenericGameSwingFrame] def setListener(r: Reactor){
        listener = r
        sliders.foreach(listener listenTo _)
        listener.reactions += {
          case ev@ValueChanged(el: Slider) if !el.adjusting && sliders.contains(el) =>
            val t = el.value - oldValue(el)
            if(currentSum + t > max) {
              el.value = oldValue(el)
            }
            else{
              currentSum += t
              oldValue(el) = el.value
              updateExtents(currentSum)
            }
        }
      }

    	private var _sliders: Seq[Slider] = Nil
      def sliders = _sliders
      protected[GenericGameSwingFrame] def register(s: Slider) = _sliders :+= s
      
      private var currentSum: Int = sliders.map(_.value).sum
      private val oldValue = mutable.HashMap.empty[Slider, Int].withDefaultValue(0)
      def updateExtents(m: Int) = sliders.foreach{ sl => sl.extent = m - sl.value }
  	}
    
    def step = 0.01

    protected def slidersConnector = new ConnectedSliders((1/step).toInt)

    protected def playerPreferenceControl(ag: Agent, connector: ConnectedSliders): BoxPanelBuilder = {
      val sliders = (panel.box(_.Vertical)() /: ag.player.availableStrategies.toSeq){
        (pBuilder, s) => pBuilder
          .appendStrut(30)
          .append(label(s.toString).component -> noId)
//          .doNotGlue
          .appendStrut(10)
          .append{
            numericControlFor(ag.preference(s))(ag.updatePreference(s, _)).slider(new UnitInterval(step))
              .horizontal.defaultLabels(0.25)
              .affect(connector.register).toComponent ->
            s"${ag.player.name}-$s-preference"
          }
        }
      sliders.affect(connector.setListener)
    }

    lazy val playerControls: Map[Game#Player, Elem[BoxPanelBuilder]] = playersRef.mapValues{
      ag => 
        val name = ag.player.name
        val conn = slidersConnector
        Elem(playerPreferenceControl(ag, conn)
          .prepend(
            numericControlFor[Double](ag.randomChance)(ag.randomChance = _)
              .slider(new UnitInterval(0.01)).horizontal.defaultLabels(0.25).toComponent -> s"$name-rand-chance"
          )
          .prependStrut(10)
          .prepend(label("random action chance").toComponent -> noId)
          .prependStrut(10).appendStrut(10).prependGlue.appendGlue
        )}.toMap
  }

  case class App2[G <: Game2,
                  E <: GenericMutableGameEnvironment[G, E],
                  C <: GenericMutableActorGameCoordinator[G, E],
                  A <: PlayerAgent[G, E] with PlayerAgent.Resettable[G, E] with PlayerAgent.RandomBehaviour[G, E],
                  Ex <: ByTurnExec[G, E]](game: G,
                                          env: E,
                                          coordinator: C,
                                          exec: Ex,
                                          agentsRef: Map[A, G#Player],
                                          descriptionHtml: NodeSeq)
                                         (implicit protected val config: AppConfig)
    extends App with Exec with RandomnessPlayerControls
  {
    frame =>

    final type Game = G
    final type Env = E
    final type Coord = C
    final type Agent = A
    final type Exec = Ex

    protected lazy val historyRenderer = new EntryRenderer2(game)

    messages.description = descriptionHtml

    override val titleElem = super.titleElem.reconfigure(_.insets(20)())
    override val turnButton = super.turnButton.reconfigure(
      _.sizes(min = 50 -> 20, max = Int.MaxValue -> 100, preferred = 200 -> 30),
      _.fillHorizontally,
      _.maxXWeight
    )
    override def history = super.history.reconfigure( _.fillBoth.maxXWeight.maxYWeight )
    override val playerLabels = super.playerLabels
    override val resetButton = super.resetButton.reconfigure(
      _.sizes(min = 50 -> 20),
      _.anchor(_.West)
    )
    override val description = super.description.reconfigure(_.insets(10)())

    lazy val mainPanel = panel.gridBag(
      place(scrollable()(history, "history").fillBoth.xWeight(.8)/*.sizes(min = 400 -> 200)*/) in theCenter,
      place(turnButton, "turn") to theNorth of "history",
  		place(playerLabels(game.A), "label-A") to theNorthWest of "history",
  		place(playerLabels(game.B), "label-B") to theNorthEast of "history",
  		place(playerControls(game.A).reconfigure(_.fillBoth, _.height(2), _.insets(10)()/*, _.xWeight(.1)*/), "controls-A") to theWest of "history",
  		place(playerControls(game.B).reconfigure(_.fillBoth, _.height(2), _.insets(10)()/*, _.xWeight(.1)*/), "controls-B") to theEast of "history"
    ).fillBoth.maxXWeight./*maxYWeight*/yWeight(.9)

//    layout

    lazy val layout = List(
    	place(mainPanel, noId) in theCenter,
    	place(titleElem, noId) at theNorth,
      place(panel.box(_.Horizontal)(resetButton -> "reset", description -> "description").width(3).fillBoth.insets(10)(bottom = 0)) in theSouthWest
		)

    override def startSeq: List[() => Unit] = List(
      (frame.minimumSize = 700 -> 500).lift,
      (frame.preferredSize = 900 -> 600).lift
    ) ::: super.startSeq
  }

  object HistoryList{

    abstract class Entry[Game <: GenericGame](game: Game) extends GridBagPanel{
      val turn = new Label() $$ {
        c => c.font = new Font(c.font.getName, Font.BOLD, c.font.getSize * 2)
      }
      val playerInfo = game.players.asInstanceOf[Set[Game#Player]].zipMap(buildPlayerLabel).toMap

      protected def buildPlayerLabel(player: Game#Player) = new Label()

      def set(turn: Int, info: Map[Game#Player, String]) = {
        this.turn.text = turn.toString
        playerInfo.zipByKey(info) foreach{
          case (_, (l, t)) => l.text = t
        }
      }

    }

    trait EntryRenderer[Game <: GenericGame] extends ListView.AbstractRenderer[(Int, List[History#HistoryEntry]), HistoryList.Entry[Game]]

    class EntryRenderer2 [Game <: Game2](game: Game)
      extends ListView.AbstractRenderer[(Int, List[History#HistoryEntry]), HistoryList.Entry[Game]](new Entry2[Game](game)) with EntryRenderer[Game]
    {
      def configure(list: ListView[_], isSelected: Boolean, focused: Boolean, a: (Int, List[History#HistoryEntry]), index: Int) = {
        component.set(a._1, a._2.map{
          case e => e.player.asInstanceOf[Game#Player] -> (e.choice + ": " + e.score + "  Σ: " + e.scoreAcc)
        }.toMap)
      }
    }

    class Entry2[Game <: Game2](game: Game) extends Entry[Game](game){
      layout ++= Map(
        turn -> (1 -> 0 : Constraints).pipe{
          c =>
            c.anchor = Anchor.Center
            c.weightx = 0
            c
        },
        playerInfo(game.A) -> (0 -> 0 : Constraints).pipe{
          c =>
            c.anchor = Anchor.West
            c.weightx = 0.5
            c
        },
        playerInfo(game.B) -> (2 -> 0 : Constraints).pipe{
          c =>
            c.anchor = Anchor.East
            c.weightx = 0.5
            c
        }
      )
    }

  }


}