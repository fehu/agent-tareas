package feh.tec.visual

import feh.tec.agent._
import feh.tec.agent.conf.AppConfig
import feh.tec.visual.AbstractGameSwingFrame._
import scala.swing.ListView.AbstractRenderer
import scala.swing.event.ButtonClicked
import scala.swing.{Label, GridBagPanel, ListView}
import java.awt.Font
import feh.tec.util._
import scala.swing.GridBagPanel.Anchor
import feh.tec.agent.AbstractGenericGame.Game2
import scala.xml.NodeSeq
import scala.collection.mutable
import feh.tec.agent.conf.AppConfig
import feh.tec.visual.GenericGameSwingFrame.HistoryList.{EntryRenderer2, EntryRenderer}

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

    lazy val titleElem: Elem[LabelBuilder[String]] = label(game.name)
    lazy val turnButton: Elem[ButtonBuilder] = triggerFor(execTurn()).button("")

    lazy val history: Elem[KeyedListBuilder[Int, List[HistoryEntry]]] =
      monitorFor(messages.history.toMap).list(Ordering.Int.reverse).renderKeys(historyRenderer)

    lazy val playerLabels: Map[Game#Player, Elem[LabelBuilder[Game#Player]]] =
      game.players.map(_.asInstanceOf[Game#Player]).zipMap{
        q =>
          Elem(html(q)(p => <center><b>{p.toString}</b><br/>{messages.score(p).toString}</center>))
      }.toMap //: Map[Game#Player, Elem[LabelBuilder[Game#Player]]]


    lazy val resetButton = Elem(triggerFor(reset()).button("Reset"))
    lazy val description = Elem(html(messages.description))

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
      updateForms().lift
    )

    override def stopSeq = super.stopSeq ::: List(frame.close().lift)

    override def resetSeq = super.resetSeq ::: List(
      coordinator.reset().lift,
      agents.foreach(_.reset()).lift,
      messages.reset().lift,
      updateForms().lift)
  }

  case class App2[G <: Game2,
                  E <: GenericMutableGameEnvironment[G, E],
                  C <: GenericMutableActorGameCoordinator[G, E],
                  A <: PlayerAgent[G, E] with PlayerAgent.Resettable[G, E],
                  Ex <: ByTurnExec[G, E]](game: G,
                                          env: E,
                                          coordinator: C,
                                          exec: Ex,
                                          agentsRef: Map[A, G#Player])
                                         (implicit protected val config: AppConfig)
    extends App with Exec
  {
    frame =>

    final type Game = G
    final type Env = E
    final type Coord = C
    final type Agent = A
    final type Exec = Ex

    protected def historyRenderer = new EntryRenderer2(game)

    val layout = null
  }

  object HistoryList{

    abstract class Entry[Game <: GenericGame](game: Game) extends GridBagPanel{
      val turn = new Label() $$ {
        c => c.font = new Font(c.font.getName, Font.BOLD, c.font.getSize * 2)
      }
      val playerInfo = game.players.asInstanceOf[Seq[Game#Player]].zipMap(buildPlayerLabel).toMap

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
        val textMap = 0
        component.set(a._1, a._2.map{
          case e => e.player.asInstanceOf[Game#Player] -> (e.choice + ": " + e.score)
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