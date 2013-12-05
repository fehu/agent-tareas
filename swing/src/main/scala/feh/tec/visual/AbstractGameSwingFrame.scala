package feh.tec.visual

import scala.swing.{ListView, Component, Frame}
import feh.tec.agent._
import akka.actor.ActorSystem
import scala.concurrent.ExecutionContext
import feh.tec.util._
import scala.util.Try
import scala.collection.mutable
import scala.swing.event.ButtonClicked
import scala.xml.NodeSeq

abstract class AbstractGameSwingFrame extends /*Main*/Frame with SwingAppFrame with SwingFrameAppCreation.Frame9PositionsLayoutBuilder{
  frame =>

  type Game <: AbstractGame
  type Env <: MutableGameEnvironmentImpl[Game, Env]
  type Coord <: MutableGameCoordinator[Game, Env] with GameCoordinatorWithActor[Game, Env]
  type Agent <: PlayerAgent[Game, Env]

  implicit def actorSystem: ActorSystem
  implicit def executionContext: ExecutionContext

  def game: Game
  def env: Env
  def coordinator: Coord

  def agents: Set[Agent]
  def players: Set[Game#Player]
  def agentsRef: Map[Agent, Game#Player]
  def playersRef: Map[Game#Player, Agent]

  implicit def utilityIsNumeric = game.utilityIsNumeric.asInstanceOf[Numeric[Game#Utility]]

  def startSeq: List[Lifted[Unit]]
  def stopSeq: List[Lifted[Unit]]
}

object AbstractGameSwingFrame{
  trait Execution extends AbstractGameSwingFrame{
    type Exec <: ByTurnExec[Game, Env]

    def exec: Exec
    def execTurn() = exec.execution.nextTurn()

    private var running: Boolean = false
    def isRunning = running

    def start(): Unit = Try{
      running = true
      startSeq.foreach(_())
    }.recover{
      case ex =>
        stop()
        throw ex
    }
    def stop(){
      running = false
      stopSeq.foreach(_())
    }

    def startSeq: List[Lifted[Unit]] = Nil
    def stopSeq: List[Lifted[Unit]] = Nil
  }

  trait Resettable {
    self: Execution =>
    def reset() = resetSeq.foreach(_())

    def resetSeq: List[Lifted[Unit]] = Nil
  }

  trait History extends AbstractGameSwingFrame{
    def messages: Messages

    case class HistoryEntry(player: Game#Player, choice: Game#Player#Strategy, score: Game#Utility, scoreAcc: Game#Utility)

    /**
     * call only once
     */
    protected def listenTurnUpdates = coordinator.listenToEndOfTurn{
      case (turn, choices, score) =>
        messages.appendHistory(turn, choices, score)
        updateForms()
    }

    trait Messages{
      def score: mutable.Map[Game#Player, Game#Utility]
      def history: mutable.Map[Int, List[HistoryEntry]]

      // called from history
      protected def updateScore(player: Game#Player, upd: Game#Utility) = score <<= (player, num.plus(_, upd))
      def appendHistory(turn: Turn, choices: Game#PlayersChoices, utility: Game#PlayersUtility) = {
        history += turn.id -> players.toList.map{
          p =>
            updateScore(p, utility(p.cast))
            HistoryEntry(p, choices(p.cast).asInstanceOf[Game#Player#Strategy], utility(p.cast), score(p))
        }
      }
      var description: NodeSeq

      def reset()

      private def num = utilityIsNumeric
    }
  }


  trait GUI extends AbstractGameSwingFrame with AbstractGUI with History{
    import Description._

    type PlayerControlsBuilder <: AbstractBuilder

    def titleElem: Elem[LabelBuilder[String]]
    def turnButton: Elem[ButtonBuilder]
    def history: Elem[KeyedListBuilder[Int, List[HistoryEntry]]] //with HistoryApi with Needs[ListView.AbstractRenderer[(Int, List[HistoryEntry]), _]]
    def playerLabels: Map[Game#Player, Elem[LabelBuilder[Game#Player]]]
    def playerControls: Map[Game#Player, Elem[PlayerControlsBuilder]]
    def description: Elem[LabelBuilder[NodeSeq]]

  }

  trait ResettableGUI extends Resettable{
    self: Execution with GUI =>
    import Description._

    def resetButton: Elem[ButtonBuilder]
  }
}
