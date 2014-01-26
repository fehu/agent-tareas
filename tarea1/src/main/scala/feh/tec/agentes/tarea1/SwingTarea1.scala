package feh.tec.agentes.tarea1

import feh.tec.visual.SwingNicolApp
import feh.tec.agent.AgentDecision.CriteriaMessage
import nicol.Scene
import scala.swing.GridBagPanel.Fill
import scala.swing.event.UIElementShown
import feh.dsl.swing.SwingFrameAppCreation
import SwingFrameAppCreation._
import feh.dsl.swing.util.AwtUtils

class SwingTarea1(t1: Tarea1App) extends SwingNicolApp
  /*with SwingFrameAppCreation */with Layout9PositionsDSL with Frame9PositionsLayoutBuilder
{
  frame =>
  lazy val app = t1.app

  def isRunning: Boolean = app.isRunning // todo ??

  override def start(): Unit = {
    prepareApp(app)
    super.start()
  }

  protected def prepareApp(app: NicolBasedTarea1AgentApp){
    app.overrideInitScene(_ => Scene {
      frame.open()
      frame.pack()
      frame.reactions += {
        case UIElementShown(`drawComponent`) => bindDisplay()
      }
      frame.listenTo(drawComponent)
    })
    app.overrideBaseScene{
      old => Scene {
          setViewPort()
          updateForms()
          old.apply
        }
    }
  }

  protected def resizeApp(size: (Int, Int)): Unit = {
    println(s"resized: $size")
  }


  def criteriaMessage[CV](msg: CriteriaMessage[CV]) = msg.beforeOpt.map(_ + "\n").getOrElse("") + msg.afterOpt.getOrElse("")

  lazy val layout =
    (place(drawComponent.layout(_.fill = Fill.Both, _.weightx = 1, _.weighty = 1), "lwjgl") at theCenter) and
    (place(monitorFor(t1.ag.lastDecision.map(_.message) map criteriaMessage getOrElse "").asLabel, "lastDecision.message") to theRight of drawComponent)
}

object SwingTarea1App extends App with AwtUtils{
  // init ag and environment
//  Tarea1App.overseer

  val t1 = new Tarea1App
  val swingApp = new SwingTarea1(t1)
  t1.ag
  swingApp.preferredSize = 1024 -> 786
  swingApp.minimumSize = 1024 -> 786
  swingApp.start()
}
