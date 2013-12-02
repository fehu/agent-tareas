package feh.tec.agentes.tarea1

import feh.tec.visual.{AppSurroundingSwingFrame, SwingNicolApp}
import feh.tec.visual.api.{AppBasicControlApi, AgentApp}
import feh.tec.agent.AgentDecision.CriteriaMessage
import feh.tec.visual.util.AwtUtils
import nicol.Scene
/*

class SwingTarea1 extends SwingNicolApp with LayoutDSLDefaultImpl with Frame9PositionsLayoutBuilder{
  lazy val layout =
    (place(drawComponent, "lwjgl") at theCenter) and
    (place(monitorFor(Tarea1App.ag.lastDecision.map(_.message) map criteriaMessage getOrElse "").asLabel, "lastDecision.message") to theRight of drawComponent)

  def criteriaMessage[CV](msg: CriteriaMessage[CV]) = msg.beforeOpt.map(_ + "\n").getOrElse("") + msg.afterOpt.getOrElse("")

  def build: AgentApp with AppBasicControlApi = {
    val a = Tarea1App.build
    a.overrideInitScene(_ => Scene {
        bindDisplay()
    })
    a.overrideBaseScene{
      old => Scene {
          setViewPort()
          updateForms()
          old.apply
        }
    }
    a
  }

  protected def resizeApp(size: (Int, Int)): Unit = {
    println(s"resized: $size")
  }

}

object SwingTarea1App extends App with AwtUtils{
  // init ag and environment
//  Tarea1App.overseer

  val swingApp = new SwingTarea1
    Tarea1App.ag
  swingApp.preferredSize = 1024 -> 786
  swingApp.minimumSize = 1024 -> 786
  swingApp.start()
}
*/
