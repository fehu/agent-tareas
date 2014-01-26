package feh.tec.visual.build

import feh.tec.util.build.AppsRunner
import scala.swing.Frame
import feh.tec.visual.SwingAppFrame
import feh.tec.visual.api.{StopNotifications, AppBasicControlApi}
import scala.collection.mutable
import feh.tec.util.LiftWrapper
import scala.collection.immutable.ListMap
import feh.dsl.swing.SwingFrameAppCreation

object SwingAppsRunner{
  type Control = AppBasicControlApi with StopNotifications
}

import SwingAppsRunner._

class SwingAppsRunner(val _title: String, val applications: (String, () => Control)*)
  extends Frame with AppsRunner with SwingAppFrame with SwingFrameAppCreation.Frame9PositionsLayoutBuilder with App
{


  val apps = ListMap(applications: _*)

  def start(): Unit = open()
  def stop(): Unit = {
    runningApps.foreach(_._2.stop())
    close()
    sys.exit()
  }
  def isRunning = visible

  val runningApps = mutable.HashMap.empty[String, Control]

  protected def stopped(app: Control) = {
    runningApps.find(_._2 == app).foreach{
      case (name, _) =>
        runningApps -= name
        println(s"'$name' has stopped")
    }
  }

  def buildAppButton(name: String, build: () => Control) =
    triggerFor{
      val app = runningApps.getOrElseUpdate(name, {
        val a = build()
        println(s"created app '$name'")
        runningApps += name -> a
        a.stopNotifications += stopped(a).liftUnit
        a
      })
      if(app.isRunning){
        app.stop()
        println(s"stopped app '$name'")
        runningApps -= name
      }
      else app.start()
    }
    .button(name) /*todo: toggle()*/


  val buttons = panel.flow(_.Leading)(apps.map{
    case (name, app) => componentIdPairToUnplacedLayoutElem(buildAppButton(name, app).component -> name)
    }.toSeq: _*)

  val layout = List(
    place(buttons) in theCenter
  )

  private def getButtons = buttons.elems.map(_.meta.component)
  private def maxWidth = getButtons.map(_.preferredSize.width).max
  private def heightSum = getButtons.map(_.preferredSize.height).sum
  private def percent(v: Int, p: Double) = (v * p).toInt

  preferredSize = percent(maxWidth, 1.1) -> percent(heightSum, 1.5)

  buildLayout()

  title = _title

  start()

  override def closeOperation(){
    runningApps.foreach(_._2.stop())
    sys.exit()
  }
}
