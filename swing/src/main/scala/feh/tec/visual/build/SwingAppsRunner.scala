package feh.tec.visual.build

import feh.tec.util.build.AppsRunner
import scala.swing.{Orientation, Frame}
import feh.tec.visual.{SwingFrameAppCreation, SwingAppFrame}
import feh.tec.visual.api.AppBasicControlApi
import scala.collection.mutable

class SwingAppsRunner(val _title: String, val applications: (String, () => AppBasicControlApi)*) extends Frame with AppsRunner
  with SwingAppFrame with SwingFrameAppCreation.Frame9PositionsLayoutBuilder with App
{

  val apps: Map[String, () => AppBasicControlApi] = applications.toMap

  def start(): Unit = open()
  def stop(): Unit = close()
  def isRunning = visible

  val runningApps = mutable.HashMap.empty[String, AppBasicControlApi]
  
  def buildAppButton(name: String, build: () => AppBasicControlApi) =
    triggerFor{
      val app = runningApps.getOrElseUpdate(name, build())
      if(app.isRunning){
        app.stop()
        runningApps -= name
      }
      else app.start()
    }
    .button(name) /*todo: toggle()*/


  val buttons = panel.box(_.Horizontal)(apps.map{
    case (name, app) => componentIdPairToUnplacedLayoutElem(buildAppButton(name, app).component -> name)
    }.toSeq: _*)

  val layout = List(
    place(buttons) in theCenter
//    place(scrollable()(buttons, "scroll")) in theCenter
  )
  preferredSize = 300 -> 300

  buildLayout()

  title = _title

  start()

  override def closeOperation(){
    runningApps.foreach(_._2.stop())
    sys.exit()
  }
}
