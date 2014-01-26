package feh.tec.visual

import feh.tec.visual.api.AppBasicControlApi
//import scala.swing._
import feh.dsl.swing.{SwingAppFrame => SwingFrame, AppFrameControl, SwingFrameAppCreation}

trait T extends AppFrameControl with AppBasicControlApi

trait SwingAppFrame extends SwingFrame with T/*AppBasicControlApi with AppFrameControl*/ {
  self: SwingFrameAppCreation#LayoutDSL with SwingFrameAppCreation#LayoutBuilder =>

  override def closeOperation(): Unit = {
    stop()
    super.closeOperation()
  }
}

trait AppSurroundingSwingFrame extends SwingAppFrame with SwingWindowedApp
//  with SwingSurroundingFrameAppCreation
{
  self: SwingFrameAppCreation#LayoutDSL with SwingFrameAppCreation#LayoutBuilder =>
//  self: SwingSurroundingFrameAppCreation#SurroundingLayoutDSL with SwingSurroundingFrameAppCreation#SurroundingLayoutBuilder =>

  def appWindow = this
}