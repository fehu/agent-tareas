package feh.tec.visual

import java.awt.Canvas
import feh.tec.visual.util.AwtEventUtils
import scala.swing.event.{WindowClosing, WindowActivated}
import org.lwjgl.opengl.{GL11, Display}

/**
 * From http://lwjgl.org/wiki/index.php?title=Using_a_Resizeable_AWT_Frame_with_LWJGL
 */
trait SwingNicolApp extends AppSurroundingSwingFrame{
  frame: SwingSurroundingFrameAppCreation#SurroundingLayoutDSL with SwingSurroundingFrameAppCreation#SurroundingLayoutBuilder =>


  lazy val drawComponent = new Canvas() with AwtEventUtils{
    canvas =>

    addComponentListener(ComponentListener(resized = _ => resizeApp(canvas.getSize)))
  }

  frame.reactions += {
    case WindowActivated(`frame`) => drawComponent.requestFocusInWindow()
    case WindowClosing(`frame`) => stop()
  }

  /**
   * binds game display into `drawComponent`; needs OpenGL Context in it's thread
   */
  def bindDisplay(){
    Display.setParent(drawComponent)
    Display.create()
  }

  /**
   * sets view port to the size of `drawComponent`; needs OpenGL Context in it's thread
   */
  def setViewPort() {
    GL11.glViewport(0, 0, drawComponent.getWidth, drawComponent.getHeight)
    Display.update()
  }

  protected def resizeApp(size: (Int, Int))

  def start(): Unit = {
    buildLayout()
    frame.open()
    app.start()
  }
  def stop(): Unit = {
    app.stop()
    frame.close()
  }
}
