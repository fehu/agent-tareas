package feh.tec.visual

import scala.swing.event.{WindowClosing, WindowActivated}
import org.lwjgl.opengl.{AWTGLCanvas, GL11, Display}
import scala.swing.{Swing, Component}
import feh.dsl.swing.swing.Canvas
import java.awt.Color
import feh.dsl.swing.{FormCreation, SwingFrameAppCreation}

trait SwingNicolApp extends AppSurroundingSwingFrame{
  frame: SwingFrameAppCreation#LayoutBuilder with SwingFrameAppCreation#LayoutDSL =>

  case class AWTGLCanvasBuildMeta(component: Component,
                                  effects: List[AWTGLCanvasBuildMeta#Comp => Unit] = Nil,
                                  layout: List[(FormCreation.Constraints) => Unit] = Nil)
    extends SwingFrameAppCreation.BuildMeta with AbstractDSLBuilder
  {
    type Comp = LWJGLCanvas
    def affect(effects: (Comp => Unit) *) = copy(effects = effects.toList)

    def layout(effects: (FormCreation.Constraints => Unit) *) = copy(layout = effects.toList)

    def `type` = "AWTGLCanvas"
  }

  implicit def aWTGLCanvasToBuildMetaWrapper(c: LWJGLCanvas) = AWTGLCanvasBuildMeta(c)

  lazy val drawComponent = {
    val c = new LWJGLCanvas
    c.border = Swing.LineBorder(Color.red)
    c
  }

  frame.reactions += {
    case WindowActivated(`frame`) => drawComponent.requestFocusInWindow()
    case WindowClosing(`frame`) => stop()
  }

  /**
   * binds game display into `drawComponent`; needs OpenGL Context in it's thread
   */
  def bindDisplay(){
    Display.setParent(drawComponent.canvas)
    Display.create()
  }

  /**
   * sets view port to the size of `drawComponent`; needs OpenGL Context in it's thread
   */
  def setViewPort() {
    GL11.glViewport(0, 0, drawComponent.size.width, drawComponent.size.height)
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

class LWJGLCanvas extends Canvas(new AWTGLCanvas())
//class JAWTGLCanvas extends JCanvas(new AWTGLCanvas())