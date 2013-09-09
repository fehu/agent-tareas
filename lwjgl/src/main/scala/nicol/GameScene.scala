package nicol

/*
  Copied from Tom Streller's (scan) Nicol-0.1.2 project (https://github.com/scan/Nicol)
  due to a lack of scala 2.10 compatible version
 */


import org.lwjgl.opengl.{Display}
import org.lwjgl.Sys

//import renderer._

/*
/**
 * This is the base class for any non-empty scene, any real game scene. You just override update to get a nice custom scene.
 */
trait GameScene extends LoopScene {
  def draw[A](that: A, position: (Float, Float) = (0, 0), rgb: (Float, Float, Float) = (1, 1, 1), rotation: Float = 0, offset: (Float, Float) = (0, 0))(implicit renderer: Renderer[A]): Unit = renderer.draw(that, position, rgb, rotation, offset)
}

object GameScene {
  /**
   * This should be used to make small scenes. For bigger ones, a real GameScene child is recommended.
   */
  def apply(f: => Option[Scene]) = new GameScene {
    @inline
    def update = f
  }
}*/

/**
 * A mixin you can use if you need a `sync` method to keep within a certain fps. Also enables showing the
 * current FPS in the title bar.
 */
trait SyncableScene {
  val targetFPS = 60

  def sync = Display.sync(targetFPS)
}

/**
 * This mixin gives you a method to write the current FPS into the title bar.
 */
trait ShowFPS {

  import Sys._

  private var lastFPS = time
  private var fps = 0

  private def time = (getTime * 1000) / getTimerResolution

  private lazy val title = Display.getTitle

  private def updateFPS = {
    if (time - lastFPS > 1000) {
      Display.setTitle(title + " [" + fps + "]")
      fps = 0
      lastFPS = time
    }
    fps += 1
  }

  def showFPS = updateFPS
}