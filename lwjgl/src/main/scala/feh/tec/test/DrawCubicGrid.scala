package feh.tec.test

import feh.tec.util._
import nicol.{Game, ShowFPS, SyncableScene, Scene}
import java.awt.Color
import feh.tec.visual.api.{OpenGLEasel, Easel3DFloat}
import feh.tec.visual.NicolLike3DEasel
import org.lwjgl.opengl.{Display, GL11}
import nicol.input.Key._
import nicol.opengl.GLUtils
import org.lwjgl.util.vector.Vector3f


class DrawCubicGrid(exitScene: Lifted[Scene], pauseScene: Scene => Scene) extends LoopScene3D with SyncableScene with ShowFPS{

  implicit val easel = new NicolLike3DEasel

  def drawGrid(nx: Int, ny: Int, nz: Int, cubeSide: Float, lineColor: Color)(implicit easel : Easel3DFloat with OpenGLEasel){
    def lineLen(i: Int) = cubeSide * i
    val (lenX, lenY, lenZ) = (lineLen(nx), lineLen(ny), lineLen(nz))

    easel.withoutTextures{
      easel.withColor(lineColor){
        for{ i <- 0 to ny; j <- 0 to nz; cy = cubeSide*i; cz = cubeSide*j } easel.drawLine((0f, cy, cz), (lenX, cy, cz))
        for{ i <- 0 to nx; j <- 0 to nz; cx = cubeSide*i; cz = cubeSide*j } easel.drawLine((cx, 0f, cz), (cx, lenY, cz))
        for{ i <- 0 to nx; j <- 0 to ny; cx = cubeSide*i; cy = cubeSide*j } easel.drawLine((cx, cy, 0f), (cx, cy, lenZ))
      }
    }

  }

  def gridVertices(nx: Int, ny: Int, nz: Int, cubeSide: Float, lineColor: Color) = {
    def lineLen(i: Int) = cubeSide * i
    val (lenX, lenY, lenZ) = (lineLen(nx), lineLen(ny), lineLen(nz))

    {
      (for{ i <- 0 to ny; j <- 0 to nz; cy = cubeSide*i; cz = cubeSide*j } yield (0f, cy, cz) :: (lenX, cy, cz) :: Nil) ++
      (for{ i <- 0 to nx; j <- 0 to nz; cx = cubeSide*i; cz = cubeSide*j } yield (cx, 0f, cz) :: (cx, lenY, cz) :: Nil) ++
      (for{ i <- 0 to nx; j <- 0 to ny; cx = cubeSide*i; cy = cubeSide*j } yield (cx, cy, 0f) :: (cx, cy, lenZ) :: Nil)
    }.flatten.toArray
  }

  lazy val camera = new Camera3DFloat(new Vector3f(0, 0, 0), new Vector3f(0, 0, 0), 71 /*45*/, Display.getWidth / Display.getHeight, 0.1f, 100)

  def update: Option[Scene] = {
    sync
    showFPS


    /*

    sys.props += "org.lwjgl.librarypath" -> "/home/fehu/dev/ScalaProjects/agentes-tarea-1/lwjgl/target/scala-2.10/resource_managed/main/lwjgl-resources/linux"
    import feh.tec.test._
    val gg = new DrawCubicGrid(() => null, _ => null)
    val game = new nicol.Game(Init3D("Draw Cubic Grid Test") >> gg) {}
    import gg._
    game.start

    */

//    camera.apply()

    GL11.glLoadIdentity()

    // Clean both color and depth buffers
    GL11.glClear(GL11.GL_COLOR_BUFFER_BIT | GL11.GL_DEPTH_BUFFER_BIT)

    // Translate into the view
    GL11.glTranslatef(0, -50, -150)

    // Rotate on both x and y axes
    GL11.glRotatef(1, 1, 1, 0)

    GLUtils.draw(GLUtils.LineStrip){
//      easel.withAffineTransform(easel.Offset((-100, -100, -100))){
        drawGrid(5, 5, 5, 50, Color.gray)
//      }
    }

    // Translate back
//    GL11.glTranslatef(0, 50, 55)

//    GL11.glDisableClientState(GL_VERTEX_ARRAY)

    if (up) camera.move(100, 0)
    if (down) camera.move(100, 2)
    if (left) camera.move(100, 1)
    if (right) camera.move(100, -1)

    keyEvent {
      e =>
        e released {
          case _ =>
        }
        e pressed {
          case "escape" =>
            exitScene()
          case "space" =>
            pauseScene(this)
        }
    }

  }
}


class DrawCubicGridGame extends Game(Init3D("Draw Cubic Grid Test") >> new DrawCubicGrid(() => null, _ => null))

object DrawCubicGridApp extends App{
  val game = new DrawCubicGridGame
  game.start
}