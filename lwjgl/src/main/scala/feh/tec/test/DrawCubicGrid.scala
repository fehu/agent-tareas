package feh.tec.test

import feh.tec.util._
import nicol.{Game, ShowFPS, SyncableScene, Scene}
import java.awt.Color
import feh.tec.visual.api.{OpenGLEasel, Easel3DFloat}
import feh.tec.visual.NicolLike3DEasel
import org.lwjgl.opengl.{Display, GL15, GL11}
import feh.tec.test.GLType.{GL_UNSIGNED_INT, GL_FLOAT}
import nicol.input.Key._
import feh.tec.test.DataStoreUsagePattern.GL_STATIC_DRAW
import feh.tec.test.BindBufferTarget.{GL_ELEMENT_ARRAY_BUFFER, GL_ARRAY_BUFFER}
import feh.tec.test.ClientState.GL_VERTEX_ARRAY
import nicol.opengl.GLUtils
import org.lwjgl.util.vector.Vector3f
import org.lwjgl.util.glu.GLU
import scala.io.Source
import feh.tec.test.DrawMode.GL_LINE


class DrawCubicGrid(exitScene: Lifted[Scene], pauseScene: Scene => Scene) extends LoopScene3D with SyncableScene with ShowFPS{

  implicit val easel = new NicolLike3DEasel

  def drawGrid(nx: Int, ny: Int, nz: Int, cubeSide: Float, lineColor: Color)(implicit easel : Easel3DFloat with OpenGLEasel){
    def lineLen(i: Int) = cubeSide * i
    val (lenX, lenY, lenZ) = (lineLen(nx), lineLen(ny), lineLen(nz))

    easel.withoutTextures{
      easel.withColor(lineColor){
        for{ i <- 0 to ny; j <- 0 to nz; cy = cubeSide*i; cz = cubeSide*j } easel.drawLine((0, cy, cz), (lenX, cy, cz))
        for{ i <- 0 to nx; j <- 0 to nz; cx = cubeSide*i; cz = cubeSide*j } easel.drawLine((cx, 0, cz), (cx, lenY, cz))
        for{ i <- 0 to nx; j <- 0 to ny; cx = cubeSide*i; cy = cubeSide*j } easel.drawLine((cx, cy, 0), (cx, cy, lenZ))
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


  lazy val vertexBuffer = Float3DBuff(gridVertices(5, 5, 5, 50, Color.gray))
    .bind(GL_ARRAY_BUFFER, GL_STATIC_DRAW)

  lazy val indexBuffer = IntBuff(0 until vertexBuffer.data.length)
    .bind(GL_ELEMENT_ARRAY_BUFFER, GL_STATIC_DRAW)

  def disposeBuffers() = {
    GL15.glDeleteBuffers(vertexBuffer.id)
    GL15.glDeleteBuffers(indexBuffer.id)
  }

  lazy val camera = new Camera3DFloat(new Vector3f(0, 0, 0), new Vector3f(0, 0, 0), 71 /*45*/, Display.getWidth / Display.getHeight, 0.1f, 100)

  lazy val shaderProgram = new ShaderProgram(
      Source.fromURL(ClassLoader.getSystemResource("lwjgl/test/shader.vert")),
      Source.fromURL(ClassLoader.getSystemResource("lwjgl/test/shader.frag"))
    ).linked()

  def update: Option[Scene] = {
    sync
    showFPS

    camera.apply()

    shaderProgram.bind()

    shaderProgram.setUniform("projection", camera.projection)
    shaderProgram.setUniform("view", camera.view)

    // Clean both color and depth buffers
    GL11.glClear(GL11.GL_COLOR_BUFFER_BIT | GL11.GL_DEPTH_BUFFER_BIT)

    // Translate into the view
//    GL11.glTranslatef(0, -50, -55)

    // Rotate on both x and y axes
//    GL11.glRotatef(1, 1, 1, 0)

/*
    GLUtils.draw(GLUtils.LineStrip){
//      easel.withAffineTransform(easel.Offset((-100, -100, -100))){
        drawGrid(5, 5, 5, 50, Color.gray)
//      }
    }
*/

    // Bind the vertex VBO
//    GL11.glEnableClientState(GL_VERTEX_ARRAY)
    vertexBuffer.bind()
    GL11.glVertexPointer(3, GL_FLOAT, 12, 0)


//    indexBuffer.bind()
    GL11.glDrawElements(GL11.GL_LINE_STRIP, indexBuffer.size, GL_UNSIGNED_INT, 0)

//    GL11.glDrawArrays(GL_LINE, 0, 0)

    // Translate back
//    GL11.glTranslatef(0, 50, 55)

//    GL11.glDisableClientState(GL_VERTEX_ARRAY)

    shaderProgram.unbind();

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
            shaderProgram.dispose()
            disposeBuffers()
            exitScene()
          case "space" =>
            shaderProgram.dispose()
            disposeBuffers()
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