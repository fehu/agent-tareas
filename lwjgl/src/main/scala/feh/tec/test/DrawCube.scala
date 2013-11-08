package feh.tec.test

/**
 *
 * Using http://www.java-gaming.org/topics/lwjgl-tutorial-series-contents/30681/view.html as an example
 *
 */

import nicol._
import nicol.input.Key._
import feh.tec.util.Lifted
import org.lwjgl.opengl.{GL11, GL15}
import org.lwjgl.util.vector.Vector3f
import org.lwjgl.BufferUtils
import java.nio.FloatBuffer
import feh.tec.test.VertexType.GL_FLOAT
import feh.tec.test.ClientState._
import feh.tec.test.DrawMode.{GL_QUAD_STRIP, GL_TRIANGLE_STRIP, GL_TRIANGLES, GL_QUADS}
import feh.tec.test.BindBufferTarget.GL_ARRAY_BUFFER
import feh.tec.test.DataStoreUsagePattern.GL_STATIC_DRAW
import nicol.opengl.GLUtils
import nicol.opengl.GLUtils.Quads

class DrawCube(exitScene: Lifted[Scene], pauseScene: Scene => Scene) extends LoopScene3D with SyncableScene with ShowFPS{

  type Coord = (Float, Float, Float)
  val Pi = scala.math.Pi.toFloat
  def cos = scala.math.cos _ andThen(_.toFloat)
  def sin = scala.math.sin _ andThen(_.toFloat)
  def sqrt = scala.math.sqrt _ andThen(_.toFloat)

  private def vec(c: Coord) = new Vector3f(c._1, c._2, c._3)

  implicit class TripleFloatWrapper(v: (Float, Float, Float)){
    def + (v2: (Float, Float, Float)) = (v._1 + v2._1, v._2 + v2._2, v._3 + v2._3)
    def - (v2: (Float, Float, Float)) = (v._1 - v2._1, v._2 - v2._2, v._3 - v2._3)
//    def rotatePhi(phi: Float) = (v._1 * cos(phi), v._2* sin(phi), v._3)
//    def zProjection = sqrt(v._1 * v._1+ v._2 * v._2)
//    def rotateTheta(phi: Float) = (v._1, v._2, v._3 * )
  }

  case class Cube(center: Coord, edge: Float){
    lazy val sides: Seq[(Coord, Coord, Coord, Coord)] = {
      val (maxX, maxY, maxZ) = center + (edge / 2, edge / 2, edge / 2)
      val (minX, minY, minZ) = center - (edge / 2, edge / 2, edge / 2)

      ((maxX, maxY, maxZ), (maxX, maxY, minZ), (maxX, minY, maxZ), (maxX, minY, minZ)) ::
      ((minX, maxY, maxZ), (maxX, maxY, maxZ), (minX, minY, maxZ), (maxX, minY, maxZ)) ::
      ((minX, maxY, minZ), (minX, maxY, maxZ), (minX, minY, minZ), (minX, minY, maxZ)) ::
      ((maxX, maxY, minZ), (minX, maxY, minZ), (maxX, minY, minZ), (minX, minY, minZ)) ::
      ((minX, maxY, maxZ), (maxX, maxY, maxZ), (minX, maxY, minZ), (maxX, maxY, minZ)) ::
      ((minX, minY, maxZ), (maxX, minY, maxZ), (minX, minY, minZ), (maxX, minY, minZ)) :: Nil

    }

    //      ((maxX, maxY, maxZ), (maxX, maxY, minZ), (maxX, minY, minZ), (maxX, minY, maxZ)) ::
    //      ((minX, maxY, maxZ), (maxX, maxY, maxZ), (maxX, minY, maxZ), (minX, minY, maxZ)) ::
    //      ((minX, maxY, minZ), (minX, maxY, maxZ), (minX, minY, maxZ), (minX, minY, minZ)) ::
    //      ((maxX, maxY, minZ), (minX, maxY, minZ), (minX, minY, minZ), (maxX, minY, minZ)) ::
    //      ((maxX, maxY, maxZ), (minX, maxY, maxZ), (minX, maxY, minZ), (maxX, maxY, minZ)) ::
    //      ((minX, minY, minZ), (maxX, minY, minZ), (maxX, minY, maxZ), (minX, minY, maxZ)) :: Nil

    lazy val textureCoords: Seq[(Coord, Coord, Coord, Coord)] =

      ((1f, 0f, 0f), (1f, 0f, 0f), (1f, 0f, 0f), (1f, 0f, 0f)) ::
      ((0f, 1f, 0f), (0f, 1f, 0f), (0f, 1f, 0f), (0f, 1f, 0f)) ::
      ((1f, 0f, 0f), (1f, 0f, 0f), (1f, 0f, 0f), (1f, 0f, 0f)) ::
      ((0f, 1f, 0f), (0f, 1f, 0f), (0f, 1f, 0f), (0f, 1f, 0f)) ::
      ((0f, 0f, 1f), (0f, 0f, 1f), (0f, 0f, 1f), (0f, 0f, 1f)) ::
      ((0f, 0f, 1f), (0f, 0f, 1f), (0f, 0f, 1f), (0f, 0f, 1f)) :: Nil
//      ((1F, 1F, 1F), (1F, 1F, 0F), (1F, 0F, 0F), (1F, 0F, 1F)) ::
//      ((0F, 1F, 1F), (1F, 1F, 1F), (1F, 0F, 1F), (0F, 0F, 1F)) ::
//      ((0F, 1F, 0F), (0F, 1F, 1F), (0F, 0F, 1F), (0F, 0F, 0F)) ::
//      ((1F, 1F, 0F), (0F, 1F, 0F), (0F, 0F, 0F), (1F, 0F, 0F)) ::
//      ((1F, 1F, 1F), (0F, 1F, 1F), (0F, 1F, 0F), (1F, 1F, 0F)) ::
//      ((0F, 0F, 0F), (1F, 0F, 0F), (1F, 0F, 1F), (0F, 0F, 1F)) :: Nil
    
    private def coordToVector4(c: (Coord, Coord, Coord, Coord)) = c match{
      case (c1, c2, c3, c4) => vec(c1) :: vec(c2) :: vec(c3) :: vec(c4) :: Nil
    }

    // cannot overload `asVectors` due to erasure of `sel` function types in VM
    def asVectorz(sel: this.type => (Coord, Coord, Coord, Coord)): Seq[Vector3f] = coordToVector4(sel(this))
    def asVectors(sel: this.type => Seq[(Coord, Coord, Coord, Coord)]): Seq[Vector3f] = sel(this) flatMap coordToVector4

  }

  val cube = Cube((1, 1, 1), 2)

  def glVertex = (GL11.glVertex3f _).tupled

  def vbo = VBO(GL_ARRAY_BUFFER, GL_STATIC_DRAW) _

  lazy val cubeVbo2 = vbo(cube.asVectors(_.sides), cube.asVectors(_.textureCoords))
  cube.sides.foreach(println)
  lazy val cubeVbo = ObjVertices.Cube.vbo

  lazy val pyramidVbo = ObjVertices.Pyramid.vbo

  def render(){
    cubeVbo2.render
  }


  def update: Option[Scene] = {
    sync
    showFPS

    render()

    keyEvent {
      e =>
        e released {
          case _ =>
        }
        e pressed {
          case "escape" =>
            cubeVbo.dispose()
            exitScene()
          case "space" => pauseScene(this)
        }
    }
  }

}

//object DrawCube{
//  def drawCube()
//}

case class VBO(vertexBuffer: BoundBuffer, textureBuffer: BoundBuffer){
  def render(){

    // Clean both color and depth buffers
    GL11.glClear(GL11.GL_COLOR_BUFFER_BIT | GL11.GL_DEPTH_BUFFER_BIT);

    // Translate into the view
    GL11.glTranslatef(0, 0, -5);

    // Rotate on both x and y axes
    GL11.glRotatef(1, 1, 1, 0);

    // Bind the vertex VBO
    vertexBuffer.bind()
    GL11.glVertexPointer(3, GL_FLOAT, 0, 0);

    // Bind the color VBO
    textureBuffer.bind()
    GL11.glColorPointer(3, GL_FLOAT, 0, 0);

    // Draw the cube with triangle strip
    GL11.glDrawArrays(GL_TRIANGLE_STRIP /*GL_QUAD_STRIP*/, 0, 24);

    // Translate back
    GL11.glTranslatef(0, 0, 5);

    // works for pyramid
//    GL11.glClear(GL11.GL_COLOR_BUFFER_BIT | GL11.GL_DEPTH_BUFFER_BIT)
//
//    // Rotate on the y-axis
//    GL11.glRotatef(1, 0, 1, 0)
//
//    // Enable Client states
//    GL11.glEnableClientState(GL_VERTEX_ARRAY)
//    GL11.glEnableClientState(GL_COLOR_ARRAY)
////    GL11.glEnableClientState(GL_TEXTURE_COORD_ARRAY)
//
//    // Bind the vertex buffer
//    vertexBuffer.bind()
//    GL11.glVertexPointer(3, GL_FLOAT.int, 0, 0)
//
//    // Bind the texture buffer
//    textureBuffer.bind()
////    GL11.glTexCoordPointer(3, GL_FLOAT.int, 0, 0)
//    GL11.glColorPointer(3, GL_FLOAT.int, 0, 0)
//
//
//
//    // Draw the textured object
//    GL11.glDrawArrays(GL_TRIANGLES, 0, 12)
//
//
//    // Disable Client states
//    GL11.glDisableClientState(GL_VERTEX_ARRAY)
//    GL11.glDisableClientState(GL_TEXTURE_COORD_ARRAY)

  }

  def dispose(){
    GL15.glDeleteBuffers(vertexBuffer.id)
    GL15.glDeleteBuffers(textureBuffer.id)
  }
}

object VBO{
  def apply(target: BindBufferTarget, storage: DataStoreUsagePattern, id: BufferId = BufferId())
           (vertices: Seq[Vector3f], textureCoords: Seq[Vector3f]): VBO = {
    val verticesBuffer = FloatBuff(vertices: _*).bind(target, storage)
    val texturesBuffer = FloatBuff(textureCoords: _*).bind(target, storage)
    VBO(verticesBuffer, texturesBuffer)
  }

}

class DrawCubeGame extends Game(Init3D("Draw Cube Test") >> new DrawCube(() => null, _ => null))

object DrawCubeApp extends App{
  val game = new DrawCubeGame
  game.start
}

case class BufferId(id: Int = GL15.glGenBuffers)
object BufferId{
  implicit def bufferIdToInt: BufferId => Int = _.id
}

class FloatBuff(val vertices: Seq[Vector3f]){
  lazy val buffer = {
    val buff = BufferUtils.createFloatBuffer(vertices.length * 3)
    vertices.foreach(_.store(buff))
    buff.rewind()
    buff
  }

  def bind(target: BindBufferTarget, storage: DataStoreUsagePattern, id: BufferId = BufferId()) =
    BoundBuffer(vertices, id, target, storage)
}

object FloatBuff{
  def apply(vertices: Vector3f*): FloatBuff = new FloatBuff(vertices)
  def unapply(buf: FloatBuff): Option[(Seq[Vector3f], FloatBuffer)] = Some(buf.vertices -> buf.buffer)

  implicit def vertexBufferToFloatBuffer = (_: FloatBuff).buffer
}

abstract class BindBufferTarget(val int: Int)
object BindBufferTarget{
  object GL_ARRAY_BUFFER extends BindBufferTarget(GL15.GL_ARRAY_BUFFER)

  implicit def targetToInt: BindBufferTarget => Int = _.int
}

abstract class DataStoreUsagePattern(val int: Int)
object DataStoreUsagePattern{
  object GL_STATIC_DRAW extends DataStoreUsagePattern(GL15.GL_STATIC_DRAW)

  implicit def dataStoreUsagePatternToInt: DataStoreUsagePattern => Int = _.int
}

abstract class VertexType(val int: Int)
object VertexType{
  object GL_FLOAT extends VertexType(GL11.GL_FLOAT)

  implicit def vertexTypeToInt: VertexType => Int = _.int
}

abstract class DrawMode(val int: Int )
object DrawMode{
  object GL_QUADS extends DrawMode(GL11.GL_QUADS)
  object GL_QUAD_STRIP extends DrawMode(GL11.GL_QUAD_STRIP)
  object GL_TRIANGLES extends DrawMode(GL11.GL_TRIANGLES)
  object GL_TRIANGLE_STRIP extends DrawMode(GL11.GL_TRIANGLE_STRIP)

  implicit def clientStateToInt: DrawMode => Int = _.int
}

abstract class ClientState(val int: Int)
object ClientState{
  object GL_VERTEX_ARRAY extends ClientState(GL11.GL_VERTEX_ARRAY)
  object GL_TEXTURE_COORD_ARRAY extends ClientState(GL11.GL_TEXTURE_COORD_ARRAY)
  object GL_COLOR_ARRAY extends ClientState(GL11.GL_COLOR_ARRAY)

  implicit def clientStateToInt: ClientState => Int = _.int
}

case class BoundBuffer(override val vertices: Seq[Vector3f], id: BufferId, target: BindBufferTarget, storage: DataStoreUsagePattern) extends FloatBuff(vertices){
  // init
  bind()
  GL15.glBufferData(target, buffer, storage)
  unbind()

  def bind(){ GL15.glBindBuffer(target, id) }
  def unbind(){ GL15.glBindBuffer(target, 0) }

}

