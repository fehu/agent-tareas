package feh.tec.test

/**
 *
 * Using http://www.java-gaming.org/topics/lwjgl-tutorial-series-contents/30681/view.html as an example
 *
 */

import nicol._
import nicol.input.Key._
import feh.tec.util.Lifted
import org.lwjgl.opengl.{Display, GL11, GL15}
import org.lwjgl.util.vector.Vector3f
import org.lwjgl.BufferUtils
import java.nio.{IntBuffer, FloatBuffer}
import feh.tec.test.GLType.GL_FLOAT
import feh.tec.test.ClientState._
import feh.tec.test.DrawMode.{GL_QUAD_STRIP, GL_TRIANGLE_STRIP, GL_TRIANGLES, GL_QUADS}
import feh.tec.test.BindBufferTarget.GL_ARRAY_BUFFER
import feh.tec.test.DataStoreUsagePattern.GL_STATIC_DRAW
import nicol.opengl.GLUtils
import nicol.opengl.GLUtils.Quads
import scala.io.Source

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

    lazy val colors: Seq[(Coord, Coord, Coord, Coord)] =

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

  lazy val cubeVbo2 = vbo(cube.asVectors(_.sides), cube.asVectors(_.colors))
  cube.sides.foreach(println)
  lazy val cubeVbo = ObjVertices.Cube.vbo

  lazy val pyramidVbo = ObjVertices.Pyramid.vbo

  def render(){
    cubeVbo2.render
  }

  lazy val camera = new Camera3DFloat(new Vector3f(0, 0, 0), new Vector3f(0, 0, 0), 71 /*45*/, Display.getWidth / Display.getHeight, 0.1f, 100)

  lazy val shaderProgram = new ShaderProgram(
    Source.fromURL(ClassLoader.getSystemResource("lwjgl/test/shader.vert")),
    Source.fromURL(ClassLoader.getSystemResource("lwjgl/test/shader.frag"))
  ).linked()


  def update: Option[Scene] = {
    sync
    showFPS

//    render()

    camera.apply()

    shaderProgram.bind()

    shaderProgram.setUniform("projection", camera.projection)
    shaderProgram.setUniform("view", camera.view)

    GL11.glClear(GL11.GL_COLOR_BUFFER_BIT | GL11.GL_DEPTH_BUFFER_BIT);

    // Bind the vertex VBO
    cubeVbo.vertexBuffer.bind
    GL11.glVertexPointer(3, GL_FLOAT, 0, 0)

    // Bind the color VBO
    cubeVbo.colorBuffer.bind
    GL11.glColorPointer(3, GL_FLOAT, 0, 0)

    // Draw the cube with triangle strip
    GL11.glDrawArrays(GL_TRIANGLE_STRIP, 0, 24);

    // Unbind the shaders
    shaderProgram.unbind();

    keyEvent {
      e =>
        e released {
          case _ =>
        }
        e pressed {
          case "escape" =>
            shaderProgram.dispose()
            cubeVbo.dispose()
            exitScene()
          case "space" => pauseScene(this)
        }
    }
  }

}

class DrawCubeGame extends Game(Init3D("Draw Cube Test") >> new DrawCube(() => null, _ => null))

object DrawCubeApp extends App{
  val game = new DrawCubeGame
  game.start
}

//object DrawCube{
//  def drawCube()
//}

case class VBO(vertexBuffer: BoundFloat3DBuffer, colorBuffer: BoundFloat3DBuffer){
  def render(){

    // Clean both color and depth buffers
    GL11.glClear(GL11.GL_COLOR_BUFFER_BIT | GL11.GL_DEPTH_BUFFER_BIT);

    // Translate into the view
    GL11.glTranslatef(0, 0, -5);

    // Rotate on both x and y axes
    GL11.glRotatef(1, 1, 1, 0);

    // Bind the vertex VBO
    GL11.glEnableClientState(GL_VERTEX_ARRAY)
    vertexBuffer.bind()
    GL11.glVertexPointer(3, GL_FLOAT, 12, 0);

    // Bind the color VBO
    GL11.glEnableClientState(GL_COLOR_ARRAY)
    colorBuffer.bind()
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
//    colorBuffer.bind()
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
    GL15.glDeleteBuffers(colorBuffer.id)
  }
}

object VBO{
  def apply(target: BindBufferTarget, storage: DataStoreUsagePattern, id: BufferId = BufferId())
           (vertices: Seq[Vector3f], textureCoords: Seq[Vector3f]): VBO = {
    val verticesBuffer = Float3DBuff(vertices: _*).bind(target, storage)
    val texturesBuffer = Float3DBuff(textureCoords: _*).bind(target, storage)
    new VBO(verticesBuffer, texturesBuffer)
  }

}

case class BufferId(id: Int = GL15.glGenBuffers)
object BufferId{
  implicit def bufferIdToInt: BufferId => Int = _.id
}

class Float3DBuff(val data: Seq[Vector3f]){
  lazy val buffer = {
    val buff = BufferUtils.createFloatBuffer(data.length * 3)
    data.foreach(_.store(buff))
//    buff.rewind()
    buff.flip
    buff
  }

  def size = data.length * 3

  def bind(target: BindBufferTarget, storage: DataStoreUsagePattern, id: BufferId = BufferId()) =
    BoundFloat3DBuffer(data, id, target, storage)
}

object Float3DBuff{
  def apply(vertices: Vector3f*): Float3DBuff = new Float3DBuff(vertices)
  def apply(vertices: Array[(Float, Float, Float)]): Float3DBuff = new Float3DBuff(vertices.map(c => new Vector3f(c._1, c._2, c._3)))
  def unapply(buf: Float3DBuff): Option[(Seq[Vector3f], FloatBuffer)] = Some(buf.data -> buf.buffer)

  implicit def vertexBufferToFloatBuffer = (_: Float3DBuff).buffer
}

class IntBuff(val data: Seq[Int]){
  lazy val buffer = {
    val buff = BufferUtils.createIntBuffer(data.length * 3)
    data.foreach(buff.put)
    buff.flip
    buff
  }

  def size = data.size

  def bind(target: BindBufferTarget, storage: DataStoreUsagePattern, id: BufferId = BufferId()) =
    BoundIntBuffer(data, id, target, storage)
}

object IntBuff{
  def apply(data: Seq[Int]): IntBuff = new IntBuff(data)
  def unapply(buf: IntBuff): Option[(Seq[Int], IntBuffer)] = Some(buf.data -> buf.buffer)
}

abstract class BindBufferTarget(val int: Int)
object BindBufferTarget{
  object GL_ARRAY_BUFFER extends BindBufferTarget(GL15.GL_ARRAY_BUFFER)
  object GL_ELEMENT_ARRAY_BUFFER extends BindBufferTarget(GL15.GL_ELEMENT_ARRAY_BUFFER)

  implicit def targetToInt: BindBufferTarget => Int = _.int
}

/**
 * GL_(STATIC | STREAM | DYNAMIC)_(DRAW | READ | COPY)
 *
 * STATIC - The data store contents will be modified once and used many times.
 * STREAM - The data store contents will be modified once and used at most a few times.
 * DYNAMIC - The data store contents will be modified repeatedly and used many times.
 * DRAW - The data store contents are modified by the application, and used as the source for GL drawing and image specification commands.
 * READ - The data store contents are modified by reading data from the GL, and used to return that data when queried by the application.
 * COPY - The data store contents are modified by reading data from the GL, and used as the source for GL drawing and image specification commands.
 */

abstract class DataStoreUsagePattern(val int: Int)
object DataStoreUsagePattern{
  object GL_STATIC_DRAW extends DataStoreUsagePattern(GL15.GL_STATIC_DRAW)

  implicit def dataStoreUsagePatternToInt: DataStoreUsagePattern => Int = _.int
}

abstract class GLType(val int: Int)
object GLType{
  object GL_FLOAT extends GLType(GL11.GL_FLOAT)
  object GL_UNSIGNED_INT extends GLType(GL11.GL_UNSIGNED_INT)


  implicit def vertexTypeToInt: GLType => Int = _.int
}

abstract class DrawMode(val int: Int )
object DrawMode{
  object GL_QUADS extends DrawMode(GL11.GL_QUADS)
  object GL_QUAD_STRIP extends DrawMode(GL11.GL_QUAD_STRIP)
  object GL_TRIANGLES extends DrawMode(GL11.GL_TRIANGLES)
  object GL_TRIANGLE_STRIP extends DrawMode(GL11.GL_TRIANGLE_STRIP)
  object GL_LINE extends DrawMode(GL11.GL_LINE)
  object GL_LINE_LOOP extends DrawMode(GL11.GL_LINE_LOOP)

  implicit def clientStateToInt: DrawMode => Int = _.int
}

abstract class ClientState(val int: Int)
object ClientState{
  object GL_VERTEX_ARRAY extends ClientState(GL11.GL_VERTEX_ARRAY)
  object GL_TEXTURE_COORD_ARRAY extends ClientState(GL11.GL_TEXTURE_COORD_ARRAY)
  object GL_COLOR_ARRAY extends ClientState(GL11.GL_COLOR_ARRAY)

  implicit def clientStateToInt: ClientState => Int = _.int
}

trait BoundBuffer{
  def id: BufferId
  def target: BindBufferTarget
  def storage: DataStoreUsagePattern

  def bind(){ GL15.glBindBuffer(target, id) }
  def unbind(){ GL15.glBindBuffer(target, 0) }

  def init(f: => Unit){
    bind()
    f
    unbind()
  }
}

case class BoundFloat3DBuffer(override val data: Seq[Vector3f], id: BufferId, target: BindBufferTarget, storage: DataStoreUsagePattern) extends Float3DBuff(null) with BoundBuffer{
  init(GL15.glBufferData(target, buffer, storage))
}

case class BoundIntBuffer(override val data: Seq[Int], id: BufferId, target: BindBufferTarget, storage: DataStoreUsagePattern) extends IntBuff(null) with BoundBuffer{
  init(GL15.glBufferData(target, buffer, storage))
}
