package feh.tec.test

import org.lwjgl.util.vector.{Vector3f, Matrix4f}
import org.lwjgl.opengl.GL11
import org.lwjgl.util.glu.GLU
import org.lwjgl.BufferUtils
import feh.tec.test.math.VectorUtils

/**
 * Taken from http://www.java-gaming.org/topics/lwjgl-tutorial-series-creating-a-3d-camera/30871/view.html
 */

class Camera3DFloat(initPosition: Vector3f,
                    initRotation: Vector3f,
                    initFov: Float,
                    initAspect: Float,
                    initZNear: Float,
                    initZFar: Float)
  extends VectorUtils
{
  // Field Of View
  protected var fov = initFov
  // Aspect Ratio
  protected var aspect = initAspect
  // Near Plane
  protected var zNear = initZNear
  // Far Plane
  protected var zFar = initZFar

  // Projection matrix
  val projection: Matrix4f = createProjection
  // View matrix
  val view: Matrix4f = createView

  // Camera position
  protected val position = initPosition
  // Camera rotation
  protected val rotation = initRotation

  // Vectors for axes
  protected val xAxis = new Vector3f(1, 0, 0)
  protected val yAxis = new Vector3f(0, 1, 0)
  protected val zAxis = new Vector3f(0, 0, 1)

  def createProjection = MatrixUtil.createPerspectiveProjection(fov, aspect, zNear, zFar)
  def createView = MatrixUtil.createIdentityMatrix

  def setPosition(pos: (Float, Float, Float)){
    updateVector(position, pos, _._2)
  }

  def rotate(angl: (Float, Float, Float)){
    updateVector(rotation, angl)(_ + _)
  }


  import scala.math._

  def apply()
  {
    // Make the view matrix an identity.
    view.setIdentity()

    // Rotate the view
    Matrix4f.rotate(toRadians(rotation.x).toFloat, xAxis, view, view)
    Matrix4f.rotate(toRadians(rotation.y).toFloat, yAxis, view, view)
    Matrix4f.rotate(toRadians(rotation.z).toFloat, zAxis, view, view)

    // Move the camera
    Matrix4f.translate(position, view, view)
  }

  def move(amount: Float, direction: Float)
  {
    val p = (position.x, position.y, position.z)
    position.z += amount * sin(toRadians(rotation.y + 90 * direction)).toFloat
    position.x += amount * cos(toRadians(rotation.y + 90 * direction)).toFloat
    println(s"move: $p -> $position")
  }
}

/**
 * Taken from http://www.java-gaming.org/topics/lwjgl-tutorial-series-creating-a-3d-camera/30871/view.html
 */
object MatrixUtil{
  def createPerspectiveProjection(fov: Float, aspect: Float, zNear: Float, zFar: Float) =
  {
    val mat = new Matrix4f()
    import scala.math._
    
    val yScale = 1 / tan(toRadians(fov / 2f)).toFloat
    val xScale = yScale / aspect
    val frustumLength = zFar - zNear

    mat.m00 = xScale
    mat.m11 = yScale
    mat.m22 = -((zFar + zNear) / frustumLength)
    mat.m23 = -1
    mat.m32 = -((2 * zFar * zNear) / frustumLength)
    mat.m33 = 0

    mat
  }

  def createIdentityMatrix = {
    val mat = new Matrix4f()

    mat.m00 = 1
    mat.m11 = 1
    mat.m22 = 1
    mat.m33 = 1

    mat
  }

  implicit class MatrixToFloatBufferWrapper(mat: Matrix4f){
    def toFloatBuffer = {
      val buffer = BufferUtils.createFloatBuffer(16)
      mat.store(buffer)
      buffer.flip()
      buffer
    }

  }
}