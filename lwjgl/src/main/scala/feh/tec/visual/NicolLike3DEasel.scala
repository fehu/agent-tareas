package feh.tec.visual

import feh.tec.visual.api._
import feh.tec.util.PipeWrapper
import feh.tec.visual.api.CubeDrawOptions
import feh.tec.visual.api.CubicWorldDrawOptions
import org.lwjgl.opengl.GL11
import nicol.opengl.GLUtils
import org.lwjgl.util.vector.Vector3f
import feh.tec.test.math.VectorUtils

class NicolLike3DEasel extends Easel3DFloat with EaselAffineTransforms with NicolLikeOpenGLEasel with OpenGL3DFloatEasel{
  type TDrawOptions = CubeDrawOptions[NicolLike3DEasel]
  type MDrawOptions = CubicWorldDrawOptions[NicolLike3DEasel]
  type StrDrawOptions = StringDrawOptions[NicolLike3DEasel]

  protected def vertex(f: Coordinate) = (GL11.glVertex3f _).tupled(f)

  def drawLine(start: Coordinate, end: Coordinate): DrawOp = GLUtils.draw(GLUtils.Lines) {
    vertex(start)
    vertex(end)
  }.toDrawOp

  def drawLine(start: GlVector, end: GlVector): DrawOp = drawLine(start.toTuple, end.toTuple)

  def drawString(what: String, where: Coordinate, how: StrDrawOptions): DrawOp = ???

  protected def coordinate_+[N: Numeric](c: Easel#Coordinate, n: N): Coordinate = mapCoordinate(c){ x =>
    val d = implicitly[Numeric[N]].toFloat(n)
    (x._1 + d, x._2 + d, x._3 + d)
  }

  protected def coordinateSum(c1: Coordinate, c2: Coordinate): Coordinate =
    (c1._1 + c2._1, c1._2 + c2._2, c1._3 + c2._3)

  def onMouseMove(f: PartialFunction[Coordinate, Unit]) {}

  def size: Coordinate = ???

  def center: Coordinate = (size._1/2, size._2/2, size._3/2)

  // todo: more rotations
  def withAffineTransform[R](tr: AffineTransform*)(f: => R): R = tr map {
    case Offset((x, y, z)) => () => GL11.glTranslatef(x, y, z)
//    case Rotate(angle) => () => rotate(angle)
//    case Scale(factor) => () => scale(factor)
  } pipe applyTransforms(f)

  def projectionEasel: Easel2DFloat with EaselAffineTransforms with OpenGLEasel =
    new NicolLike2DEasel
}