package feh.tec.visual

import java.awt.Color
import nicol.opengl.GLUtils
import GLUtils._
import feh.tec.visual.api._
import scala.collection.mutable
import feh.tec.visual.helpers.ColorHelpers._
import org.lwjgl.opengl.{Display, GL11}
import org.lwjgl.BufferUtils
import nicol.input.Mouse
import feh.tec.util._
import nicol.opengl.GLUtils

/*
trait NicolLikeTileRenderer[Tile <: AbstractTile[Tile, Coordinate], Coordinate] extends TileRenderer[Tile, Coordinate]{
  
}
*/

/**
 * some parts are taken from Tom Streller's (scan) Nicol-0.1.2 project (https://github.com/scan/Nicol)
 * due to a lack of scala 2.10 compatible version
 */
class NicolLike2DEasel extends Easel with Easel2DFloat with EaselAffineTransforms with OpenGLEasel{
  easel =>

  type TDrawOptions = SquareTileDrawOptions[NicolLike2DEasel]
  type StrDrawOptions = StringDrawOptions[NicolLike2DEasel]
  type MDrawOptions = SquareMapDrawOptions[NicolLike2DEasel]

  type Repr = GLUtils.type
  def repr: Repr = GLUtils 

  protected val colorHolder = new ThreadLocal[Color]{
    override def initialValue(): Color = Color.black
  }
  
  protected def setColor(c: Color) = colour(c.getRed, c.getGreen, c.getBlue, c.getAlpha)


  def size: Coordinate = Display.getWidth.toFloat -> Display.getHeight.toFloat
  def center: Coordinate = size._1/2 -> size._2/2

  def withColor[R](color: Color)(f: => R): R = {
    val old = colorHolder.get()
    if(old != color) {
      colorHolder.set(color)
      setColor(color)  
    }
    val res = f
    if(old != color){
      colorHolder.set(old)
      setColor(old)  
    }
    res
  }

  def withoutTextures[R](r: => R): R = {
    import GL11._
    glDisable(GL_TEXTURE_2D)
    val x = r
    glEnable(GL_TEXTURE_2D)
    x
  }

  implicit class WithoutTextures[R](r: => R) {
    def withoutTextures = easel.withoutTextures(r)
  }

  def withTransform[R](tr: Transform*)(f: => R): R = {
    withAffineTransform(tr.flatMap(_.tryAs[AffineTransform]): _*)(f)
  }

  def withAffineTransform[R](tr: AffineTransform*)(f: => R): R = tr map {
    case Offset((x, y)) => () => translate(x, y)
    case Rotate(angle) => () => rotate(angle)
    case Scale(factor) => () => scale(factor)
  } pipe {
    transforms =>
      var x: R = null.asInstanceOf[R] // que horror
      preserve(/*GLUtils.withoutTextures*/ {
        transforms.foreach(_())
        x = f
      })
      x
  }


  private implicit class AnyToDrawOp(a: Any) {
    def toDrawOp = new DrawOp{}
  }

  def drawLine(start: Coordinate, end: Coordinate): DrawOp = GLUtils.draw(Lines) {
    vertex(start)
    vertex(end)
  }.toDrawOp

  def drawRect(_bottomLeft: Coordinate, _topRight: Coordinate): DrawOp = GLUtils.draw(LineLoop) {
    mapCoordinates(_bottomLeft, _topRight){ (bottomLeft, topRight) =>
      vertex(bottomLeft)
      vertex(bottomLeft._1, topRight._2)
      vertex(topRight)
      vertex(topRight._1, bottomLeft._2)
    }
  }.toDrawOp

  def drawRect(_bottomLeft: Easel2D#Coordinate, _width: Easel2D#CoordinateUnit, _height: Easel2D#CoordinateUnit): DrawOp =
    mapCoordinate(_bottomLeft){ bottomLeft =>
      mapCoordinateUnits(_width, _height){ (width, height) =>
        val topRight = (bottomLeft._1 + width, bottomLeft._2 + height)
        GLUtils.draw(LineLoop) {
          vertex(bottomLeft)
          vertex(bottomLeft._1, topRight._2)
          vertex(topRight)
          vertex(topRight._1, bottomLeft._2)
        }
      }
  }.toDrawOp

  protected val Fonts = mutable.Map.empty[(String, Int), nicol.font.Font]

  def getFont(fontName: String, size: Int) = Fonts get fontName -> size getOrElse {
    val nfont = nicol.font.Font(fontName, size)
    Fonts += fontName -> size -> nfont
    nfont
  }

  def drawString(what: String, where: Coordinate, how: StrDrawOptions): DrawOp = {
        val font = getFont(how.font, how.size.toInt)
        val splitted = what split '\n'
        lazy val offset = how.size + how.vSpacing
        def write(center: Coordinate) = splitted.zipWithIndex.map{
          case (s, i) => font.write(s, center.ops + (0, offset*i), how.color.toFloatRgb, 0)
        }
        how.alignment match{
          case StringAlignment.Left => write(where)
          case StringAlignment.Center => write(where._1 - font.stringWidth(what) / 2, where._2 - font.height / 2)
          case StringAlignment.Right => write(where._1 - font.stringWidth(what), where._2)
        }
//      }
    }.toDrawOp

  protected def mapCoordinate[R](c: Easel#Coordinate)(f: Coordinate => R): R = c match {
    case x: Coordinate => f(x)
  }

  protected def mapCoordinates[R](c1: Coordinate, c2: Coordinate)(f: (Coordinate, Coordinate) => R): R = c1 -> c2 match {
    case (x: Coordinate, y: Coordinate) => f(x, y)
  }

  protected def mapCoordinateUnits[R](u1: Easel2D#CoordinateUnit, u2: Easel2D#CoordinateUnit)
                                     (f: (CoordinateUnit, CoordinateUnit) => R): R = u1 -> u2 match {
    case (x: CoordinateUnit, y: CoordinateUnit) => f(x, y)
  }

  protected def mapStrOps[R](ops: Easel#StrDrawOptions)(f: StrDrawOptions => R): R = ops match {
    case x: StrDrawOptions => f(x)
  }

  protected def coordinate_+[N: Numeric](c: Easel#Coordinate, n: N): Coordinate = mapCoordinate(c){ x =>
    val d = implicitly[Numeric[N]].toFloat(n)
    (x._1 + d, x._2 + d)
  }

//  /**
//   * http://www.lwjgl.org/wiki/index.php?title=Taking_Screen_Shots
//   */
//  def screenshot: Array[Byte] =
  
  def onMouseMove(f: PartialFunction[Coordinate, Unit]) = f(Mouse.apply)

}

object NicoleLike2dEaselCoordinateOps extends EaselCoordinateOps[NicolLike2DEasel]{
  def zeroCoordinate: NicolLike2DEasel#Coordinate = (0F, 0F)
}