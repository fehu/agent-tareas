package feh.tec.visual

import java.awt.Color
import nicol.opengl.GLUtils
import GLUtils._
import feh.tec.visual.api._
import scala.collection.mutable
import feh.tec.visual.helpers.ColorHelpers._
import org.lwjgl.opengl.{Display, GL11}
import org.lwjgl.BufferUtils

/*
trait NicolLikeTileRenderer[Tile <: AbstractTile[Tile, Coordinate], Coordinate] extends TileRenderer[Tile, Coordinate]{
  
}
*/

/**
 * some parts are taken from Tom Streller's (scan) Nicol-0.1.2 project (https://github.com/scan/Nicol)
 * due to a lack of scala 2.10 compatible version
 */
class NicolLike2DEasel extends Easel with Easel2DFloat{
  type TDrawOptions = SquareTileDrawOptions[NicolLike2DEasel] //todo
  type StrDrawOptions = StringDrawOptions[NicolLike2DEasel]
  type MDrawOptions = SquareMapDrawOptions[NicolLike2DEasel]

  type Repr = GLUtils.type
  def repr: Repr = GLUtils 

  protected val colorHolder = new ThreadLocal[Color]{
    override def initialValue(): Color = Color.black
  }
  
  protected def setColor(c: Color) = colour(c.getRed, c.getGreen, c.getBlue, c.getAlpha)
  
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

  private implicit class AnyToDrawOp(a: Any) {
    def toDrawOp = new DrawOp{}
  }

  def drawLine(start: Coordinate, end: Coordinate): DrawOp = GLUtils.draw(Lines) {
    vertex(start)
    vertex(end)
  }.toDrawOp

  def drawRect(_bottomLeft: Easel#Coordinate, _topRight: Easel#Coordinate): DrawOp = GLUtils.draw(LineLoop) {
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
//    mapCoordinate(_where){ where =>
//      mapStrOps(_how){ how =>
        val font = getFont(how.font, how.size.toInt)
        how.alignment match{
          case StringAlignment.Left =>
            font.write(what, where, how.color.toFloatRgb, how.rotation)
          case StringAlignment.Center =>
            font.write(what, (where._1 - font.stringWidth(what) / 2, where._2 - font.height / 2), how.color.toFloatRgb, how.rotation)
          case StringAlignment.Right =>
            font.write(what, (where._1 - font.stringWidth(what), where._2), how.color.toFloatRgb, how.rotation)
        }
//      }
    }.toDrawOp

  protected def mapCoordinate[R](c: Easel#Coordinate)(f: Coordinate => R): R = c match {
    case x: Coordinate => f(x)
  }
//  protected def mapCoordinate2d[R](c: Easel2D#Coordinate)(f: Coordinate => R): R = c match {
//    case x: Coordinate => f(x)
//  }
  protected def mapCoordinates[R](c1: Easel#Coordinate, c2: Easel#Coordinate)(f: (Coordinate, Coordinate) => R): R = c1 -> c2 match {
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
}