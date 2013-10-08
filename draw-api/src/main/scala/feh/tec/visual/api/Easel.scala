package feh.tec.visual.api

import feh.tec.map.tile.SquareTile
import java.awt.{Font, Color}

trait Easel{ easel =>
  type Coordinate <: Product
  type CoordinateUnit
  type TDrawOptions <: TileDrawOptions[Easel]
  type MDrawOptions <: MapDrawOptions[Easel]
  type StrDrawOptions <: StringDrawOptions[Easel]

  implicit def unitNumeric: Numeric[CoordinateUnit]

  type Repr
  def repr: Repr

  def withColor[R](color: java.awt.Color)(f: => R): R

  trait DrawOp

  def drawLine(start: Coordinate, end: Coordinate): DrawOp
  def drawRect(bottomLeft: Easel#Coordinate, topRight: Easel#Coordinate): DrawOp
  def drawString(what: String, where: Coordinate, how: StrDrawOptions): DrawOp

  implicit class DrawOpWrapper(op: => Easel#DrawOp) {
    def withColor[R](color: java.awt.Color)(f: => R) = easel.withColor(color)(f)
  }

  implicit class CoordinateOps(c: Easel#Coordinate){
    def ops = new {
      /*
      * + to every element of vector
      */
      def +[N : Numeric](n: N): Coordinate = coordinate_+(c, n)
    }
  }

//  def buildTDrawOptions[Ops <: TileDrawOptions[_ <: Easel]](ops: Ops): TDrawOptions
//  def buildStrDrawOptions[Ops <: StringDrawOptions[_ <: Easel]](ops: Ops): StrDrawOptions

  protected def coordinate_+[N : Numeric](c: Easel#Coordinate, n: N): Coordinate

//  def screenshot: Array[Byte]
}

trait Easel2D extends Easel{
  def drawRect(bottomLeft: Easel2D#Coordinate, width: Easel2D#CoordinateUnit, height: Easel2D#CoordinateUnit): DrawOp
}

trait Easel2DFloat extends Easel2D{
  type Coordinate = (Float, Float)
  type CoordinateUnit = Float

  implicit def unitNumeric: Numeric[Easel2DFloat#CoordinateUnit] = Numeric.FloatIsFractional
}

trait TileDrawOptions[+E <: Easel]

trait SquareTileDrawOptions[E <: Easel] extends TileDrawOptions[E]{
  def sideSize: E#CoordinateUnit
  def lineColor: Color
  def fillColor: Option[Color] // todo
}

case class BasicSquareTileDrawOptions[E <: Easel](sideSize: E#CoordinateUnit, lineColor: Color, fillColor: Option[Color])
  extends SquareTileDrawOptions[E]

trait StringDrawOptions[+E <: Easel]
{
  def font: String
  def size: E#CoordinateUnit
  def color: Color
  def alignment: StringAlignment
  def rotation: E#CoordinateUnit // ??
}

trait StringAlignment
object StringAlignment{
  case object Left extends StringAlignment
  case object Center extends StringAlignment
  case object Right extends StringAlignment
}

case class BasicStringDrawOps[+E <: Easel]( alignment: StringAlignment,
                                            color: Color,
                                            font: String,
                                            rotation: E#CoordinateUnit,
                                            size: E#CoordinateUnit
                                           ) extends StringDrawOptions[E]

trait MapDrawOptions[+E <: Easel]

trait SquareMapDrawOptions[E <: Easel] extends MapDrawOptions[E]{
  def tileSideSize: E#CoordinateUnit
  def showLabels: Boolean
}

case class BasicSquareMapDrawOptions[E <: Easel](tileSideSize: E#CoordinateUnit, showLabels: Boolean) extends SquareMapDrawOptions[E]

object BasicSquareMapDrawOptions{
  def apply[E <: Easel](n: Int, showLabels: Boolean)(implicit easel: E): BasicSquareMapDrawOptions[E] = BasicSquareMapDrawOptions(easel.unitNumeric.fromInt(n), showLabels)
}