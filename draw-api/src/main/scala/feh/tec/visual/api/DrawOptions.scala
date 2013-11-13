package feh.tec.visual.api

import java.awt.Color

// // // // // // // // // // // // // // // // //  String Draw Options  // // // // // // // // // // // // // // // // //

trait StringDrawOptions[+E <: Easel]
{
  def font: String
  def size: E#CoordinateUnit
  def color: Color
  def alignment: StringAlignment
  def vSpacing: E#CoordinateUnit
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
                                            size: E#CoordinateUnit,
                                            vSpacing: E#CoordinateUnit
                                            ) extends StringDrawOptions[E]

// // // // // // // // // // // // // // // // //  Atom Draw Options  // // // // // // // // // // // // // // // // //

trait AtomDrawOptions[+E <: Easel]

trait SquareTileDrawOptions[E <: Easel] extends AtomDrawOptions[E]{
  def sideSize: E#CoordinateUnit
  def lineColor: Color
  def fillColor: Option[Color] // todo
}


case class BasicSquareTileDrawOptions[E <: Easel](sideSize: E#CoordinateUnit, lineColor: Color, fillColor: Option[Color]) extends SquareTileDrawOptions[E]

case class CubeDrawOptions[E <: Easel](sideSize: E#CoordinateUnit, lineColor: Color, fillColor: Option[Color]) extends AtomDrawOptions[E]

// // // // // // // // // // // // // // // // //  World Draw Options  // // // // // // // // // // // // // // // // //

trait WorldDrawOptions[+E <: Easel]

trait SquareMapDrawOptions[E <: Easel] extends WorldDrawOptions[E]{
  def tileSideSize: E#CoordinateUnit
  def showLabels: Boolean
}

case class BasicSquareMapDrawOptions[E <: Easel](tileSideSize: E#CoordinateUnit, showLabels: Boolean) extends SquareMapDrawOptions[E]

object BasicSquareMapDrawOptions{
  def apply[E <: Easel](n: Int, showLabels: Boolean)(implicit easel: E): BasicSquareMapDrawOptions[E] = BasicSquareMapDrawOptions(easel.unitNumeric.fromInt(n), showLabels)
}

case class CubicWorldDrawOptions[E <: Easel](cubeSideSize: E#CoordinateUnit, showLabels: Boolean) extends WorldDrawOptions[E]