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

// // // // // // // // // // // // // // // // //  Tile Draw Options  // // // // // // // // // // // // // // // // //

trait TileDrawOptions[+E <: Easel]{
  def drawBorder: Boolean
  def borderColor: Color
  def fillColor: Option[Color]
  // tiles with this flag are rendered after the rendering is done for the rest of tiles
  def delayedRendering: Boolean
}

trait SquareTileDrawOptions[E <: Easel] extends TileDrawOptions[E]{
  def sideSize: E#CoordinateUnit
}


case class BasicSquareTileDrawOptions[E <: Easel](sideSize: E#CoordinateUnit, borderColorOpt: Option[Color], fillColor: Option[Color], delayedRendering: Boolean = false)
  extends SquareTileDrawOptions[E]
{
  def drawBorder: Boolean = borderColorOpt.isDefined
  def borderColor: Color = borderColorOpt.orNull
}


// // // // // // // // // // // // // // // // //  Map Draw Options  // // // // // // // // // // // // // // // // //

trait MapDrawOptions[+E <: Easel]

trait SquareMapDrawOptions[E <: Easel] extends MapDrawOptions[E]{
  def tileSideSize: E#CoordinateUnit
  def showLabels: Boolean
}

case class BasicSquareMapDrawOptions[E <: Easel](tileSideSize: E#CoordinateUnit, showLabels: Boolean) extends SquareMapDrawOptions[E]

object BasicSquareMapDrawOptions{
  def apply[E <: Easel](n: Int, showLabels: Boolean)(implicit easel: E): BasicSquareMapDrawOptions[E] = BasicSquareMapDrawOptions(easel.unitNumeric.fromInt(n), showLabels)
}