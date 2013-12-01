package feh.tec.visual.api

import java.awt.Color

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
  def withTransform[R](tr: Transform*)(f: => R): R

  trait Transform
  trait DrawOp

  def drawLine(start: easel.Coordinate, end: easel.Coordinate): DrawOp
  def drawRect(bottomLeft: easel.Coordinate, topRight: easel.Coordinate): DrawOp
  def drawString(what: String, where: easel.Coordinate, how: easel.StrDrawOptions): DrawOp
  def drawString(what: String, how: StrDrawOptions): DrawOp = drawString(what, zeroCoordinate, how)

  implicit class DrawOpWrapper(op: => Easel#DrawOp) {
    def withColor[R](color: java.awt.Color)(f: => R) = easel.withColor(color)(f)
  }

  implicit class CoordinateOpsWrapper[C <% Coordinate](c: C){
    def ops = new CoordinateOps(c)
  }

  protected class CoordinateOps(c: Coordinate){
    /*
    * + to every element of vector
    */
    def plusNum[N : Numeric](n: N): Coordinate = coordinate_+(c, n)
    def +(c2: Coordinate): Coordinate = coordinateSum(c, c2)
  }

  protected def coordinate_+[N : Numeric](c: Easel#Coordinate, n: N): Coordinate
  protected def coordinateSum(c1: Coordinate, c2: Coordinate): Coordinate

  def onMouseMove(f: PartialFunction[Coordinate, Unit]): Unit

  def size: Coordinate
  def center: Coordinate
  def zeroCoordinate: Coordinate
}

trait Easel2D extends Easel{
  def drawRect(bottomLeft: Easel2D#Coordinate, width: Easel2D#CoordinateUnit, height: Easel2D#CoordinateUnit): DrawOp
  def coordinate(c1: CoordinateUnit, c2: CoordinateUnit): Coordinate
}

trait Easel2DFloat extends Easel2D{
  type Coordinate = (Float, Float)
  type CoordinateUnit = Float

  implicit def unitNumeric: Numeric[Easel2DFloat#CoordinateUnit] = Numeric.FloatIsFractional

  def zeroCoordinate: Easel2DFloat#Coordinate = (0F, 0F)

  protected def coordinateSum(c1: Coordinate, c2: Coordinate): Coordinate = (c1._1 + c2._1, c1._2 + c2._2)
  def coordinate(c1: CoordinateUnit, c2: CoordinateUnit): Coordinate = c1 -> c2
}

trait Easel3D extends Easel{
  def drawCube(/*todo: bottomLeft: Easel3D#Coordinate, width: Easel3D#CoordinateUnit, height: Easel3D#CoordinateUnit*/): DrawOp
}

trait Easel3DFloat extends Easel3D{
  type Coordinate = (Float, Float, Float)
  type CoordinateUnit = Float

  implicit def unitNumeric: Numeric[Easel3DFloat#CoordinateUnit] = Numeric.FloatIsFractional

  def zeroCoordinate: Easel3DFloat#Coordinate = (0F, 0F, 0F)

  protected def coordinateSum(c1: Coordinate, c2: Coordinate, c3: Coordinate): Coordinate =  (c1._1 + c2._1, c1._2 + c2._2, c1._3 + c2._3)
}


trait EaselCoordinateOps[E <: Easel]{
  def zeroCoordinate: E#Coordinate
}

trait EaselAffineTransforms {
  easel: Easel =>

  trait AffineTransform extends Transform

  object Offset{
    def zero = Offset(zeroCoordinate)
    implicit def toCoordinate(o: Offset) = o.c
  }
  case class Offset(c: Coordinate) extends AffineTransform{
    implicit val num =  easel.unitNumeric.asInstanceOf[Numeric[Coordinate]]
    def +(that: Offset) = Offset(this.c.ops + that.c)
    def +(that: Coordinate) = Offset(this.c.ops + that)
  }
  case class Rotate(c: CoordinateUnit) extends AffineTransform
  case class Scale(f: CoordinateUnit) extends AffineTransform

  def withAffineTransform[R](tr: AffineTransform*)(f: => R): R
}