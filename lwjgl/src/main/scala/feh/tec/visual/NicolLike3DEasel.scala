package feh.tec.visual

import feh.tec.visual.api.{OpenGLEasel, EaselAffineTransforms, Easel3DFloat, Easel}
import java.awt.Color

class NicolLike3DEasel extends Easel3DFloat with EaselAffineTransforms with OpenGLEasel{
  type TDrawOptions = this.type
  type MDrawOptions = this.type
  type StrDrawOptions = this.type
  type Repr = this.type

  def repr: NicolLike3DEasel#Repr = ???

  def withColor[R](color: Color)(f: => R): R = ???

  def withTransform[R](tr: NicolLike3DEasel#Transform*)(f: => R): R = ???

  def drawLine(start: NicolLike3DEasel#Coordinate, end: NicolLike3DEasel#Coordinate): NicolLike3DEasel#DrawOp = ???

  def drawRect(bottomLeft: NicolLike3DEasel#Coordinate, topRight: NicolLike3DEasel#Coordinate): NicolLike3DEasel#DrawOp = ???

  def drawString(what: String, where: NicolLike3DEasel#Coordinate, how: NicolLike3DEasel#StrDrawOptions): NicolLike3DEasel#DrawOp = ???

  protected def coordinate_+[N: Numeric](c: Easel#Coordinate, n: N): NicolLike3DEasel#Coordinate = ???

  protected def coordinateSum(c1: NicolLike3DEasel#Coordinate, c2: NicolLike3DEasel#Coordinate): NicolLike3DEasel#Coordinate = ???

  def onMouseMove(f: PartialFunction[NicolLike3DEasel#Coordinate, Unit]) {}

  def size: NicolLike3DEasel#Coordinate = ???

  def center: NicolLike3DEasel#Coordinate = ???

  def drawCube(): NicolLike3DEasel#DrawOp = ???

  def withAffineTransform[R](tr: NicolLike3DEasel#AffineTransform*)(f: => R): R = ???

  def withoutTextures[R](r: => R): R = ???
}
