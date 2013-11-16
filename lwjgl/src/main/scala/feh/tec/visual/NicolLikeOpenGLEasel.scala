package feh.tec.visual

import feh.tec.visual.api.{EaselAffineTransforms, OpenGLEasel, Easel}
import org.lwjgl.opengl.GL11
import java.awt.Color
import nicol.opengl.GLUtils._
import feh.tec.util.TryAsWrapper
import scala.collection.mutable
import org.lwjgl.opengl.GL11._
import feh.tec.test.math.VectorUtils

trait NicolLikeOpenGLEasel extends OpenGLEasel with EaselAffineTransforms with VectorUtils{
  easel: Easel =>

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

  def withTransform[R](tr: Transform*)(f: => R): R = {
    withAffineTransform(tr.flatMap(_.tryAs[AffineTransform]): _*)(f)
  }

  protected def applyTransforms[R](f: => R)(transforms: Seq[() => Unit]) =
    preserving{
      transforms.foreach(_())
      f
    }

  protected implicit class AnyToDrawOp(a: Any) {
    def toDrawOp = new DrawOp{}
  }

  protected val Fonts = mutable.Map.empty[(String, Int), nicol.font.Font]

  def getFont(fontName: String, size: Int) = Fonts get fontName -> size getOrElse {
    val nfont = nicol.font.Font(fontName, size)
    Fonts += fontName -> size -> nfont
    nfont
  }

  def preserving[R](f: => R): R = {
    glPushMatrix
    val r = f
    glPopMatrix
    r
  }

  protected def mapCoordinate[R](c: Easel#Coordinate)(f: Coordinate => R): R = c match {
    case x: Coordinate => f(x)
  }


}