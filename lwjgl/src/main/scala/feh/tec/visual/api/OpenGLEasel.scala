package feh.tec.visual.api

import org.lwjgl.util.vector.{Vector3f, Vector2f}

trait OpenGLEasel {
  self: Easel =>

  def withoutTextures[R](r: => R): R

  type GlVector

  def drawLine(start: GlVector, end: GlVector): DrawOp

}

trait OpenGL2DFloatEasel extends OpenGLEasel{
  self: Easel =>

  type GlVector = Vector2f
}

trait OpenGL3DFloatEasel extends OpenGLEasel{
  self: Easel =>

  type GlVector = Vector3f
}