package feh.tec.visual.api

trait OpenGLEasel {
  self: Easel =>

  def withoutTextures[R](r: => R): R
}
