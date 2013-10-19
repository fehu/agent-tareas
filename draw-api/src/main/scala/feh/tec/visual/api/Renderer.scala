package feh.tec.visual.api

trait Renderer[T, E <: Easel] {
  def render(t: T)(implicit easel: E)
}
