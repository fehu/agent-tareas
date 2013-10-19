package feh.tec.visual.api

trait LayoutRenderer[L <: Layout[E], E <: Easel] extends Renderer[L, E]

case class Layout[E <: Easel](elems: List[LayoutElem[_, E]]){
  def render(implicit easel: E) = elems.foreach{case e@LayoutElem(x, p) => e.renderer.render(e.what)}
}

case class LayoutElem[T, E <: Easel](what: T, where: E#Coordinate)(implicit val renderer: Renderer[T, E]) // todo: transforms
