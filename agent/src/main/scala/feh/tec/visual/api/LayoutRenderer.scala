package feh.tec.visual.api

import feh.tec.util.{LiftWrapper, Lifted}

trait LayoutRenderer[L <: Layout[E], E <: Easel with EaselAffineTransforms] extends Renderer[L, E]

case class Layout[E <: Easel with EaselAffineTransforms](elems: List[LiftedOptionLayoutElem[_, E]]){
  def render(implicit easel: E) = elems.foreach{e =>
    easel.withTransform(easel.Offset(e.where.asInstanceOf[easel.Coordinate])){
      e.render
    }
  }
}

class LiftedOptionLayoutElem[T, E <: Easel](val lWhatOpt: Lifted[Option[T]], val where: E#Coordinate)(implicit val renderer: Renderer[T, E]){
  def render(implicit easel: E) = lWhatOpt().foreach(renderer.render)
}
object LiftedOptionLayoutElem{
  def apply[T, E <: Easel](lWhatOpt: Lifted[Option[T]], where: E#Coordinate)(implicit renderer: Renderer[T, E]) = new LiftedOptionLayoutElem(lWhatOpt, where)
}

class LiftedLayoutElem[T, E <: Easel](val lWhat: Lifted[T], _where: E#Coordinate)(implicit _renderer: Renderer[T, E])
  extends LiftedOptionLayoutElem[T, E](() => Option(lWhat()), _where)

case class LayoutElem[T, E <: Easel](what: T, override val where: E#Coordinate)(override implicit val renderer: Renderer[T, E]) extends LiftedLayoutElem[T, E](what.lifted, where)
