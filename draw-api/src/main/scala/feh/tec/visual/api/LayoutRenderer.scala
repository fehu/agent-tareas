package feh.tec.visual.api

import feh.tec.util.{LiftWrapper, Lifted}

trait LayoutRenderer[L <: Layout[E], E <: Easel with EaselAffineTransforms] extends Renderer[L, E]

case class Layout[E <: Easel with EaselAffineTransforms](elems: List[LiftedLayoutElem[_, E]]){
  def render(implicit easel: E) = elems.foreach{case e: LiftedLayoutElem[_, _] =>
    easel.withTransform(easel.Offset(e.where.asInstanceOf[easel.Coordinate])){
      e.render
    }
  }
}

class LiftedLayoutElem[T, E <: Easel](val lWhat: Lifted[T], val where: E#Coordinate)(implicit val renderer: Renderer[T, E]){ self => // todo: transforms
  def render(implicit easel: E) = renderer.render(lWhat())
}
case class LayoutElem[T, E <: Easel](what: T, override val where: E#Coordinate)(override implicit val renderer: Renderer[T, E]) extends LiftedLayoutElem[T, E](what.lifted, where)
