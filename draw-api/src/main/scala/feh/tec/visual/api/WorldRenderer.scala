package feh.tec.visual.api

import feh.tec.world.{WorldAtom, AbstractWorld}

trait WorldRenderer[Map <: AbstractWorld[Tile , Coordinate], Tile <: WorldAtom[Tile, Coordinate], Coordinate, E <: Easel] extends Renderer[Map, E]{
  def render(map: Map, how: E#MDrawOptions)(implicit easel: E)
  def render(t: Map)(implicit easel: E): Unit = render(t, renderOptions)

  def renderOptions: E#MDrawOptions
}
