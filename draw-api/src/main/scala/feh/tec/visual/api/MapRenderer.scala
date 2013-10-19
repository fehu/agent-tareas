package feh.tec.visual.api

import feh.tec.map.AbstractMap
import feh.tec.map.tile.AbstractTile

trait MapRenderer[Map <: AbstractMap[Tile , Coordinate], Tile <: AbstractTile[Tile, Coordinate], Coordinate, E <: Easel] extends Renderer[Map, E]{
  def render(map: Map, how: E#MDrawOptions)(implicit easel: E)
  def render(t: Map)(implicit easel: E): Unit = render(t, renderOptions)

  def renderOptions: E#MDrawOptions
}
