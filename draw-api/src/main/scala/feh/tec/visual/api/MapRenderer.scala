package feh.tec.visual.api

import feh.tec.map.AbstractMap
import feh.tec.map.tile.AbstractTile

trait MapRenderer[Map <: AbstractMap[Tile , Coordinate], Tile <: AbstractTile[Tile, Coordinate], Coordinate, E <: Easel] {
  def render(map: Map, how: E#MDrawOptions)(implicit easel: E)
}
