package feh.tec.visual.api

import feh.tec.map.tile.AbstractTile


trait TileRenderer[Tile <: AbstractTile[Tile, TileCoordinate], TileCoordinate] {
  type E <: Easel

  def draw(tile: Tile, where: E#Coordinate, how: E#TDrawOptions)(implicit easel: E)
}

