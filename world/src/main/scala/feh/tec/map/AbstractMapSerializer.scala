package feh.tec.map

import feh.tec.map.tile.AbstractTile

trait AbstractMapSerializer[Map <: AbstractMap[Tile , Coordinate], Tile <: AbstractTile[Tile, Coordinate], Coordinate] {
  type Serialized
  def serialize(map: Map): Serialized
  def toMap(ser: Serialized): Map
}
