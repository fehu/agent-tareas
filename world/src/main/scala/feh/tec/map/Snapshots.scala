package feh.tec.map

import feh.tec.map.tile.AbstractTile

trait TileSnapshot[Tile <: AbstractTile[Tile, Coordinate], Coordinate] extends AbstractTile[Tile, Coordinate]{
  self: Tile =>

  override val coordinate: Coordinate

  override def neighbours: Seq[Tile] = ???

  def neighboursSnapshots: Seq[TileSnapshot[Tile, Coordinate]]
}

trait MapSnapshot[+Map <: AbstractMap[Tile, Coordinate], Tile <: AbstractTile[Tile, Coordinate], Coordinate]
  extends AbstractMap[Tile, Coordinate]
{
  self: Map =>

  val tilesSnapshots: Seq[TileSnapshot[Tile, Coordinate]]

  override val tiles: Seq[Tile] = ???

  override val get: PartialFunction[Coordinate, Tile] = ???

  def getSnapshot: PartialFunction[Coordinate, TileSnapshot[Tile, Coordinate]]
}

trait TileSnapshotBuilder[Tile <: AbstractTile[Tile, Coordinate], Coordinate]{
  def snapshot(t: Tile): TileSnapshot[Tile, Coordinate]
}

trait MapSnapshotBuilder[Map <: AbstractMap[Tile, Coordinate], Tile <: AbstractTile[Tile, Coordinate], Coordinate]{
  def snapshot(m: Map): MapSnapshot[Map, Tile, Coordinate]
}