package feh.tec.map

import feh.tec.map.tile.AbstractTile


trait AbstractMap {
  type Coordinate
  type Tile <: AbstractTile[Tile, Coordinate]

  def tiles: Seq[Tile]

  def get: PartialFunction[Coordinate, Tile]
}

trait AbstractSquareMap extends AbstractMap{
  type Coordinate = (Int, Int)

//  implicit class CoordinateWrapper(c: Coordinate){
//    def x = c._1
//    def y = c._2
//  }

  implicit class TileCoordinatesWrapper(t: Tile){
    def x = t.coordinate._1
    def y = t.coordinate._2
  }

  assertDefinedAtAllCoordinates()

  def coordinates: CoordinatesMeta
  protected def assertDefinedAtAllCoordinates()

  trait CoordinatesMeta {
    def xRange: Range
    def yRange: Range
  }
}

/**
 * Continuous, enclosed map. Is has no border and therefore every tile has exactly the same number of neighbours
 */
trait EnclosedMap extends AbstractMap{
  def nNeighbours: Int
}