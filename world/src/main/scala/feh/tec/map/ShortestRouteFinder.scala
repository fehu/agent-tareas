package feh.tec.map

import feh.tec.map.tile.AbstractTile
import feh.tec.agent.Route

trait ShortestRouteFinder[Map <: AbstractMap[Tile, Coordinate], Tile <: AbstractTile[Tile, Coordinate], Coordinate] {
  def shortestRoutes(map: Map)(from: Coordinate, to: Set[Coordinate]): Predef.Map[Coordinate, Route[Coordinate]]
  def shortestRoutes(snapshot: MapSnapshot[Map, Tile, Coordinate])(from: Coordinate, to: Set[Coordinate]): Predef.Map[Coordinate, Route[Coordinate]]

  def findClosest(map: Map)(relativelyTo: (Int, Int), what: (Int, Int) => Boolean): Predef.Map[(Int, Int), Int]
  def findClosest(snapshot: MapSnapshot[Map, Tile, Coordinate])(relativelyTo: (Int, Int), what: (Int, Int) => Boolean): Predef.Map[(Int, Int), Int]
}