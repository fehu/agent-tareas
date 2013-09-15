package feh.tec.map

import feh.tec.map.tile.AbstractTile
import feh.tec.agent.Route

trait ShortestRouteFinder[Map <: AbstractMap[Tile, Coordinate], Tile <: AbstractTile[Tile, Coordinate], Coordinate] {
  @deprecated("very inefficient")
  def shortestRoute(map: Map)(from: Coordinate, to: Coordinate): Route[Coordinate] =
    shortestRoutes(map)(from, Set(to)).head._2
  @deprecated("very inefficient")
  def shortestRoute(snapshot: MapSnapshot[Map, Tile, Coordinate])(from: Coordinate, to: Coordinate): Route[Coordinate] =
    shortestRoutes(snapshot)(from, Set(to)).head._2

  def shortestRoutes(map: Map)(from: Coordinate, to: Set[Coordinate]): Predef.Map[Coordinate, Route[Coordinate]]
  def shortestRoutes(snapshot: MapSnapshot[Map, Tile, Coordinate])(from: Coordinate, to: Set[Coordinate]): Predef.Map[Coordinate, Route[Coordinate]]

  def findClosest(map: Map)(relativelyTo: (Int, Int), what: (Int, Int) => Boolean): Option[((Int, Int), Int)] =
    findClosests(map)(relativelyTo, what).headOption
  def findClosest(snapshot: MapSnapshot[Map, Tile, Coordinate])(relativelyTo: (Int, Int), what: (Int, Int) => Boolean): Option[((Int, Int), Int)] =
    findClosests(snapshot)(relativelyTo, what).headOption

  def findClosests(map: Map)(relativelyTo: (Int, Int), what: (Int, Int) => Boolean): Predef.Map[(Int, Int), Int]
  def findClosests(snapshot: MapSnapshot[Map, Tile, Coordinate])(relativelyTo: (Int, Int), what: (Int, Int) => Boolean): Predef.Map[(Int, Int), Int]
}