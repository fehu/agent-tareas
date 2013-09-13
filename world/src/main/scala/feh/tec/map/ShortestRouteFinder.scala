package feh.tec.map

import feh.tec.map.tile.AbstractTile
import feh.tec.agent.Route

trait ShortestRouteFinder[Map <: AbstractMap[Tile, Coordinate], Tile <: AbstractTile[Tile, Coordinate], Coordinate] {
  def shortestRoute(map: Map)(from: Coordinate, to: Coordinate): Route[Coordinate]
  def shortestRoute(snapshot: MapSnapshot[Map, Tile, Coordinate])(from: Coordinate, to: Coordinate): Route[Coordinate]
}