package feh.tec.world

import feh.tec.agent.Route

trait ShortestRouteFinder[World <: AbstractWorld[Atom, Coordinate], Atom <: WorldAtom[Atom, Coordinate], Coordinate] {
  def shortestRoutes(world: World)(from: Coordinate, to: Set[Coordinate]): Map[Coordinate, Route[Coordinate]]
  def shortestRoutes(snapshot: WorldSnapshot[World, Atom, Coordinate])(from: Coordinate, to: Set[Coordinate]): Map[Coordinate, Route[Coordinate]]
}