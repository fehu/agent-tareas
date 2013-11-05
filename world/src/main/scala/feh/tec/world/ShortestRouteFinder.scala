package feh.tec.world

import feh.tec.agent.Route

trait ShortestRouteFinder[World <: AbstractWorld[Atom, Coordinate], Atom <: WorldAtom[Atom, Coordinate], Coordinate] {
  def shortestRoutes(world: World)(from: Coordinate, to: Set[Coordinate]): Map[Coordinate, Route[Coordinate]]
  def shortestRoutes(snapshot: WorldSnapshot[World, Atom, Coordinate])(from: Coordinate, to: Set[Coordinate]): Map[Coordinate, Route[Coordinate]]

// todo : CoordinateUnit needed; Implicit mechanism could be used
//  def findClosest(world: World)(relativelyTo: Coordinate, what: Coordinate => Boolean): Map[Coordinate, Int]
//  def findClosest(snapshot: WorldSnapshot[World, Atom, Coordinate])(relativelyTo: (Int, Int), what: (Int, Int) => Boolean): Predef.Map[(Int, Int), Int]
}