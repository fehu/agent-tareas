package feh.tec.world

import feh.tec.agent.AgentId


trait AbstractWorld[Atom <: WorldAtom[Atom, Coordinate], Coordinate] {
  def atoms: Seq[Atom]

  def get: PartialFunction[Coordinate, Atom]
}


/**
 * Continuous, enclosed, toroidal map. Is has no border and therefore every tile has exactly the same number of neighbours
 */
trait EnclosedWorld[Atom <: WorldAtom[Atom, Coordinate], Coordinate] extends AbstractWorld[Atom, Coordinate]{
  def nNeighbours: Int
}


trait AgentsPositionsProvidingWorld[Atom <: WorldAtom[Atom, Coordinate], Coordinate]{
  self: AbstractWorld[Atom, Coordinate] =>

  def agentsPositions: Map[AgentId, Atom]
}
