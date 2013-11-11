package feh.tec.map.visual

import feh.tec.world.{AbstractWorld, WorldAtom}

trait WorldVisualisationCalls[Atom <: WorldAtom[Atom, Coordinate], Coordinate] {
  self: AbstractWorld[Atom, Coordinate] =>

  def highlightAtoms(tiles: Set[Atom])
  def showMessage(msg: String)
  def clearMessage()
}
