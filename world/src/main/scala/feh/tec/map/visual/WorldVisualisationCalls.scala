package feh.tec.map.visual

import feh.tec.world.{CoordinatesChain, WorldAtom}

trait WorldVisualisationCalls[Atom <: WorldAtom[Atom, Coordinate], Coordinate] {
  def drawRoute(atoms: CoordinatesChain[Coordinate])
  def highlightAtoms(atoms: Set[Atom])
  def showMessage(msg: String)
  def clearMessage()
}
