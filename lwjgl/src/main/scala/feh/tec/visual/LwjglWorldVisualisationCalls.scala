package feh.tec.visual

import feh.tec.map.visual.WorldVisualisationCalls
import feh.tec.visual.api.Easel
import feh.tec.world.{WorldAtom, AbstractWorld}

trait LwjglWorldVisualisationCalls[Atom <: WorldAtom[Atom, Coordinate], Coordinate, E <: Easel]
  extends WorldVisualisationCalls[Atom, Coordinate]
{
  self: AbstractWorld[Atom, Coordinate] =>

  def highlightAtoms(tiles: Set[Atom]) {}

  def showMessage(msg: String) {}

  def clearMessage() {}
}
