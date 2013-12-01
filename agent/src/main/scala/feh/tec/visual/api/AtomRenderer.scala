package feh.tec.visual.api

import feh.tec.world.WorldAtom


trait AtomRenderer[Atom <: WorldAtom[Atom, Coordinate], Coordinate] {
  type E <: Easel

  def draw(atom: Atom, where: E#Coordinate, how: E#TDrawOptions)(implicit easel: E)
}

