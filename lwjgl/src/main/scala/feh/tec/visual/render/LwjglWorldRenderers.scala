package feh.tec.visual.render

import feh.tec.visual.api._
import feh.tec.world._
import feh.tec.visual.{NicolLike3DEasel, NicolLike2DEasel}

trait LwjglAtomDrawer[Atom <: WorldAtom[Atom, Coordinate], Coordinate, E <: Easel]{
  def doTheDrawing(atom: Atom, where: E#Coordinate, how: E#TDrawOptions)(implicit easel: E)
}


trait LwjglContainerAtomDrawer[Atom <: WorldObjectContainer[Atom, Coord, WObj], Coord,  E <: Easel, WObj <: WorldObject]
  extends LwjglAtomDrawer[Atom, Coord, E]

class Generic2DLwjglContainerAtomDrawer[Atom <: WorldObjectContainer[Atom, Coord, WObj], Coord,  E <: Easel2D, WObj <: WorldObject]
(val worldObjectDrawers: Seq[WorldObjectLwjglAtomDrawer[Atom, Coord, WObj]])
  extends LwjglContainerAtomDrawer[Atom, Coord, E, WObj]
{

  def doTheDrawing(atom: Atom, where: E#Coordinate, how: E#TDrawOptions)(implicit easel: E) {
    for {
      obj <- atom.containerObjectsToList
      drawer <- worldObjectDrawers
    } drawer.draw(obj, where, how)
  }
}

trait WorldObjectLwjglAtomDrawer[Atom <: WorldObjectContainer[Atom, Coord, WObj], Coord, WObj <: WorldObject]{
  def draw(obj: WorldObject, where: Easel2D#Coordinate, how: Easel2D#TDrawOptions)(implicit easel: Easel2D)
}

trait CompositeLwjglWorldRenderer[Atom <: WorldAtom[Atom, Coordinate], Coordinate]
  extends AtomRenderer[Atom, Coordinate]
{
  def renderers: Seq[LwjglAtomDrawer[Atom, Coordinate, E]]

  def draw(atom: Atom, where: E#Coordinate, how: E#TDrawOptions)(implicit easel: E) {
    renderers foreach (_.doTheDrawing(atom, where, how))
  }
}

trait Lwjgl2DWorldRenderer[World <: AbstractWorld[Atom, Coordinate], Atom <: WorldAtom[Atom, Coordinate], Coordinate, E <: NicolLike2DEasel]
  extends WorldRenderer[World, Atom , Coordinate, E]

trait Lwjgl3DWorldRenderer[World <: AbstractWorld[Atom, Coordinate], Atom <: WorldAtom[Atom, Coordinate], Coordinate, E <: NicolLike3DEasel]
  extends WorldRenderer[World, Atom , Coordinate, E]

trait LwjglAtom2DRenderer[Atom <: WorldAtom[Atom, Coordinate], Coordinate] extends CompositeLwjglWorldRenderer[Atom, Coordinate]{
  override type E <: Easel2D with OpenGLEasel
}

class LwjglAtom2DIntRenderer[Atom <: WorldAtom[Atom, (Int, Int)]](val renderers: Seq[LwjglAtomDrawer[Atom, (Int, Int), Easel2DFloat with OpenGLEasel]])
  extends LwjglAtom2DRenderer[Atom, (Int, Int)]
{

  type E = Easel2DFloat with OpenGLEasel
}