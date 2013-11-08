package feh.tec.world

trait AtomSnapshot[Atom <: WorldAtom[Atom, Coordinate], Coordinate] extends WorldAtom[Atom, Coordinate]{
  self: Atom =>

  override val coordinate: Coordinate

  override def neighbours: Seq[Atom] = ???

  def neighboursSnapshots: Seq[AtomSnapshot[Atom, Coordinate]]

  def asAtom: Atom with AtomSnapshot[Atom, Coordinate] = self
}

object AtomSnapshot{
  implicit def asTile[Tile <: WorldAtom[Tile, Coordinate], Coordinate](s: AtomSnapshot[Tile, Coordinate]) = s.asAtom
}

trait WorldSnapshot[+World <: AbstractWorld[Atom, Coordinate], Atom <: WorldAtom[Atom, Coordinate], Coordinate]
  extends AbstractWorld[Atom, Coordinate]
{
  self: World =>

  val tilesSnapshots: Seq[AtomSnapshot[Atom, Coordinate]]

  def atoms: Seq[Atom] = ???

  def get: PartialFunction[Coordinate, Atom] = ???

  def getSnapshot: PartialFunction[Coordinate, AtomSnapshot[Atom, Coordinate]]

  def asWorld: World with WorldSnapshot[World, Atom, Coordinate] = self
}

trait AtomSnapshotBuilder[Atom <: WorldAtom[Atom, Coordinate], Coordinate]{
  def snapshot(t: Atom): AtomSnapshot[Atom, Coordinate]
}

trait MapSnapshotBuilder[World <: AbstractWorld[Atom, Coordinate], Atom <: WorldAtom[Atom, Coordinate], Coordinate]{
  def snapshot(m: World): WorldSnapshot[World, Atom, Coordinate]
}