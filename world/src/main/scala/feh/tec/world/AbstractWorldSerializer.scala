package feh.tec.world

trait AbstractWorldSerializer[World <: AbstractWorld[Atom , Coordinate], Atom <: WorldAtom[Atom, Coordinate], Coordinate] {
  type Serialized
  def serialize(map: World): Serialized
  def world(ser: Serialized): World
}
