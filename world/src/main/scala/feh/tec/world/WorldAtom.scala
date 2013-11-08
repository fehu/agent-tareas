package feh.tec.world

trait WorldAtom[Atom <: WorldAtom[Atom, Coordinate], Coordinate] {
  def coordinate: Coordinate

  def neighbours: Seq[Atom]

  type Snapshot = AtomSnapshot[Atom, Coordinate]
}

trait WorldObject

trait TypedContainerAtom[Atom <: WorldAtom[Atom, Coordinate], Coordinate, Contents] extends WorldAtom[Atom, Coordinate]{
  def contents: Contents
}

trait UntypedContainerAtom[Atom <: WorldAtom[Atom, Coordinate], Coordinate] extends TypedContainerAtom[Atom, Coordinate, Any]

trait OptionalTypedContainerAtom[Atom <: WorldAtom[Atom, Coordinate], Coordinate, Contents]
  extends TypedContainerAtom[Atom, Coordinate, Option[Contents]]
trait OptionalUntypedContainerAtom[Atom <: WorldAtom[Atom, Coordinate], Coordinate]
  extends OptionalTypedContainerAtom[Atom, Coordinate, Any]

trait TypedListContainerAtom[Atom <: WorldAtom[Atom, Coordinate], Coordinate, Contents]
  extends TypedContainerAtom[Atom, Coordinate, List[Contents]]

trait WorldObjectContainer[Atom <: WorldAtom[Atom, Coordinate], Coordinate, WObj <: WorldObject]
  extends WorldAtom[Atom, Coordinate]
{
  def isOptionalContainer: Boolean
  def isSequenceContainer: Boolean
  def containerObjectsToList: List[WObj]

}

trait WorldObjectContainingAtom[Atom <: WorldAtom[Atom, Coordinate], Coordinate, WObj <: WorldObject]
  extends TypedContainerAtom[Atom, Coordinate, WObj] with WorldObjectContainer[Atom, Coordinate, WObj]
{
  def isOptionalContainer: Boolean = false
  def isSequenceContainer: Boolean = false
  def containerObjectsToList: List[WObj] = contents :: Nil
}

trait OptionalWorldObjectContainingAtom[Atom <: WorldAtom[Atom, Coordinate], Contents <: WorldObject, Coordinate]
  extends OptionalTypedContainerAtom[Atom, Coordinate, Contents] with WorldObjectContainer[Atom, Coordinate, Contents]
{
  def isOptionalContainer: Boolean = true
  def isSequenceContainer: Boolean = false
  def containerObjectsToList: List[Contents] = contents.toList
  def exists(f: Contents => Boolean): Boolean = contents.exists(f)
  def notExists(f: Contents => Boolean): Boolean = !contents.exists(f)
}

trait WorldObjectListContainingAtom[Atom <: WorldAtom[Atom, Coordinate], Contents <: WorldObject, Coordinate]
  extends TypedListContainerAtom[Atom, Coordinate, Contents] with WorldObjectContainer[Atom, Coordinate, Contents]
{
  def isOptionalContainer: Boolean = false
  def isSequenceContainer: Boolean = true
  def containerObjectsToList: List[Contents] = contents
}
