package feh.tec.map.tile

trait AbstractTile[Tile <: AbstractTile[Tile, Coordinate], Coordinate] {
  def coordinate: Coordinate

  def neighbours: Seq[Tile]
}

trait TypedContainerTile[Tile <: AbstractTile[Tile, Coordinate], Coordinate, Contents] extends AbstractTile[Tile, Coordinate]{
  def contents: Contents
}

trait UntypedContainerTile[Tile <: AbstractTile[Tile, Coordinate], Coordinate] extends TypedContainerTile[Tile, Coordinate, Any]

trait OptionalTypedContainerTile[Tile <: AbstractTile[Tile, Coordinate], Coordinate, Contents]
  extends TypedContainerTile[Tile, Coordinate, Option[Contents]]
trait OptionalUntypedContainerTile[Tile <: AbstractTile[Tile, Coordinate], Coordinate]
  extends OptionalTypedContainerTile[Tile, Coordinate, Any]

trait TypedListContainerTile[Tile <: AbstractTile[Tile, Coordinate], Coordinate, Contents]
  extends TypedContainerTile[Tile, Coordinate, List[Contents]]

trait MapObjectContainer[Tile <: AbstractTile[Tile, Coordinate], Coordinate, MObj <: MapObject]
  extends AbstractTile[Tile, Coordinate]
{
  def isOptionalContainer: Boolean
  def isSequenceContainer: Boolean
  def containerObjectsToList: List[MObj]

}

trait MapObjectContainerTile[Tile <: AbstractTile[Tile, Coordinate], Coordinate, MObj <: MapObject]
  extends TypedContainerTile[Tile, Coordinate, MObj] with MapObjectContainer[Tile, Coordinate, MObj]
{
  def isOptionalContainer: Boolean = false
  def isSequenceContainer: Boolean = false
  def containerObjectsToList: List[MObj] = contents :: Nil
}

trait OptionalMabObjectContainerTile[Tile <: AbstractTile[Tile, Coordinate], Contents <: MapObject, Coordinate]
  extends OptionalTypedContainerTile[Tile, Coordinate, Contents] with MapObjectContainer[Tile, Coordinate, Contents]
{
  def isOptionalContainer: Boolean = true
  def isSequenceContainer: Boolean = false
  def containerObjectsToList: List[Contents] = contents.toList
}

trait MapObjectListContainerTile[Tile <: AbstractTile[Tile, Coordinate], Contents <: MapObject, Coordinate]
  extends TypedListContainerTile[Tile, Coordinate, Contents] with MapObjectContainer[Tile, Coordinate, Contents]
{
  def isOptionalContainer: Boolean = false
  def isSequenceContainer: Boolean = true
  def containerObjectsToList: List[Contents] = contents
}

trait SquareTile[Tile <: SquareTile[Tile, Coordinate], Coordinate] extends AbstractTile[Tile, Coordinate]
