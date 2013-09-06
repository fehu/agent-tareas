package feh.tec.agentos.map.tile

trait AbstractTile[Tile <: AbstractTile[Tile, Coordinate], Coordinate] {
  def coordinate: Coordinate

  def neighbours: Seq[Tile]
}

trait TypedContainerTile[Tile <: AbstractTile[Tile, Coordinate], Contents, Coordinate] extends AbstractTile[Tile, Coordinate]{
  def contents: Contents
}

trait UntypedContainerTile[Tile <: AbstractTile[Tile, Coordinate], Coordinate] extends TypedContainerTile[Tile, Any, Coordinate]

trait OptionalTypedContainerTile[Tile <: AbstractTile[Tile, Coordinate], Contents, Coordinate] extends TypedContainerTile[Tile, Option[Contents], Coordinate]
trait OptionalUntypedContainerTile[Tile <: AbstractTile[Tile, Coordinate], Coordinate] extends OptionalTypedContainerTile[Tile, Any, Coordinate]