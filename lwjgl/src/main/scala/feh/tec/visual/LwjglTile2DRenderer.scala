package feh.tec.visual

import feh.tec.visual.api._
import feh.tec.map.tile.{MapObjectContainer, MapObject, SquareTile, AbstractTile}

trait LwjglTile2DRenderer[Tile <: AbstractTile[Tile, TCoordinate], TCoordinate] extends TileRenderer[Tile, TCoordinate]{
  override type E <: Easel2D with OpenGLEasel

  def renderers: Seq[LwjglTileDrawer[Tile, TCoordinate, E]]

  def draw(tile: Tile, where: E#Coordinate, how: E#TDrawOptions)(implicit easel: E) {
    renderers foreach (_.doTheDrawing(tile, where, how))
  }
}

class LwjglTile2DIntRenderer[Tile <: AbstractTile[Tile, (Int, Int)]](val renderers: Seq[LwjglTileDrawer[Tile, (Int, Int), Easel2DFloat with OpenGLEasel]])
  extends LwjglTile2DRenderer[Tile, (Int, Int)]
{

  type E = Easel2DFloat with OpenGLEasel
}

trait LwjglTileDrawer[Tile <: AbstractTile[Tile, TCoordinate], TCoordinate, E <: Easel]{
  def doTheDrawing(tile: Tile, where: E#Coordinate, how: E#TDrawOptions)(implicit easel: E)
}

class BasicLwjglSquareTileDrawer[Tile <: SquareTile[Tile, TCoord], TCoord, E <: Easel2D with OpenGLEasel]
  extends LwjglTileDrawer[Tile, TCoord, E]
{

  def doTheDrawing(tile: Tile, where: E#Coordinate, how: E#TDrawOptions)(implicit easel: E) {
    how match {
      case ops: E#TDrawOptions with SquareTileDrawOptions[E] =>
        easel.withoutTextures{
          easel.withColor(ops.lineColor){
            easel.asInstanceOf[E].drawRect(where: E#Coordinate, ops.sideSize, ops.sideSize)
          }
        }
      case other =>
        println(s"BasicLwjglSquareTileDrawer doesn't know how to draw $other") // todo: use logger
    }
  }
}

trait LwjglContainerTileDrawer[Tile <: MapObjectContainer[Tile, TCoord, MObj], TCoord,  E <: Easel, MObj <: MapObject]
  extends LwjglTileDrawer[Tile, TCoord, E]

class Generic2DLwjglContainerTileDrawer[Tile <: MapObjectContainer[Tile, TCoord, MObj], TCoord,  E <: Easel2D, MObj <: MapObject]
  (val mapObjectDrawers: Seq[MapObjectLwjglTileDrawer[Tile, TCoord, MObj]])
  extends LwjglContainerTileDrawer[Tile, TCoord, E, MObj]
{

  def doTheDrawing(tile: Tile, where: E#Coordinate, how: E#TDrawOptions)(implicit easel: E) {
    for {
      obj <- tile.containerObjectsToList
      drawer <- mapObjectDrawers
    } drawer.draw(obj, where, how)
  }
}

trait MapObjectLwjglTileDrawer[Tile <: MapObjectContainer[Tile, TCoord, MObj], TCoord, MObj <: MapObject]{
  def draw(obj: MapObject, where: Easel2D#Coordinate, how: Easel2D#TDrawOptions)(implicit easel: Easel2D)
}