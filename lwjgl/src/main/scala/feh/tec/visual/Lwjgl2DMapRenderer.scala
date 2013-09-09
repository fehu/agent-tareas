package feh.tec.visual

import feh.tec.visual.api.{SquareMapDrawOptions, MapRenderer}
import feh.tec.map.{AbstractSquareMap, AbstractMap}
import feh.tec.map.tile.{AbstractTile, SquareTile}

trait Lwjgl2DMapRenderer[Map <: AbstractMap[Tile, Coordinate], Tile <: AbstractTile[Tile, Coordinate], Coordinate]
  extends MapRenderer[Map, Tile , Coordinate]
{
  type E = NicolLike2DEasel
}

class LwjglSquare2DMapRenderer[Map <: AbstractSquareMap[Tile], Tile <: SquareTile[Tile, (Int, Int)]]
  (val tileRenderer: LwjglTile2DIntRenderer[Tile],
   buildTDrawOps: (Tile, NicolLike2DEasel#MDrawOptions with SquareMapDrawOptions[NicolLike2DEasel]) => NicolLike2DEasel#TDrawOptions
  )
  extends Lwjgl2DMapRenderer[Map, Tile, (Int, Int)]
{
  def render(map: Map, how: E#MDrawOptions)(implicit easel: E) {
    how match {
      case ops: E#MDrawOptions with SquareMapDrawOptions[E] =>
//        val tileOps = easel.buildTDrawOptions(BasicSquareTileDrawOptions(easel.unitNumeric.fromInt(50), Color.black, None))
        for {
          tile <- map.tiles
        } tileRenderer.draw(tile, adjustCoords(tile.coordinate, how), buildTDrawOptions(tile, ops))
    }
  }

  def buildTDrawOptions(tile: Tile, ops: E#MDrawOptions with SquareMapDrawOptions[E]) =
    buildTDrawOps(tile, ops).asInstanceOf[LwjglTile2DIntRenderer[Tile]#E#TDrawOptions]

  def adjustCoords(mc: (Int, Int), how: E#MDrawOptions): (Float, Float) =
    (mc._1*how.tileSideSize, mc._2*how.tileSideSize)
}