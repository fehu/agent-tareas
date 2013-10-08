package feh.tec.visual

import feh.tec.visual.api.{BasicStringDrawOps, SquareMapDrawOptions, MapRenderer}
import feh.tec.map.{AbstractSquareMap, AbstractMap}
import feh.tec.map.tile.{AbstractTile, SquareTile}
import feh.tec.visual.api.StringAlignment.Center
import java.awt.Color

trait Lwjgl2DMapRenderer[Map <: AbstractMap[Tile, Coordinate], Tile <: AbstractTile[Tile, Coordinate], Coordinate, E <: NicolLike2DEasel]
  extends MapRenderer[Map, Tile , Coordinate, E]

class LwjglSquare2DMapRenderer[Map <: AbstractSquareMap[Tile], Tile <: SquareTile[Tile, (Int, Int)], E <: NicolLike2DEasel]
  (val tileRenderer: LwjglTile2DIntRenderer[Tile],
   buildTDrawOps: (Tile, NicolLike2DEasel#MDrawOptions with SquareMapDrawOptions[NicolLike2DEasel]) => NicolLike2DEasel#TDrawOptions
  )
  extends Lwjgl2DMapRenderer[Map, Tile, (Int, Int), E]
{
  def render(map: Map, how: E#MDrawOptions)(implicit easel: E) {
    how match {
      case ops: E#MDrawOptions with SquareMapDrawOptions[E] =>
        for {
          tile <- map.tiles
        } tileRenderer.draw(tile, adjustCoords(tile.coordinate, how), buildTDrawOptions(tile, ops))
        renderLabels(map, how)
    }
  }

  def buildTDrawOptions(tile: Tile, ops: E#MDrawOptions with SquareMapDrawOptions[E]) =
    buildTDrawOps(tile, ops).asInstanceOf[LwjglTile2DIntRenderer[Tile]#E#TDrawOptions]

  def adjustCoords(mc: (Int, Int), how: E#MDrawOptions): (Float, Float) =
    (mc._1*how.tileSideSize, mc._2*how.tileSideSize)
  
  def renderLabels(map: Map, how: E#MDrawOptions)(implicit easel: E) =
    if(how.showLabels) {
      val offset = how.tileSideSize / 2
      val strOps = BasicStringDrawOps[E](Center, Color.white, "arial", 0, 10)
      
      for(x <- map.coordinates.xRange) easel.drawString(x.toString, x*how.tileSideSize + offset -> how.tileSideSize/2, strOps)
      for(y <- map.coordinates.yRange) easel.drawString(y.toString, how.tileSideSize/2 -> (y*how.tileSideSize + offset), strOps)
    } 
}