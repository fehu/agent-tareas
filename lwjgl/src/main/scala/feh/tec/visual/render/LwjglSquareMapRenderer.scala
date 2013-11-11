package feh.tec.visual.render

import feh.tec.world.{SquareTile, AbstractSquareMap}
import feh.tec.visual.NicolLike2DEasel
import feh.tec.visual.api._
import feh.tec.visual.api.StringAlignment.Center
import java.awt.Color
import feh.tec.visual.render.LwjglSquareMapRenderer.BuildTDrawOpsParams
import scala.Some
import feh.tec.visual.api.BasicStringDrawOps


object LwjglSquareMapRenderer{
  case class BuildTDrawOpsParams[Map <: AbstractSquareMap[Tile], Tile <: SquareTile[Tile, (Int, Int)], E <: NicolLike2DEasel]
  (tile: Tile, mOps: NicolLike2DEasel#MDrawOptions with SquareMapDrawOptions[NicolLike2DEasel], highlightedX: Boolean, highlightedY: Boolean)
}

trait Lwjgl2DSquareGridRenderer[E <: Easel2D with OpenGLEasel]{
  def renderGrid(nX: Int, nY: Int, tileSize: E#CoordinateUnit, lineColor: Color)(implicit easel: E) =
    easel.withoutTextures{
      easel.withColor(lineColor){
        val num = easel.unitNumeric
        def tileSizeTimes(i: Int) = num.times(tileSize.asInstanceOf[easel.CoordinateUnit], num.fromInt(i))
        def coord(c1: (Easel2D with OpenGLEasel)#CoordinateUnit, c2: (Easel2D with OpenGLEasel)#CoordinateUnit) =
          easel.coordinate(c1.asInstanceOf[easel.CoordinateUnit], c2.asInstanceOf[easel.CoordinateUnit])

        val lenX = tileSizeTimes(nX)
        val lenY = tileSizeTimes(nY)

        for {
          i <- 0 to nX
          cx = tileSizeTimes(i)
        }
          easel.drawLine(coord(cx, num.zero), coord(cx, lenY))

        for {
          i <- 0 to nY
          cy = tileSizeTimes(i)
        }
          easel.drawLine(coord(num.zero, cy), coord(lenX, cy))
      }
    }
  }

class LwjglSquareMapRenderer[Map <: AbstractSquareMap[Tile], Tile <: SquareTile[Tile, (Int, Int)], E <: NicolLike2DEasel]
    (val tileRenderer: LwjglCompositeAtom2DIntRenderer[Tile],
     val renderOptions: E#MDrawOptions,
     buildTDrawOps: BuildTDrawOpsParams[Map, Tile, E] => NicolLike2DEasel#TDrawOptions
      )
  extends Lwjgl2DWorldRenderer[Map, Tile, (Int, Int), E] with Lwjgl2DSquareGridRenderer[E]
{
  def render(map: Map, how: E#MDrawOptions)(implicit easel: E) {
    positionHighlight(how, map)

    how match {
      case ops: E#MDrawOptions with SquareMapDrawOptions[E] =>
        renderGrid(map.coordinates.xRange.length, map.coordinates.yRange.length, ops.tileSideSize, Color.white)

        val atomsDrawOps = map.atoms.map(a => a -> buildTDrawOptions(a, ops))
        val (delayedRender, atoms) = atomsDrawOps.partition(_._2.delayedRendering)
        for ((tile, ops) <- atoms ++ delayedRender) tileRenderer.draw(tile, adjustCoords(tile.coordinate, how), ops)

        if(ops.showLabels) renderLabels(map, how)
    }
  }

  protected var highlighted: Option[(Int, Int)] = None

  def buildTDrawOptions(tile: Tile, ops: E#MDrawOptions with SquareMapDrawOptions[E]) = {
    val (hX, hY) = highlighted.map{case (x, y) => (tile.coordinate._1 == x) -> (tile.coordinate._2 == y)}.getOrElse(false -> false)
    buildTDrawOps(BuildTDrawOpsParams(tile, ops, hX, hY)).asInstanceOf[LwjglCompositeAtom2DIntRenderer[Tile]#E#TDrawOptions]
  }


  def adjustCoords(mc: (Int, Int), how: E#MDrawOptions): (Float, Float) =
    (mc._1*how.tileSideSize, mc._2*how.tileSideSize)

  def tileAt(pos: (Float, Float), how: E#MDrawOptions, map: Map): Option[(Int, Int)] = {
    val tileSize = how.tileSideSize
    pos match{
      case (x, y) if x < tileSize || y < tileSize => None
      case (x, y) if x > (map.coordinates.xRange.max+1)*tileSize || y > (map.coordinates.yRange.max+1)*tileSize => None
      case (x, y) =>
        val (nx, ny) = x/tileSize -> y/tileSize
        if(nx == nx.toInt || ny == ny.toInt) None
        else Some(nx.toInt, ny.toInt)
    }
  }

  def renderLabels(map: Map, how: E#MDrawOptions)(implicit easel: E) = {
    val offset = how.tileSideSize / 2
    val strOps = BasicStringDrawOps[E](Center, Color.white, "arial", 10, 2)

    for(x <- map.coordinates.xRange) easel.drawString(x.toString, x*how.tileSideSize + offset -> how.tileSideSize/2, strOps)
    for(y <- map.coordinates.yRange) easel.drawString(y.toString, how.tileSideSize/2 -> (y*how.tileSideSize + offset), strOps)
  }

  def positionHighlight(how: E#MDrawOptions, map: Map)(implicit easel: E) = easel.onMouseMove{
    case c => highlighted = tileAt(c, how, map)
  }
}

class BasicLwjglSquareTileDrawer[Tile <: SquareTile[Tile, TCoord], TCoord, E <: Easel2D with OpenGLEasel]
  extends LwjglAtomDrawer[Tile, TCoord, E]
{

  def doTheDrawing(tile: Tile, where: E#Coordinate, how: E#TDrawOptions)(implicit easel: E) {
    how match {
      case ops: E#TDrawOptions with SquareTileDrawOptions[E] =>
        if(ops.drawBorder)
          easel.withoutTextures{
            easel.withColor(ops.borderColor){
              easel.asInstanceOf[E].drawRect(where: E#Coordinate, ops.sideSize, ops.sideSize)
            }
          }
      case other =>
        println(s"BasicLwjglSquareTileDrawer doesn't know how to draw $other") // todo: use logger
    }
  }
}
