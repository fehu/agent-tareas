package feh.tec.visual.render

import feh.tec.world.{EmptyCoordinatesChain, CoordinatesChain, SquareTile, AbstractSquareMap}
import feh.tec.visual.{NicolLike2DEasel}
import feh.tec.visual.api._
import feh.tec.visual.api.StringAlignment.Center
import java.awt.Color
import feh.tec.visual.render.LwjglSquareMapRenderer.BuildTDrawOpsParams
import scala.Some
import feh.tec.visual.api.BasicStringDrawOps
import feh.tec.map.visual.WorldVisualisationCalls
import feh.tec.util
import feh.tec.world.Simple2dDirection._
import feh.tec.util.TripleBuilder


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
  extends Lwjgl2DWorldRenderer[Map, Tile, (Int, Int), E] with Lwjgl2DSquareGridRenderer[E] with WorldVisualisationCalls[Tile, (Int, Int)]
{
  def render(map: Map, how: E#MDrawOptions)(implicit easel: E) {
    positionHighlight(how, map)

    how match {
      case ops: E#MDrawOptions with SquareMapDrawOptions[E] =>
        renderGrid(map.coordinates.xRange.length, map.coordinates.yRange.length, ops.tileSideSize, Color.white)

        val atomsDrawOps = map.atoms.map(a => a -> buildTDrawOptions(a, ops))
        val (delayedRender, atoms) = atomsDrawOps.partition(_._2.delayedRendering)
        for ((tile, ops) <- atoms ++ delayedRender) tileRenderer.draw(tile, adjustCoords(tile.coordinate, how), ops)

        if(!markedRoute.isEmpty) doTheRouteDrawing(markedRoute, map, ops)

        if(ops.showLabels) renderLabels(map, how)
    }
  }

  protected var highlightedByMouse: Option[(Int, Int)] = None

  def buildTDrawOptions(tile: Tile, ops: E#MDrawOptions with SquareMapDrawOptions[E]) = {
    val (hX, hY) = highlightedByMouse.map{case (x, y) => (tile.coordinate._1 == x) -> (tile.coordinate._2 == y)}.getOrElse(false -> false)
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
    case c => highlightedByMouse = tileAt(c, how, map)
  }

  def markedRoute = _markedRoute
  private var _markedRoute: CoordinatesChain[(Int, Int)] = {
    new EmptyCoordinatesChain[(Int, Int)]
  }

  def drawRoute(atoms: CoordinatesChain[(Int, Int)]){
    _markedRoute = atoms
  }

  protected def doTheRouteDrawing(coordinates: CoordinatesChain[(Int, Int)], map: Map, ops: SquareMapDrawOptions[E])
                                 (implicit easel: E){
    easel.withoutTextures{
      easel.withColor(ops.routeHighlightColor){
        util.Y[(Boolean, Seq[Tile], Simple2dDirection), Unit](
          rec => {
            case (_, last :: Nil, unsafeDirection) => Option(unsafeDirection).map(drawArrowRoute(last, _, ops))
            case (true, first :: second :: tail, _) =>
              val dir = map.relativeNeighboursPosition(second.coordinate, first.coordinate)
              drawStraightRoute(first, dir, ops)
              rec(false -> (second :: tail) --> dir)
            case (_, l1 :: l2 :: tail, dir) =>
              val newDir = map.relativeNeighboursPosition(l2.coordinate, l1.coordinate)
              if (newDir == dir || newDir == dir.opposite)
                drawStraightRoute(l1, dir, ops)
              else {
                drawHalfRoute(l1, dir, ops)
                drawHalfRoute(l1, newDir.opposite, ops)
              }
              rec(false -> (l2 :: tail) --> newDir)
          }
        )((true, coordinates.seq.map(map.get), null))

      }
    }
  }

  private def drawHalfRoute(tile: Tile, dir: Simple2dDirection, ops: SquareMapDrawOptions[E])
                           (implicit easel: E) = {
    val (x, y) = tileCenter(tile, ops)
    val halfSide = ops.tileSideSize / 2
    dir match{
      case Right => easel.drawLine((x - halfSide) -> y, x -> y)
      case Left => easel.drawLine(x -> y, (x + halfSide) -> y)
      case Up => easel.drawLine(x -> y, x -> (y + halfSide))
      case Down => easel.drawLine(x -> (y - halfSide), x -> y)
    }
  }
  private def drawStraightRoute(tile: Tile, dir: Simple2dDirection, ops: SquareMapDrawOptions[E])
                               (implicit easel: E)= {
    val (x, y) = tileCenter(tile, ops)
    val halfSide = ops.tileSideSize / 2
    dir match{
      case Up | Down  => easel.drawLine(x -> (y - halfSide), x -> (y + halfSide))
      case Left | Right    => easel.drawLine((x - halfSide) -> y, (x + halfSide) -> y)
    }
  }

  private def drawArrowRoute(tile: Tile, dir: Simple2dDirection, ops: SquareMapDrawOptions[E])
                            (implicit easel: E) = {
    drawHalfRoute(tile, dir, ops)

    val (x, y) = tileCenter(tile, ops)
    val quarterSide = ops.tileSideSize / 2
    dir match {
      case Right =>
        easel.drawLine((x - quarterSide) -> (y - quarterSide), x -> y)
        easel.drawLine((x - quarterSide) -> (y + quarterSide), x -> y)
      case Left =>
        easel.drawLine(x -> y, (x + quarterSide) -> (y - quarterSide))
        easel.drawLine(x -> y, (x + quarterSide) -> (y + quarterSide))
      case Up =>
        easel.drawLine(x -> y, (x - quarterSide) -> (y + quarterSide))
        easel.drawLine(x -> y, (x + quarterSide) -> (y + quarterSide))
      case Down =>
        easel.drawLine((x - quarterSide) -> (y - quarterSide), x -> y)
        easel.drawLine((x + quarterSide) -> (y - quarterSide), x -> y)
    }
  }

  private def tileCenter(tile: Tile, ops: SquareMapDrawOptions[E]) = {
    def calcCoord(c: Int) = ops.tileSideSize * c + ops.tileSideSize/2 
    calcCoord(tile.coordinate._1) -> calcCoord(tile.coordinate._2)
  }

  def highlightAtoms(atoms: Set[Tile]) { /* todo */ }

  def showMessage(msg: String) {}

  def clearMessage() {}
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
