package feh.tec.map

import feh.tec.map.tile.{SquareTile, AbstractTile}
import feh.tec.agent.AgentId


trait AbstractMap[Tile <: AbstractTile[Tile, Coordinate], Coordinate] {
  def tiles: Seq[Tile]

  def get: PartialFunction[Coordinate, Tile]
}

trait AbstractSquareMap[Tile <: SquareTile[Tile, (Int, Int)]] extends AbstractMap[Tile, (Int, Int)]{

  implicit class TileCoordinatesWrapper(t: Tile){
    def x = t.coordinate._1
    def y = t.coordinate._2
  }

  val coordinates: CoordinatesMeta
  protected def assertDefinedAtAllCoordinates()

  trait CoordinatesMeta {
    def xRange: Range
    def yRange: Range
  }

  /**         todo: description!!
   * @return A relative position of `of` relatively to `relativelyTo` as a set of directions.
   *         one direction in set means that coordinates have the same vert/horiz position;
   *         three directions can be returned by Enclosed maps,
   *           case of 3 means that the distances between two coordinates are equal for both possible vertical or horizontal routes
   *         two directions are returned in the rest of cases, e.g. A is up and left of B
   *         Set() is returned if `of` == `relativelyTo`
   */
  def relativePosition(of: (Int, Int), relativelyTo: (Int, Int)): Set[SimpleDirection]

  def relativeNeighboursPosition(of:  (Int, Int), relativelyTo:  (Int, Int)): SimpleDirection
}

/**
 * Continuous, enclosed, toroidal map. Is has no border and therefore every tile has exactly the same number of neighbours
 */
trait EnclosedMap[Tile <: AbstractTile[Tile, Coordinate], Coordinate] extends AbstractMap[Tile, Coordinate]{
  def nNeighbours: Int
}

trait EnclosedSquareMap[Tile <: SquareTile[Tile, (Int, Int)]] extends AbstractSquareMap[Tile] with EnclosedMap[Tile, (Int, Int)]{
  def relativePosition(of: (Int, Int), relativelyTo: (Int, Int)): Set[SimpleDirection] = {
    import coordinates._
    import SimpleDirection._

    if(of == relativelyTo) return Set()

    def leftUpDist(ofProjection: Int, relProjection: Int, sizeProjection: => Int) =
      if(ofProjection < relProjection) relProjection - ofProjection else relProjection + sizeProjection - ofProjection
    def rightDownDist(ofProjection: Int, relProjection: Int, sizeProjection: => Int) =
      if(ofProjection > relProjection) ofProjection - relProjection else sizeProjection - relProjection + ofProjection

    val leftDist = leftUpDist(of._1, relativelyTo._1, xRange.length)
    val rightDist = rightDownDist(of._1, relativelyTo._1, xRange.length)
    val upDist = leftUpDist(of._2, relativelyTo._2, yRange.length)
    val downDist = rightDownDist(of._2, relativelyTo._2, yRange.length)

    def selectDirection(dir1: SimpleDirection, dir2: SimpleDirection)(dist1: Int, dist2: Int) =
      if(dist1 == dist2) dir1 :: dir2 :: Nil
      else if(dist1 < dist2) dir1 :: Nil
      else dir2 :: Nil

    val horDir = selectDirection(Left, Right)(leftDist, rightDist)
    val vertDir = selectDirection(Up, Down)(upDist, downDist)

    (if(horDir.size == 2 && vertDir.nonEmpty) Set() else horDir.toSet) ++
      (if(vertDir.size == 2 && horDir.nonEmpty) Set() else vertDir.toSet)
  }

  def relativeNeighboursPosition(of: (Int, Int), relativelyTo: (Int, Int)): SimpleDirection = {
    import coordinates._
    import SimpleDirection._

    relativelyTo -> of match{
      case ((x1, y1), (x2, y2)) if x1 == x2 && (y2 == y1 + 1 || y1 == yRange.max && y2 == yRange.min) => Down
      case ((x1, y1), (x2, y2)) if x1 == x2 && (y2 == y1 - 1 || y1 == yRange.min && y2 == yRange.max) => Up
      case ((x1, y1), (x2, y2)) if y1 == y2 && (x2 == x1 - 1 || x1 == xRange.min && x2 == xRange.max) => Left
      case ((x1, y1), (x2, y2)) if y1 == y2 && (x2 == x1 + 1 || x1 == xRange.max && x2 == xRange.min) => Right
      case (c1, c2) => sys.error(s"$c1 and $c2 are not neighbouring tiles")
    }
  }
}

trait AgentsPositionsProvidingMap[Tile <: AbstractTile[Tile, Coordinate], Coordinate]{
  self: AbstractMap[Tile, Coordinate] =>

  def agentsPositions: Predef.Map[AgentId, Tile]
}
