package feh.tec.world

import feh.tec.world.Simple2dDirection.Simple2dDirection

trait AbstractSquareMap[Tile <: SquareTile[Tile, (Int, Int)]]
  extends AbstractWorld[Tile, (Int, Int)] with AbstractWorldOps[Tile, (Int, Int), Simple2dDirection]
{

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
}

trait EnclosedSquareMap[Tile <: SquareTile[Tile, (Int, Int)]] extends AbstractSquareMap[Tile] with EnclosedWorld[Tile, (Int, Int)]{

  /**         todo: description!!
    * @return A relative position of `of` relatively to `relativelyTo` as a set of directions.
    *         one direction in set means that coordinates have the same vert/horiz position;
    *         three directions can be returned by Enclosed maps,
    *           case of 3 means that the distances between two coordinates are equal for both possible vertical or horizontal routes
    *         two directions are returned in the rest of cases, e.g. A is up and left of B
    *         Set() is returned if `of` == `relativelyTo`
    */
  def relativePosition(of: (Int, Int), relativelyTo: (Int, Int)): Set[Simple2dDirection] = {
    import coordinates._
    import Simple2dDirection._

    if(of == relativelyTo) return Set()

    def leftUpDist(ofProjection: Int, relProjection: Int, sizeProjection: => Int) =
      if(ofProjection < relProjection) relProjection - ofProjection else relProjection + sizeProjection - ofProjection
    def rightDownDist(ofProjection: Int, relProjection: Int, sizeProjection: => Int) =
      if(ofProjection > relProjection) ofProjection - relProjection else sizeProjection - relProjection + ofProjection

    def leftDist = leftUpDist(of._1, relativelyTo._1, xRange.length)
    def rightDist = rightDownDist(of._1, relativelyTo._1, xRange.length)
    def upDist = leftUpDist(of._2, relativelyTo._2, yRange.length)
    def downDist = rightDownDist(of._2, relativelyTo._2, yRange.length)

    def selectDirection(dir1: Simple2dDirection, dir2: Simple2dDirection)(dist1: Int, dist2: Int) =
      if(dist1 == dist2) dir1 :: dir2 :: Nil
      else if(dist1 < dist2) dir1 :: Nil
      else dir2 :: Nil

    val horDir = if(of._1 == relativelyTo._1) Nil else selectDirection(Left, Right)(leftDist, rightDist)
    val vertDir = if(of._2 == relativelyTo._2) Nil else selectDirection(Up, Down)(upDist, downDist)

    horDir.toSet ++ vertDir.toSet
  }

  def relativeNeighboursPosition(of: (Int, Int), relativelyTo: (Int, Int)): Simple2dDirection = {
    import coordinates._
    import Simple2dDirection._

    relativelyTo -> of match{
      case ((x1, y1), (x2, y2)) if x1 == x2 && (y2 == y1 + 1 || y1 == yRange.max && y2 == yRange.min) => Down
      case ((x1, y1), (x2, y2)) if x1 == x2 && (y2 == y1 - 1 || y1 == yRange.min && y2 == yRange.max) => Up
      case ((x1, y1), (x2, y2)) if y1 == y2 && (x2 == x1 - 1 || x1 == xRange.min && x2 == xRange.max) => Left
      case ((x1, y1), (x2, y2)) if y1 == y2 && (x2 == x1 + 1 || x1 == xRange.max && x2 == xRange.min) => Right
      case (c1, c2) => sys.error(s"$c1 and $c2 are not neighbouring tiles")
    }
  }
}

trait SquareTile[Tile <: SquareTile[Tile, Coordinate], Coordinate] extends WorldAtom[Tile, Coordinate]