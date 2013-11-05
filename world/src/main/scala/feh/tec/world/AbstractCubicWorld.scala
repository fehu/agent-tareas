package feh.tec.world

import feh.tec.world.Simple3dDirection.Simple3dDirection

trait AbstractCubicWorld[Cube <: CubicAtom[Cube, (Int, Int, Int)]]
  extends AbstractWorld[Cube, (Int, Int, Int)] with AbstractWorldOps[Cube, (Int, Int, Int), Simple3dDirection]
{
  implicit class CubeCoordinatesWrapper(t: Cube){
    def x = t.coordinate._1
    def y = t.coordinate._2
    def z = t.coordinate._3
  }

  val coordinates: CoordinatesMeta

  trait CoordinatesMeta {
    def xRange: Range
    def yRange: Range
    def zRange: Range
  }

}

trait EnclosedCubicWorld[Cube <: CubicAtom[Cube, (Int, Int, Int)]] extends AbstractCubicWorld[Cube] with EnclosedWorld[Cube, (Int, Int, Int)]{
  /**
   * todo: description!!
   */
  def relativePosition(of: (Int, Int, Int), relativelyTo: (Int, Int, Int)): Set[Simple3dDirection] = ???

  def relativeNeighboursPosition(of: (Int, Int, Int), relativelyTo: (Int, Int, Int)): Simple3dDirection = {
    import coordinates._
    import Simple3dDirection._

    relativelyTo -> of match{
      case ((x1, y1, z1), (x2, y2, z2)) if z1 == z2 && x1 == x2 && (y2 == y1 + 1 || y1 == yRange.max && y2 == yRange.min) => South
      case ((x1, y1, z1), (x2, y2, z2)) if z1 == z2 && x1 == x2 && (y2 == y1 - 1 || y1 == yRange.min && y2 == yRange.max) => North
      case ((x1, y1, z1), (x2, y2, z2)) if z1 == z2 && y1 == y2 && (x2 == x1 - 1 || x1 == xRange.min && x2 == xRange.max) => West
      case ((x1, y1, z1), (x2, y2, z2)) if z1 == z2 && y1 == y2 && (x2 == x1 + 1 || x1 == xRange.max && x2 == xRange.min) => East
      case ((x1, y1, z1), (x2, y2, z2)) if z1 == z2 && y1 == y2 && (x2 == x1 + 1 || x1 == xRange.max && x2 == xRange.min) => ???
      case ((x1, y1, z1), (x2, y2, z2)) if z1 == z2 && y1 == y2 && (x2 == x1 + 1 || x1 == xRange.max && x2 == xRange.min) => ???
      case (c1, c2) => sys.error(s"$c1 and $c2 are not neighbouring cubes")
    }
  }
}

trait CubicAtom[Atom <: CubicAtom[Atom, Coordinate], Coordinate] extends WorldAtom[Atom, Coordinate]