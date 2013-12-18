package feh.tec.world

/** A large world, where an AbstractWorld's Atom effectively turns into a dot.
 *  Therefore it is replaced by AtomRef, representing a given point off space.   
 */
trait LargeWorld[ARef <: LargeWorld[ARef, Coordinate]#AtomRef, Coordinate] extends AbstractWorld[ARef, Coordinate]{

  def properties: Set[Property[_]]

  /**
   * represe
   */
  trait AtomRef extends WorldAtom[ARef, Coordinate]{
    /**
     *
     */
    def properties: Set[Property[_]#Value]
  }

  trait Property[P] extends (Coordinate => P) {
    prop =>

    def name: String
    protected def get: Coordinate => P
    def canAccess: Boolean

    final def apply(v1: Coordinate): P = if(canAccess) apply(v1) else throw new PropertyAccessException

    object Value{
      def apply(c: Coordinate): Value = Value(name, prop.apply(c))
    }
    case class Value protected (name: String, value: P){
      lazy val g = this.asInstanceOf[Property[Any]#Value]
    }

    protected class PropertyAccessException extends IllegalAccessException(s"access to property $name has been denied to caller")
  }

  trait MutableProperty[P] extends Property[P] with Mutable{
    def canUpdate: Boolean
    protected def set(c: Coordinate, p: P)

    final def update(c: Coordinate, p: P) = if(canUpdate) set(c, p) else throw new PropertyAccessException
  }
}

/**
 *  Larger world entities, occupying several coordinates.
 */
trait LargeWorldObjects[ARef <: LargeWorld[ARef, Coordinate]#AtomRef, Coordinate] {
  self: LargeWorld[ARef, Coordinate] =>

  type Region <: WorldRegion

  trait WorldRegion{
    def includes(c: Coordinate): Boolean
  }

  override def atoms: Stream[ARef]

  trait Object extends WorldAtom[Object, Region]
  trait Container extends Object{
    def contents: Seq[Object]
    def at(c: Coordinate): Option[Object]
  }
  trait RootObject extends Container

  def rootObject: RootObject
}

object TestWorld{
  type Coord = (Long, Long)
  implicit class CoordinatesWrapper(c: Coord){
    def x = c._1
    def y = c._2
  }
}

import TestWorld._

class TestWorld[ARef <: LargeWorld[ARef, Coord]#AtomRef]
  extends LargeWorld[ARef, Coord] with LargeWorldObjects[ARef, Coord]
{
  world =>

  def get: PartialFunction[Coord, ARef] = ???

  case class BaseRef(coordinate: Coord ) extends AtomRef{
    def neighbours: Seq[ARef] = ???

    /**
     *
     */
    def properties: Set[Property[_]#Value] = world.properties.map(_.Value(coordinate).g)
  }

  case class SquareRegion(leftTop: Coord, bottomRight: Coord) extends WorldRegion{
    def includes(c: Coord): Boolean =
      c.x > leftTop.x && c.x < bottomRight.x &&
      c.y > bottomRight.y && c.y < leftTop.y
  }

  type Region = SquareRegion

  override def atoms: Stream[ARef] = ???

  def rootObject: RootObject = ???

  def properties: Set[Property[_]] = ???
}