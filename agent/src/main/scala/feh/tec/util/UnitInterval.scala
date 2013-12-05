package feh.tec.util

import scala.collection.immutable.NumericRange
import scala.math.Numeric.DoubleAsIfIntegral
import scala.collection.mutable

class UnitInterval(step: Double) extends NumericRange.Inclusive[Double](0, 1, step)(DoubleAsIfIntegral)

object UnitInterval{
  
  def inUnitInterval(d: Double) = d >= 0 && d <= 1
  def assertInUnitInterval(d: Double) = assert(inUnitInterval(d), s"$d is not in [0, 1] interval")
}

import UnitInterval._

object InUnitInterval{
  implicit def inUnitIntervalToDoubleWrapper(u: InUnitInterval) = u.d
	implicit def DoubleToInUnitIntervalToWrapper(d: Double) = InUnitInterval(d)
}

case class InUnitInterval(d: Double){ assertInUnitInterval(d) }

trait SumInUnitInterval{
  self: TraversableOnce[Double] =>

  assertInUnitInterval(self.sum)
}

trait ValueSumInUnitInterval[K]{
  self: collection.Map[K, Double] =>

  assertInUnitInterval(self.map(_._2).sum)
}

class MutableHashMapValueSumInUnitInterval[K] extends mutable.HashMap[K, Double] with ValueSumInUnitInterval[K]{
  hmap =>
//  override protected def clearTable(): Unit = super.clearTable().assertingSum()
  override protected def removeEntry(key: K): mutable.DefaultEntry[K, Double] = super.removeEntry(key).assertingSum()
  override protected def findOrAddEntry[B](key: K, value: B): mutable.DefaultEntry[K, Double] = super.findOrAddEntry(key, value).assertingSum()
  override protected def addEntry(e: mutable.DefaultEntry[K, Double]): Unit = super.addEntry(e).assertingSum()

  protected lazy val enableAsserting = new ScopedState[Boolean](true)

  private implicit class Asserting[R](f: => R) {
    def assertingSum()  = {
      if(enableAsserting.get)assertInUnitInterval(hmap.map(_._2).sum)
      f
      }
  }

  def setFrom(secured: collection.Map[K, Double] with ValueSumInUnitInterval[K]) = enableAsserting.doWith(false){
    this.clear()
    this ++= secured
    this
  }

}