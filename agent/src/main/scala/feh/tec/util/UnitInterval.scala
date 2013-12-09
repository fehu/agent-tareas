package feh.tec.util

import scala.collection.immutable.NumericRange
import scala.math.Numeric.DoubleAsIfIntegral
import scala.collection.{mutable, immutable}

class UnitInterval(step: Double) extends NumericRange.Inclusive[Double](0, 1, step)(DoubleAsIfIntegral)

object UnitInterval{

  def inUnitInterval(d: Double) = d >= 0 && d <= 1
  def assertInUnitInterval(d: Double) = assert(inUnitInterval(d), s"$d is not in [0, 1] interval")
}

import UnitInterval._

object InUnitInterval{
  implicit object IsNumeric extends NumericProxy[Double, InUnitInterval](_.d, InUnitInterval.apply)

  implicit def inUnitIntervalToDoubleWrapper(u: InUnitInterval) = u.d
	implicit def DoubleToInUnitIntervalToWrapper(d: Double) = InUnitInterval(d)

  def apply(d: Double) = {
    assertInUnitInterval(d)
    new InUnitInterval(d)
  }
}

class InUnitInterval protected (val d: Double) extends AnyVal

trait SumInUnitInterval{
  self: TraversableOnce[Double] =>

  assertInUnitInterval(self.sum)
}

trait ValueSumInUnitInterval[K]{
  self: collection.Map[K, Double] =>

  assertInUnitInterval(self.map(_._2).sum)
}

class ImmutableHashMapValueSumInUnitInterval[K] extends immutable.HashMap[K, Double]() with ValueSumInUnitInterval[K]{

}
// todo
class MutableHashMapValueSumInUnitInterval[K](init: (K, Double)*) extends mutable.HashMap[K, Double] with ValueSumInUnitInterval[K]{
  hmap =>

  this ++= init

//  private var currentSum = 0d
//  private lazy val oldValue = mutable.Map.empty[K, Double].withDefaultValue(0)
//
////  override protected def clearTable(): Unit = super.clearTable().assertingSum()
//  override protected def removeEntry(key: K): mutable.DefaultEntry[K, Double] = {
//    get(key).foreach{
//      v =>
//        currentSum -= v - oldValue(key)
//        oldValue -= key
//        assertSum
//    }
//    super.removeEntry(key)
//  }
//
//
//  override protected def findOrAddEntry[B](key: K, value: B): mutable.DefaultEntry[K, Double] = {
//    Option(value.asInstanceOf[Double]).flatMap(v => Option(key -> v))foreach{
//      case (k, d) =>
//        Option(findEntry(key)).getOrElse{
//          println(currentSum, d, key, oldValue)
//              currentSum += d - oldValue(key)
//              oldValue(key) = d
//          }
//          assertSum
//    }
//    super.findOrAddEntry(key, value)
//  }
//  override protected def addEntry(e: mutable.DefaultEntry[K, Double]): Unit = {
//    currentSum += e.value - oldValue(e.key)
//    oldValue(e.key) = e.value
//    assertSum
//    println(currentSum + "*(********************" + oldValue)
//    super.addEntry(e)
//  }
//
//  override protected def findEntry(key: K): mutable.DefaultEntry[K, Double] = super.findEntry(key)

//  protected lazy val enableAsserting = new ScopedState[Boolean](true)

//  private def assertSum = assertInUnitInterval(currentSum)

//  private implicit class Asserting[R](f: => R) {
//    def assertingSum()  = {
//      val res = f
//      if(enableAsserting.get)assertInUnitInterval(hmap.values.sum)
//      res
//      }
//  }

//  def setFrom(secured: collection.Map[K, Double] with ValueSumInUnitInterval[K]) = enableAsserting.doWith(false){
//    this.clear()
//    this ++= secured
//    this
//  }

}