package feh.tec.util

class NumericProxy[N: Numeric, T](extract: T => N, build: N => T) extends Numeric[T]{
  val underlying = implicitly[Numeric[N]]
  def plus(x: T, y: T): T = build(underlying.plus(extract(x), extract(y)))
  def minus(x: T, y: T): T = build(underlying.minus(extract(x), extract(y)))
  def times(x: T, y: T): T = build(underlying.times(extract(x), extract(y)))
  def negate(x: T): T = build(underlying.negate(extract(x)))
  def fromInt(x: Int): T = build(underlying.fromInt(x))
  def toInt(x: T): Int = underlying.toInt(extract(x))
  def toLong(x: T): Long = underlying.toLong(extract(x))
  def toFloat(x: T): Float = underlying.toFloat(extract(x))
  def toDouble(x: T): Double = underlying.toDouble(extract(x))
  def compare(x: T, y: T): Int = underlying.compare(extract(x), extract(y))
}
