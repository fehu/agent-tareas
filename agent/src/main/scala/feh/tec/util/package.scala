package feh.tec

import scala.collection.{mutable, TraversableLike}
import scala.concurrent.duration._
import java.util.Calendar
import scala.reflect.runtime.universe._

package object util {
  type I[T] = T => T
  /**
   *  The fixed point combinator
   */
  def Y[A, B](rec: (A => B) => (A => B)): A => B = rec(Y(rec))(_: A)

  case class CYResult[A, B](result: B, cache: Map[A, B])
  def CY[A, B](rec: (A => B) => (A => B)): A => CYResult[A, B] = {
    val cache = mutable.HashMap.empty[A, B]
    def YY(f: (A => B) => (A => B)): A => B = {
      a => cache.getOrElse(a, {
        val b = rec(YY(rec))(a)
        cache += a -> b
        b
      })
    }
    YY(rec) andThen (res => CYResult(res, cache.toMap))
  }

  type Lifted[+T] = () => T

  implicit class PipeWrapper[T](t: => T){
    def pipe[R](f: T => R): R = f(t)
    def |>[R](f: T => R): R = f(t)
  }

  implicit class LiftWrapper[T](t: =>T){
    def lift = () => t
    def lifted = lift
    def liftUnit = () => t: Unit
  }

  object ImplicitLift{
    implicit def implicitLift[T](t: T): Lifted[T] = t.lifted
  }

  implicit class FilteringHelpingWrapper[+A, +Repr](tr: TraversableLike[A, Repr]){
    private def filter[B](f: A => B, v: B) = tr.filter(e => f(e) == v)

    def filterMax[B](f: A => B)(implicit cmp: Ordering[B]): Repr = filter(f, tr.maxBy(f) |> f)

    def filterMin[B](f: A => B)(implicit cmp: Ordering[B]): Repr = filter(f, tr.minBy(f) |> f)
  }

  def elapsed[R](f: => R): (R, Duration) = {
    val time1 = System.nanoTime()
    val res = f
    val time2 = System.nanoTime()
    val dur = Duration(time2 - time1, NANOSECONDS)
    res -> dur
  }

  def tryo[R](f: => R): Either[R, Throwable] = try Left(f) catch { case th: Throwable => Right(th) }

  implicit class TryAsWrapper[T](t: T){
    def tryAs[A: TypeTag]: Option[A] = t match {
      case x: A => Option(x)
      case _ => None
    }
  }

  implicit class OptionWrapper[T](opt: Option[T]){
    def $(u: T => Unit): Option[T] = {
      opt foreach u
      opt
    }
  }

  implicit class TripleBuilder[T1, T2](tuple: (T1, T2)){
    def -->[T3](t: T3) = (tuple._1, tuple._2, t)
  }

  implicit class MapZipperWrapper[A, B](map: Map[A, B]){
    def zipByKey[C](m2: Map[A, C]): Map[A, (B, C)] = {
      assert(map.keySet == m2.keySet, s"maps have different keys: ${m2.keySet &~ map.keySet }")
      map.map{
        case (k, v) => k -> (v, m2(k))
      }.toMap
    }
  }

  implicit class ConditionalChainingWrapper[T](t: T){
    def `if`[R](cond: T => Boolean)(then: T => R)(`else`: T => R): R = if(cond(t)) then(t) else `else`(t)
    def `case`(cond: T => Boolean)(f: T => T): T = if(cond(t)) f(t) else t
    def `case`(cond: Boolean)(f: T => T): T = if(cond) f(t) else t
  }
}
