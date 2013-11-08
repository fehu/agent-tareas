package feh.tec

import scala.collection.TraversableLike
import scala.concurrent.duration._
import java.util.Calendar
import scala.reflect.runtime.universe._

package object util {
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
    val dur = Duration(time2 - time1, MILLISECONDS)
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
}
