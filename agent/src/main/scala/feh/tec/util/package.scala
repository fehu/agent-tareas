package feh.tec

import scala.collection.TraversableLike

package object util {
  implicit class PipeWrapper[T](t: => T){
    def pipe[R](f: T => R): R = f(t)
    def |>[R](f: T => R): R = f(t)
  }

  implicit class LiftWrapper[T](t: =>T){
    def lift = () => t
    def lifted = lift
    def liftUnit = () => t: Unit
  }

  implicit class FilteringHelpingWrapper[+A, +Repr](tr: TraversableLike[A, Repr]){
    private def filter[B](f: A => B, v: B) = tr.filter(e => f(e) == v)

    def filterMax[B](f: A => B)(implicit cmp: Ordering[B]): Repr = filter(f, tr.maxBy(f) |> f)

    def filterMin[B](f: A => B)(implicit cmp: Ordering[B]): Repr = filter(f, tr.minBy(f) |> f)
  }
}
