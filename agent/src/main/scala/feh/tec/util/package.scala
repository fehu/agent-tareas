package feh.tec

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
}
