package feh.tec

import scala.reflect.runtime.universe._

package object util extends feh.util.Util{
  implicit class TryAsWrapper[T](t: T){
    def tryAs[A: TypeTag]: Option[A] = t match {
      case x: A => Option(x)
      case _ => None
    }
  }
}
