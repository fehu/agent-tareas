package feh.tec.util

trait StateInScope[T]{
  def default: T

  protected var state = default
  def get: T = state

//  def doWith[R](t: T, r: => R): R = doWith(t)(r)
  def doWith[R](t: T)(r: => R): R = {
    val old = get
    state = t
    val res = r
    state = old
    res
  }
}


class ScopedState[T](_default: T) extends StateInScope[T]{
  def default: T = _default
}