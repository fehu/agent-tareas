package feh.tec.util

trait StateInScope[T]{
  protected def default: T

  private val _state = new ThreadLocal[T]{
    override def initialValue(): T = default
  }

  protected def state_=(t: T) = _state.set(t)

  def get: T = _state.get
  def state = get

  def doWith[R](t: T, f: T => R): R = doWith(t)(f(t))
  def doWith[R](t: T)(r: => R): R = {
    val old = get
    state = t
    val res = r
    state = old
    res
  }
}


class ScopedState[T](protected val default: T) extends StateInScope[T]