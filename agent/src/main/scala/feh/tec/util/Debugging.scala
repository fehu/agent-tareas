package feh.tec.util

trait Debugging {
  outer =>

  def debug: Boolean

  protected implicit class DebugLogWrapper[R](r: => R){
    def debugLog(f: R => String): R = {
      val a = r
      outer.debugLog(f(a) + ": " + a)
      a
    }

    def debugLog(msg: String): R = debugLog(_ => msg)
  }

  def debugMessagePrefix: String

  protected def debugLog(a: => Any) = if(debug) println(debugMessagePrefix + a)
}

trait GlobalDebugging extends Debugging{
  protected def setup: DebuggingSetup

  def debug: Boolean = setup.debug
}

trait DebuggingSetup{
  def debug: Boolean
}

trait GlobalDebuggingSetup extends DebuggingSetup{
  private var _debug: Boolean = false

  def debug: Boolean = _debug
  def apply = debug
  def update(d: Boolean) = _debug = d
}

trait ThreadLoacalDebuggingSetup extends ThreadLocal[Boolean] with DebuggingSetup{
  def debug: Boolean = get()
}