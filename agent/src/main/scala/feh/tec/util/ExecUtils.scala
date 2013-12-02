package feh.tec.util

import scala.collection.mutable
import java.io.PipedOutputStream
import org.apache.commons.io.IOUtils

object ExecUtils extends ExecUtils
trait ExecUtils {
  private def exec(args: Seq[String]): Process = {
    println(s"## executing command: $args")
    sys.runtime.exec(args.toArray).register
  }
  def exec(cmd: String, args: String*): Process = {
    println(s"## executing command: $cmd, $args")
    exec(cmd +: args)
  }

  def sbt(cmd: String, args: String*) = exec("sbt" +: cmd +: args :+ "-no-colors")

  private implicit class RegisterWrapper(pr: Process){
    def register = {
      processes += pr
      IOUtils.copy(pr.getInputStream, System.out)
      IOUtils.copy(pr.getErrorStream, System.err)
      pr
    }
  }
  protected val processes = mutable.Buffer.empty[Process]

  object Process{
    def waitForAll() = processes.par.foreach(_.waitFor())
  }
}
