package nicol

import akka.actor.ActorSystem
import akka.event.Logging
import akka.actor.ActorDSL._
import org.lwjgl.opengl._
import java.util.{Calendar, UUID}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import akka.pattern._
import feh.tec.util._
import scala.concurrent.Await
import org.lwjgl.BufferUtils
import scala.collection.mutable

/**
 * Modified for scala 2.10; since Actors frameworks is no longer a part of scala distribution, using akka actors.
 */
abstract class Game(entry: Scene){
  val name = Display.getTitle.replaceAll("\\s", "-")
  implicit val system = ActorSystem(s"${UUID.randomUUID()}-$name")

  protected val gameActor = actor(new Act {
    become{
//      case "screenshot" =>
//        println("received 'screenshot' message")
//        val s = screenshotInner()
//        sender ! s
      case "start" => entry.apply
      case "delayedStop" => system.scheduler.scheduleOnce(20 millis, self, "stop")
      case "stop" => system.stop(self)
    }

    whenStopping(onGameStop())
  })

  protected val log = Logging(system, gameActor)

  def main(args: Array[String]) = start

  def start = gameActor ! "start"
  def stop = gameActor ! "delayedStop"

  lazy val drawable = Display.getDrawable

/*
  def withContext[R](f: => R): R =
    if(tryo(GLContext.getCapabilities).isLeft) f
    else {
      drawable.makeCurrent()
      val res = f
      drawable.releaseContext()
      res
    }
*/

/*
  def screenshot(): Array[Byte] = withContext{
    println("screenshot inner")
    def time = Calendar.getInstance().getTimeInMillis
    val t0 = time
    GL11.glReadBuffer(GL11.GL_FRONT)
    val (width, height) = Display.getDisplayMode.getWidth -> Display.getDisplayMode.getHeight
    val bpp = 4 // Assuming a 32-bit display with a byte each for red, green, blue, and alpha.
    val buffer = BufferUtils.createByteBuffer(width * height * bpp)
    val t1 = time
    println(s"time elapsed 1 = ${t1-t0}")
    GL11.glReadPixels(0, 0, width, height, GL11.GL_RGBA, GL11.GL_UNSIGNED_BYTE, buffer)
    val t2 = time
    println(s"time elapsed 2 = ${t2-t1}")
    buffer.array()

  }
*/

  protected def onGameStop(){
    system.shutdown()
    sys.exit(0)
  }
}
