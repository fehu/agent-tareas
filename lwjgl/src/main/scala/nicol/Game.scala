package nicol

import akka.actor.ActorSystem
import akka.event.Logging
import akka.actor.ActorDSL._
import org.lwjgl.opengl._
import java.util.UUID
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Modified for scala 2.10; since Actors frameworks is no longer a part of scala distribution, using akka actors.
 */
abstract class Game(entry: Scene){
  val name = Display.getTitle.replaceAll("\\s", "-").filter(ch => ch.isLetter || ch.isDigit || ch == '-')
  implicit val system = ActorSystem(s"${UUID.randomUUID()}-$name")

  protected val gameActor = actor(new Act {
    become{
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

  protected def onGameStop(){
    system.shutdown()
    sys.exit(0)
  }
}
