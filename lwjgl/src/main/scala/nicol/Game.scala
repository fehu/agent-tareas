package nicol

import akka.actor.ActorSystem
import akka.event.Logging
import akka.actor.ActorDSL._
import org.lwjgl.opengl.Display
import java.util.UUID
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Modified for scala 2.10; since Actors frameworks is no longer a part of scala distribution, using akka actors.
 */
abstract class Game(entry: Scene){
  val name = Display.getTitle.replaceAll("\\s", "-")
  implicit val system = ActorSystem(s"${UUID.randomUUID()}-$name")

  protected val theGame = actor(new Act {
    become{
      case "start" => entry.apply
      case "delayedStop" => system.scheduler.scheduleOnce(20 millis, self, "stop")
      case "stop" => system.stop(self)
    }

    whenStopping(onGameStop())
  })

  protected val log = Logging(system, theGame)

  def main(args: Array[String]) = start

  def start = theGame ! "start"
  def stop = theGame ! "delayedStop"

  protected def onGameStop(){
    system.shutdown()
    sys.exit(0)
  }
}
