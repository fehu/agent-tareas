package nicol

import akka.actor.{ActorRef, ActorSystem}
import akka.event.Logging
import akka.actor.ActorDSL._
import org.lwjgl.opengl.Display
import java.util.{Calendar, UUID}

/**
 * Modified for scala 2.10; since Actors frameworks is no longer a part of scala distribution, using akka actors.
 */
abstract class Game(entry: Scene){
  val name = Display.getTitle.replaceAll("\\s", "-")
  implicit val system = ActorSystem(s"${UUID.randomUUID()}-$name")

  protected val theGame = actor(new Act {
    become{
      case "start" => entry.apply
      case "delayedStop" => delayedNotifyActor ! "stop" -> 1000
      case "stop" =>
        system.stop(self)
    }

    whenStopping(onGameStop())
  })

  protected lazy val delayedNotifyActor = actor(new Act {
    var actorToNotify: ActorRef = _
    var message: String = _

    var locked = false

    def sleepDelay = 100

    become{
      case _: (String, Int) if locked => sender ! "busy"
      case (msg: String, delay: Int) =>
        actorToNotify = sender
        message = msg
        locked = true
        self ! (Calendar.getInstance.getTimeInMillis + delay)
      case t: Long if Calendar.getInstance().getTimeInMillis >= t =>
        actorToNotify ! message
        locked = false
      case t: Long =>
        Thread.sleep(sleepDelay)
        self ! t
    }
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
