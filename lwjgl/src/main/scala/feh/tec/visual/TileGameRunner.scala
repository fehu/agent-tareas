package feh.tec.visual

import akka.actor.ActorDSL._
import akka.actor.ActorSystem
import akka.event.Logging
import org.lwjgl.opengl.Display
import java.util.UUID
import feh.tec.visual.api.TileGame

@deprecated("use nicol")
class TileGameRunner(val game: TileGame) extends App{
  val name = Display.getTitle.replaceAll("\\s", "-")
  implicit val system = ActorSystem(s"${UUID.randomUUID()}-$name")

  val theGame = actor(new Act {
    become{
      case "start" => game.run()
    }
  })

  val log = Logging(system, theGame)

  theGame ! "start"
}
