package feh.tec.agentes.tarea1

import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import scala.concurrent.duration._

class MapSpec extends Specification with ScalaCheck with Arbitraries{

  "The Map (effectively 'Environment')" should {
    "provide correct information about neighbouring tiles" in prop{
      (overseer: Overseer) =>
        val ref = overseer.ref
        val env = overseer.env
        val ag = Agents.MyDummyAgent.randomlyMoving(ref, 1 milli)
        val stopFunc = ag.execution()
        val res = for{
          tile <- env.tiles
          neighbour <- tile.neighbours
        } yield
          tile.neighbours.size mustEqual 4 and
          (neighbour.coordinate mustNotEqual tile.coordinate)

          /*and      // todo
          (
            math.abs(neighbour.coordinate._1 - tile.coordinate._1) == 1 or
            math.abs(neighbour.coordinate._2 - tile.coordinate._2) == 1
            )*/
        stopFunc()
        res.all
    }
  }


}
