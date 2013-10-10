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
        val res = for(tile <- env.tiles) yield
          "provide all neighbours" >> { tile.neighbours.distinct.size mustEqual 4 } &&
          (for(neighbour <- tile.neighbours) yield
            "must not contain self" >> { neighbour.coordinate mustNotEqual tile.coordinate } &&
              "must be really neighbours" >> {
                val diffX = math.abs(neighbour.coordinate._1 - tile.coordinate._1)
                val diffY = math.abs(neighbour.coordinate._2 - tile.coordinate._2)
                ((diffX == 1 or diffX == env.coordinates.xRange.length - 1) and diffY == 0) or
                ((diffY == 1 or diffY == env.coordinates.yRange.length - 1) and diffX == 0)
              }
            ).all

      /*
            val res =
        for(tile <- env.tiles) yield
          tile.neighbours.size mustEqual 4 and
          (for(neighbour <- tile.neighbours) yield
            neighbour.coordinate mustNotEqual tile.coordinate and {
              val diffX = math.abs(neighbour.coordinate._1 - tile.coordinate._1)
              val diffY = math.abs(neighbour.coordinate._2 - tile.coordinate._2)
              ((diffX == 1 or diffX == env.coordinates.xRange.length - 1) and diffY == 0) or
              ((diffY == 1 or diffY == env.coordinates.yRange.length - 1) and diffX == 0)
            }
            ).all
       */
        stopFunc()
        res.all
    }
/*
    "provide distance calculating services" in prop{
      (overseer: Overseer) =>
        val ref = overseer.ref
        val env = overseer.env
        val ag = Agents.MyDummyAgent.randomlyMoving(ref, 1 milli)
        val stopFunc = ag.execution()
//        env.
    }
*/
  }


}
