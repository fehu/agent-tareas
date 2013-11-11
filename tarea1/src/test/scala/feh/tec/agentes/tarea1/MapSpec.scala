package feh.tec.agentes.tarea1

import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import scala.concurrent.duration._
import feh.tec.world.Simple2dDirection

class MapSpec extends Specification with ScalaCheck with Arbitraries{
  import Conf._

  "The Map (effectively 'Environment')" should {
    "provide correct information about neighbouring tiles" in prop{
      (overseer: Overseer) =>
        val ref = overseer.ref
        val env = overseer.env
        val ag = Agents.MyDummyAgent.randomlyMoving(ref, 1 milli)
        val stopFunc = ag.execution()
        val res = for(tile <- env.atoms) yield
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

        stopFunc()
        res.all
    }

    "provide correct relative tile positions" in {
      import Simple2dDirection._
      val map = Maps.randomMap(0 until 10, 0 until 10, Some(agentId))

      Seq(
        map.relativePosition(2 -> 5, 2 -> 5) must beEmpty,

        map.relativePosition(6 -> 5, 2 -> 5) mustEqual Set(Right),
        map.relativePosition(2 -> 5, 6 -> 5) mustEqual Set(Left),

        map.relativePosition(7 -> 5, 2 -> 5) mustEqual Set(Left, Right),
        map.relativePosition(2 -> 5, 7 -> 5) mustEqual Set(Left, Right),

        map.relativePosition(7 -> 4, 2 -> 5) mustEqual Set(Left, Right, Up),
        map.relativePosition(2 -> 5, 7 -> 4) mustEqual Set(Left, Right, Down),

        map.relativePosition(6 -> 4, 2 -> 5) mustEqual Set(Right, Up),
        map.relativePosition(2 -> 5, 6 -> 4) mustEqual Set(Left, Down)
      )
      .reduceLeft(_ and _)
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
