package feh.tec.agentes.tarea1

import feh.tec.agentes.tarea1.Tarea1.Agents.{DummyExec, MyDummyAgent}
import feh.tec.agentes.tarea1.Agent.Action
import feh.tec.util.RandomWrappers._
import scala.concurrent.duration.FiniteDuration

object Agents {
  import Conf._

  object MyDummyAgent{
    def randomlyMoving(ref: Environment#Ref, freq: FiniteDuration) =
      new MyDummyAgent(ref, Nil, _ => _ => Set(), agentId, 0) { agent =>
        override def decide(currentPerception: Perception): Action = Move.all.toSeq.randomChoose
        override lazy val executionLoop: DummyExec = execLoopBuilder.buildExec(agent).copy(pauseBetweenExecs = freq)
      }
  }
}
