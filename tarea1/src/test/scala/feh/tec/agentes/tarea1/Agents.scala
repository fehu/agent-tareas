package feh.tec.agentes.tarea1

import feh.tec.agentes.tarea1.Tarea1.Agents.{InfExec, MyDummyAgent}
import feh.tec.agentes.tarea1.Agent.Action
import feh.tec.util.RandomWrappers._
import scala.concurrent.duration.FiniteDuration

object Agents {
  import Conf._
  import Tarea1.Agents.ExecLoopBuilders._

  object MyDummyAgent{
    def randomlyMoving(ref: Environment#Ref, freq: FiniteDuration) =
      new MyDummyAgent[InfExec](ref, Nil, _ => _ => Set(), agentId, -1) { agent =>
        override def decide(currentPerception: Perception): Action = Move.all.toSeq.randomChoose
        override lazy val executionLoop: InfExec = execLoopBuilder.buildExec(agent).copy(pauseBetweenExecs = freq)
      }
  }
}
