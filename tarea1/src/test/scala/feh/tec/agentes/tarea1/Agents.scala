package feh.tec.agentes.tarea1

import feh.tec.agentes.tarea1.Tarea1.Agents.{InfExec, MySimpleAgent}
import feh.util.RandomWrappers._
import scala.concurrent.duration.FiniteDuration
import feh.tec.agent.StatelessAgentPerformanceMeasure
import feh.tec.agent.AgentDecision.CriteriaReasonedDecision

object Agents {
  import Conf._
  import Tarea1.Agents.ExecLoopBuilders._
  import Agent._

  object MyDummyAgent{
    object RandomlyChosenCriteriaValue extends StatelessAgentPerformanceMeasure.CalculatedCriterion[Env, Measure]("Randomly chosen", 0)

    def randomlyMoving(ref: Environment#Ref, freq: FiniteDuration) ={
      implicit def pauseBetweenExecs = PauseBetweenExecs(freq)

      new MySimpleAgent[InfExec](ref, Nil, Nil, _ => _ => Set(), agentId, -1, null) { agent =>

        override def decide(currentPerception: Perception): ActionExplanation =
          CriteriaReasonedDecision[Env, Measure](Move.all.toSeq.randomChoice, Set(RandomlyChosenCriteriaValue), 0)
      }
    }
  }
}
