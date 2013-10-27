package feh.tec.agentes.tarea1

import feh.tec.agentes.tarea1.Tarea1.Agents.{InfExec, MyDummyAgent}
import feh.tec.util.RandomWrappers._
import scala.concurrent.duration.FiniteDuration
import feh.tec.agent.StatelessAgentPerformanceMeasure
import feh.tec.agent.AgentDecision.CriteriaReasonedDecision

object Agents {
  import Conf._
  import Tarea1.Agents.ExecLoopBuilders._
  import Agent._

  object MyDummyAgent{
    object RandomlyChosenCriteriaValue extends StatelessAgentPerformanceMeasure.CalculatedCriterion[Position, EnvState, EnvGlobal, Action, Env, Measure]("Randomly chosen", 0)

    def randomlyMoving(ref: Environment#Ref, freq: FiniteDuration) ={
      implicit def pauseBetweenExecs = PauseBetweenExecs(freq)

      new MyDummyAgent[InfExec](ref, Nil, Nil, _ => _ => Set(), agentId, -1) { agent =>

        override def decide(currentPerception: Perception): ActionExplanation =
          CriteriaReasonedDecision[Position, EnvState, EnvGlobal, Action, Env, InfExec, Measure](Move.all.toSeq.randomChoose, Set(RandomlyChosenCriteriaValue), 0)
      }
    }
  }
}
