package feh.tec.agentes.tarea1

import feh.tec.agentes.tarea1.Tarea1.Agents.{InfExec, MyDummyAgent}
import feh.tec.agentes.tarea1.Agent.Action
import feh.tec.util.RandomWrappers._
import scala.concurrent.duration.FiniteDuration
import feh.tec.agent.{StatelessAgentPerformanceMeasure, AgentPerformanceMeasureCriterionValue}

object Agents {
  import Conf._
  import Tarea1.Agents.ExecLoopBuilders._
  import Agent._

  object MyDummyAgent{
    object RandomlyChosenCriteriaValue extends StatelessAgentPerformanceMeasure.CalculatedCriterion[Position, EnvState, EnvGlobal, Action, Env, Measure]("Randomly chosen", 0)

    def randomlyMoving(ref: Environment#Ref, freq: FiniteDuration) =
      new MyDummyAgent[InfExec](ref, Nil, _ => _ => Set(), agentId, -1) { agent =>
        override def decide(currentPerception: Perception): ExplainedAction = criterionExplainedAction(Move.all.toSeq.randomChoose, RandomlyChosenCriteriaValue :: Nil, 0)
        override lazy val executionLoop: InfExec = execLoopBuilder.buildExec(agent).copy(pauseBetweenExecs = freq)
      }
  }
}
