package feh.tec.agentos.tarea1

import feh.tec.agent._
import feh.tec.agent.StatelessAgentPerformanceMeasure.Criterion
import feh.tec.map.{MapStateBuilder, MapEnvironmentRef, InAbstractMapEnvironment}
import feh.tec.util.SideEffect
import scala.concurrent.duration.FiniteDuration


object Agent{
  type Position = Environment.Coordinate
  type EnvState = Environment.State
  type EnvGlobal = Environment.Global
  type Action = Environment.Action
  type Env = Environment

  type Tile = Environment.Tile

  class Measure extends StatelessAgentPerformanceDoubleMeasure[Position, EnvState, EnvGlobal, Action, Env, Measure]
}

import Agent._

abstract class AbstractAgent[Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env]]
                (val env: AbstractAgent[Exec]#EnvRef,
                 val performanceCriteria: Seq[Criterion[Position, EnvState, EnvGlobal, Action, Env, Measure]],
                 val mapStateBuilder: MapStateBuilder[Position, Tile, Map, EnvState])
                (implicit execLoopBuilder: ExecLoopBuilder[AbstractAgent[Exec], Exec])
  extends Agent[Position, EnvState, EnvGlobal, Action, Env, Exec]
    with IdealRationalAgent[Position, EnvState, EnvGlobal, Action, Env, Exec, Measure]
    with InAbstractMapEnvironment[Position, EnvState, EnvGlobal, Action, Env, Tile, Map]
{
  agent: DecisiveAgent[Position, EnvState, EnvGlobal, Action, Env, Exec] =>

  lazy val measure = new Measure

  protected def calcPerformance(prediction: Env#Prediction) = measure.performance(prediction)(performanceCriteria)

  lazy val executionLoop: Exec = execLoopBuilder.buildExec(agent)

  @deprecated("mix in ShortestRouteFinder to Map")
  def shortestRoute(from: Position, to: Position): Route[Position] = ???
}

trait ExecLoopBuilder[Ag <: AbstractAgent[AgentInfiniteExecution[Position, EnvState, EnvGlobal, Action, Env, Ag]],
                      Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env]]{
  def buildExec(ag: Ag): Exec
}

class AgentInfiniteExecLoopBuilder[Ag <: AbstractAgent[AgentInfiniteExecution[Position, EnvState, EnvGlobal, Action, Env, Ag]]]
  (pauseBetweenExecs: FiniteDuration, stopTimeout: FiniteDuration)
  extends ExecLoopBuilder[Ag, AgentInfiniteExecution[Position, EnvState, EnvGlobal, Action, Env, Ag]]
{
  outer =>

  def buildExec(ag: Ag) =
    new AgentInfiniteExecution[Position, EnvState, EnvGlobal, Action, Env, Ag]{
      def agent: Ag = ag
      def pauseBetweenExecs: FiniteDuration = outer.pauseBetweenExecs
      def stopTimeout: FiniteDuration = outer.stopTimeout
    }
}
  