package feh.tec.agentos.tarea1

import feh.tec.agent._
import feh.tec.agent.StatelessAgentPerformanceMeasure.Criterion
import feh.tec.map.{ShortestRouteFinder, MapStateBuilder, MapEnvironmentRef, InAbstractMapEnvironment}
import feh.tec.util.SideEffect
import scala.concurrent.duration.FiniteDuration
import akka.actor.Actor


object Agent{
  type Position = Environment.Coordinate
  type EnvState = Environment.State
  type EnvGlobal = Environment.Global
  type Action = Environment.Action
  type Env = Environment

  type Tile = Environment.Tile

  type Easel = Environment.Easel // todo: shouldn't be here

  class Measure extends StatelessAgentPerformanceDoubleMeasure[Position, EnvState, EnvGlobal, Action, Env, Measure]
}

import Agent._

abstract class AbstractAgent[Exec <: ActorAgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env, AbstractAgent[Exec]]]
                (val env: AbstractAgent[Exec]#EnvRef,
                 val performanceCriteria: Seq[Criterion[Position, EnvState, EnvGlobal, Action, Env, Measure]],
                 val mapStateBuilder: MapStateBuilder[Position, Tile, Map, EnvState],
                 val shortestRouteFinder: ShortestRouteFinder[Map, Tile, Position])
                (implicit execLoopBuilder: ExecLoopBuilder[AbstractAgent[Exec], Exec])
  extends Agent[Position, EnvState, EnvGlobal, Action, Env, Exec] with ActorAgent[Position, EnvState, EnvGlobal, Action, Env, Exec]
    with IdealRationalAgent[Position, EnvState, EnvGlobal, Action, Env, Exec, Measure]
    with InAbstractMapEnvironment[Position, EnvState, EnvGlobal, Action, Env, Tile, Map]
{
  agent: DecisiveAgent[Position, EnvState, EnvGlobal, Action, Env, Exec] =>

/*
  AgentApp.Agents.DummyExec <: feh.tec.agent.AgentExecutionLoop[(Int, Int),Agent.EnvState,Agent.EnvGlobal,Agent.Action,Agent.Env]
   */
  lazy val measure = new Measure

  protected def calcPerformance(prediction: Env#Prediction) = measure.performance(prediction)(performanceCriteria)

  lazy val executionLoop: Exec = execLoopBuilder.buildExec(agent)

  def receive: Actor.Receive = executionLoop.receive
}

trait ExecLoopBuilder[Ag <: AbstractAgent[Exec], Exec <: ActorAgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env, Ag]]{
  def buildExec(ag: Ag): Exec
}

class AgentInfiniteExecLoopBuilder[Ag <: AbstractAgent[AgentInfiniteExecution[Position, EnvState, EnvGlobal, Action, Env, Ag]]]
  (pauseBetweenExecs: FiniteDuration, stopTimeout: FiniteDuration)
  extends ExecLoopBuilder[Ag, AgentInfiniteExecution[Position, EnvState, EnvGlobal, Action, Env, Ag]]
{
  outer =>
  /*
  type arguments [(Int, Int),Agent.EnvState,Agent.EnvGlobal,Agent.Action,Agent.Env,Ag] do not conform to trait AgentInfiniteExecution's type parameter bounds
  [Position,
    EnvState,
    EnvGlobal,
    Action <: AbstractAction,
    Env <: Environment[Position,EnvState,EnvGlobal,Action,Env],
    Ag <: ActorAgent[Position, EnvState, EnvGlobal, Action, Env, _ <: ActorAgentExecutionLoop[Position,EnvState,EnvGlobal,Action,Env,Ag]]
    ]
   */
  def buildExec(ag: Ag) =
    new AgentInfiniteExecution[Position, EnvState, EnvGlobal, Action, Env, Ag]{
      def agent: Ag = ag
      def pauseBetweenExecs: FiniteDuration = outer.pauseBetweenExecs
      def stopTimeout: FiniteDuration = outer.stopTimeout
    }
}
  