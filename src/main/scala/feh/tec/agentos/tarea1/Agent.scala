package feh.tec.agentos.tarea1

import feh.tec.agent._
import feh.tec.agent.StatelessAgentPerformanceMeasure.Criterion
import feh.tec.map.{MapEnvironmentRef, InAbstractMapEnvironment}
import feh.tec.util.SideEffect


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
                 val executionLoop: Exec,
                 val performanceCriteria: Seq[Criterion[Position, EnvState, EnvGlobal, Action, Env, Measure]]
                  )
  extends Agent[Position, EnvState, EnvGlobal, Action, Env, Exec]
    with IdealRationalAgent[Position, EnvState, EnvGlobal, Action, Env, Exec, Measure]
    with InAbstractMapEnvironment[Position, EnvState, EnvGlobal, Action, Env, Tile, Map]
{
  agent: DecisiveAgent[Position, EnvState, EnvGlobal, Action, Env, Exec] =>

  lazy val measure = new Measure

  protected def calcPerformance(prediction: Env#Prediction) = measure.performance(prediction)(performanceCriteria)


  def detailed(env: EnvRef, c: Position): Option[DetailedPerception] = ???

  def lifetimeCycle: (EnvRef) => SideEffect[EnvRef] = ???

  def shortestRoute(from: Position, to: Position): Route[Position] = ???
}