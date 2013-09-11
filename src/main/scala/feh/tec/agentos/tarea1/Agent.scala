package feh.tec.agentos.tarea1

object Agent{
  type Position = Environment.Coordinate
  type EnvState = Environment.State
  type EnvGlobal = Environment.Global
  type Action = Environment.Action
  type Env = Environment

}

import Agent._
import feh.tec.agent.AgentExecutionLoop
import feh.tec.agent.DummyAgent
import feh.tec.agent.Agent

class DummyAgent[Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env]](val executionLoop: Exec)
  extends Agent[Position, EnvState, EnvGlobal, Action, Env, Exec] with feh.tec.agent.DummyAgent[Position, EnvState, EnvGlobal, Action, Env, Exec]
{
  def decide(currentPerception: Perception) = ???

  type Perception = AbstractGlobalPerception
  type DetailedPerception = AbstractDetailedPerception

  def env = ???

  def sense(env: EnvRef) = ???

  def detailed(env: EnvRef, c: Agent.Position) = ???

  def act(a: Agent.Action) = ???
}