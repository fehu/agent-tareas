package feh.tec.agent

import akka.actor.Actor
import feh.tec.util.SideEffect
import scala.concurrent.Future

/**
 *  An agent that lacks decision part
 */
trait IndecisiveAgent[Position, EnvState, EnvGlobal, Action <: AbstractAction, Env <: Environment[Position, EnvState, EnvGlobal, Action, Env]] {
  trait AbstractPerception
  trait AbstractDetailedPerception

  type Perception <: AbstractPerception
  type DetailedPerception <: AbstractDetailedPerception

  type EnvRef = EnvironmentRef[Position, EnvState, EnvGlobal, Action, Env]
  def env: EnvRef


  def sense(env: EnvRef): Perception
  def sense(env: EnvRef, c: Position): Option[DetailedPerception]

  def act(a: Action): SideEffect[EnvRef]
}

/** Abstract trait for agents execution;
 *  [[feh.tec.agent.DecisiveAgent]] and [[feh.tec.agent.Agent]] implementations make it necessary to mix-in at least one of execution patterns in them
 */
sealed trait AgentExecution[Position, EnvState, EnvGlobal, Action <: AbstractAction, Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                            Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env]]
  extends IndecisiveAgent[Position, EnvState, EnvGlobal, Action, Env]
{
  def executionPattern: Exec

  def executionSequence: EnvRef => Action
  def execution: Exec#Execution = executionPattern.execution
}



/** Abstract trait for agents with decision part;
 *  [[feh.tec.agent.Agent]] implementation makes it necessary to mix-in one of decision strategies in it
 */
sealed trait DecisiveAgent[Position, EnvState, EnvGlobal, Action <: AbstractAction, Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                           Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env]]{
  indecisiveSelf: AgentExecution[Position, EnvState, EnvGlobal, Action, Env, Exec] =>
}

/**
 *  Stupid agent, that makes decisions based only on current environment state
 */
trait DummyAgent[Position, EnvState, EnvGlobal, Action <: AbstractAction, Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                 Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env]]
  extends AgentExecution[Position, EnvState, EnvGlobal, Action, Env, Exec]
{
  indecisiveSelf: IndecisiveAgent[Position, EnvState, EnvGlobal, Action, Env] =>

  def decide(currentEnv: EnvRef): Action
}

/**
 *  A wiser agent, that analyses the past
 */
trait WiserAgent[Position, EnvState, EnvGlobal, Action <: AbstractAction, Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                 Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env], Ag <: Agent[Position, EnvState, EnvGlobal, Action, Env, Exec]]
  extends DecisiveAgent[Position, EnvState, EnvGlobal, Action, Env, Exec]
{
  self: AgentExecution[Position, EnvState, EnvGlobal, Action, Env, Exec] with Ag =>

  def decide(past: Past[Position, EnvState, EnvGlobal, Action, Env, Ag], currentEnv: EnvRef): Action
}

/**
 *  An agent with an inner state
 */
trait StatefulAgent[Position, EnvState, EnvGlobal, Action <: AbstractAction, Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                    Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env], AgState]
  extends DecisiveAgent[Position, EnvState, EnvGlobal, Action, Env, Exec]
{
  agent: AgentExecution[Position, EnvState, EnvGlobal, Action, Env, Exec] with ActorAgent[Position, EnvState, EnvGlobal, Action, Env, Exec] =>

  def state: AgState

  protected def changeState(f: AgState => AgState): SideEffect[AgState]

  def decide(state: AgState, currentEnv: EnvRef): Action
}

/**
 *  A basic trait for an agent, that enforces usage of at least one of decision strategy trait ([[feh.tec.agent.DecisiveAgent]]'s child)
 */
trait Agent[Position, EnvState, EnvGlobal, Action <: AbstractAction, Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
            Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env]]
  extends IndecisiveAgent[Position, EnvState, EnvGlobal, Action, Env]
{
  self: DecisiveAgent[Position, EnvState, EnvGlobal, Action, Env, Exec] =>
}

/**
 *  An agent implemented using [[akka.actor.Actor]]
 */
trait ActorAgent[Position, EnvState, EnvGlobal, Action <: AbstractAction, Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                 Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env]]
  extends Actor with IndecisiveAgent[Position, EnvState, EnvGlobal, Action, Env]
{
  agent: DecisiveAgent[Position, EnvState, EnvGlobal, Action, Env, Exec] =>
}