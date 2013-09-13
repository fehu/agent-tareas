package feh.tec.agent

import akka.actor.Actor
import feh.tec.util.{UUIDed, HasUUID, SideEffect}
import scala.concurrent.Future
import java.util.UUID

/**
 *  An agent that lacks decision part
 */
trait IndecisiveAgent[Position, EnvState, EnvGlobal, Action <: AbstractAction, Env <: Environment[Position, EnvState, EnvGlobal, Action, Env]]{
  trait AbstractGlobalPerception{
    def perceived: Seq[Position]
    def position: Position
  }

  trait AbstractDetailedPerception{
    type ActualDetailedPerception

    def where: Position
    def what: ActualDetailedPerception
  }

  type Perception <: AbstractGlobalPerception
  type DetailedPerception <: AbstractDetailedPerception

  type EnvRef = Env#Ref //<: EnvironmentRef[Position, EnvState, EnvGlobal, Action, Env]
  def env: EnvRef

  def sense(env: EnvRef): Perception
  def detailed(env: EnvRef, c: Position): Option[DetailedPerception]

  def act(a: Action): SideEffect[EnvRef]

  val id: AgentId = AgentId()
}

case class AgentId(uuid: UUID = UUID.randomUUID()) extends HasUUID

/** Abstract trait for agents execution;
 *  [[feh.tec.agent.DecisiveAgent]] and [[feh.tec.agent.Agent]] implementations make it necessary to mix-in at least one of execution patterns in them
 */
sealed trait AgentExecution[Position, EnvState, EnvGlobal, Action <: AbstractAction, Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                            Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env]]
  extends IndecisiveAgent[Position, EnvState, EnvGlobal, Action, Env]
{
  def executionLoop: Exec

  def lifetimeCycle: EnvRef => SideEffect[EnvRef]
  def execution: Exec#Execution = executionLoop.execution

  protected def executionSequence(decide: Perception => Action): EnvRef => SideEffect[EnvRef] = act _ compose decide compose sense
}



/** Abstract trait for agents with decision part;
 *  [[feh.tec.agent.Agent]] implementation makes it necessary to mix-in one of decision strategies in it
 */
sealed trait DecisiveAgent[Position, EnvState, EnvGlobal, Action <: AbstractAction, Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                           Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env]]{
  indecisiveSelf: AgentExecution[Position, EnvState, EnvGlobal, Action, Env, Exec] =>
}

/**
 *  Stupid agent, that makes decisions based only on current environment perception
 */
trait DummyAgent[Position, EnvState, EnvGlobal, Action <: AbstractAction, Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                 Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env]]
  extends DecisiveAgent[Position, EnvState, EnvGlobal, Action, Env, Exec]
{
  indecisiveSelf: AgentExecution[Position, EnvState, EnvGlobal, Action, Env, Exec] =>

  def decide(currentPerception: Perception): Action

  def lifetimeCycle = executionSequence(decide)
}

/**
 *  A wiser agent, that analyses the past
 */
trait WiserAgent[Position, EnvState, EnvGlobal, Action <: AbstractAction, Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                 Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env], Ag <: Agent[Position, EnvState, EnvGlobal, Action, Env, Exec]]
  extends DecisiveAgent[Position, EnvState, EnvGlobal, Action, Env, Exec]
{
  self: AgentExecution[Position, EnvState, EnvGlobal, Action, Env, Exec] with Ag =>

  def decide(past: Past[Position, EnvState, EnvGlobal, Action, Env, Ag], currentPerception: Perception): Action

  def past: Past[Position, EnvState, EnvGlobal, Action, Env, Ag]

  def lifetimeCycle = executionSequence(decide(past, _))
}

/**
 *  An agent with an inner state; no past participates in decision explicitly, but in can be stored in agent's inner state
 */
trait StatefulAgent[Position, EnvState, EnvGlobal, Action <: AbstractAction, Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                    Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env], AgState]
  extends DecisiveAgent[Position, EnvState, EnvGlobal, Action, Env, Exec]
{
  agent: AgentExecution[Position, EnvState, EnvGlobal, Action, Env, Exec] with ActorAgent[Position, EnvState, EnvGlobal, Action, Env, Exec] =>

  def state: AgState

  protected def changeState(f: AgState => AgState): SideEffect[AgState]

  def decide(state: AgState, currentPerception: Perception): Action

  def lifetimeCycle = executionSequence(decide(state, _))
}

/**
 *  A basic trait for an agent, that enforces usage of at least one of decision strategy trait ([[feh.tec.agent.DecisiveAgent]]'s child)
 */
trait Agent[Position, EnvState, EnvGlobal, Action <: AbstractAction, Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
            Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env]]
  extends AgentExecution[Position, EnvState, EnvGlobal, Action, Env, Exec]
{
  agent: DecisiveAgent[Position, EnvState, EnvGlobal, Action, Env, Exec] =>

  def act(a: Action) = env.blocking.affect(a)
}

/**
 *  An agent implemented using [[akka.actor.Actor]]
 */
trait ActorAgent[Position, EnvState, EnvGlobal, Action <: AbstractAction, Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                 Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env]]
  extends Actor with AgentExecution[Position, EnvState, EnvGlobal, Action, Env, Exec]
{
  agent: DecisiveAgent[Position, EnvState, EnvGlobal, Action, Env, Exec] =>
}

trait MeasuredAgent[Position, EnvState, EnvGlobal, Action <: AbstractAction, Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                    Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env],
                    M <: AgentMeasure[Position, EnvState, EnvGlobal, Action, Env]]{
  agent: DecisiveAgent[Position, EnvState, EnvGlobal, Action, Env, Exec] =>

  def measure: M
}

/**
    For each possible percept sequence, an ideal rational agent should do whatever action is expected to maximize its performance measure,
    on the basis of the evidence provided by the percept sequence and whatever built-in knowledge the agent has.
 */
trait IdealRationalAgent[Position, EnvState, EnvGlobal, Action <: AbstractAction,
                         Env <: Environment[Position, EnvState, EnvGlobal, Action, Env] with PredictableEnvironment[Position, EnvState, EnvGlobal, Action, Env],
                         Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env],
                         M <: AgentPerformanceMeasure[Position, EnvState, EnvGlobal, Action, Env]]
  extends IndecisiveAgent[Position, EnvState, EnvGlobal, Action, Env] with MeasuredAgent[Position, EnvState, EnvGlobal, Action, Env, Exec, M]
{
  agent: DecisiveAgent[Position, EnvState, EnvGlobal, Action, Env, Exec] =>

//  override type EnvRef = EnvironmentRef[Position, EnvState, EnvGlobal, Action, Env] with PredictableEnvironmentRef[Position, EnvState, EnvGlobal, Action, Env]


//  override def env: PredictableEnvironmentRef[Position, EnvState, EnvGlobal, Action, Env]

  protected def calcPerformance(prediction: Env#Prediction): M#Measure

  def selectTheBestBehavior(possibleActions: List[Action]): Action = {
    for{
      a <- possibleActions
      prediction = env.predict(a)
      performance = calcPerformance(prediction)
    } yield a -> performance
  }.maxBy(_._2)(measure.measureNumeric.asInstanceOf[Numeric[M#Measure]])._1
}
