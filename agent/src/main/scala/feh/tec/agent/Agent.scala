package feh.tec.agent

import akka.actor.ActorRef
import feh.tec.util._
import java.util.UUID
import feh.tec.agent.AgentDecision._
import feh.tec.agent.IdealRationalAgentDecisionStrategies.MeasureBasedDecisionStrategy

/**
 *  An agent that lacks decision part
 */
trait IndecisiveAgent[Env <: Environment[Env]]{
  trait AbstractGlobalPerception{
    def perceived: Seq[Env#Coordinate]
    def position: Env#Coordinate
  }

  trait AbstractDetailedPerception{
    type ActualDetailedPerception

    def where: Env#Coordinate
    def what: ActualDetailedPerception
  }

  type Perception //<: AbstractGlobalPerception
  type DetailedPerception //<: AbstractDetailedPerception

  type EnvRef = Env#Ref
  def env: EnvRef

  def sense(env: EnvRef): Perception
  def detailed(env: EnvRef, c: Env#Coordinate): Option[DetailedPerception]

  def act(a: Env#Action): SideEffect[EnvRef]

  val id: AgentId = AgentId()
}

case class AgentId(uuid: UUID = UUID.randomUUID()) extends HasUUID

/** Abstract trait for agents execution;
 *  [[feh.tec.agent.DecisiveAgent]] and [[feh.tec.agent.Agent]] implementations make it necessary to mix-in at least one of execution patterns in them
 */
sealed trait AgentExecution[Env <: Environment[Env], +Exec <: AgentExecutionLoop] extends IndecisiveAgent[Env]{
  def executionLoop: Exec

  def lifetimeCycle: EnvRef => SideEffect[EnvRef]
  def execution: Exec#Execution = executionLoop.execution

  type ActionExplanation <: ExplainedAction[Env#Action]

  def notifyDecision(a: ActionExplanation)
  def lastDecision: Option[ActionExplanation]

  protected def executionSequence(decide: Perception => ActionExplanation): EnvRef => SideEffect[EnvRef] =
    sense _ andThen decide andThen {expl => notifyDecision(expl); expl.action} andThen act
}

trait SimultaneousAgentExecution[Env <: Environment[Env], Exec <: SimultaneousAgentsExecutor]
  extends AgentExecution[Env, Exec]
{

}

/** Abstract trait for agents with decision part;
 *  [[feh.tec.agent.Agent]] implementation makes it necessary to mix-in one of decision strategies in it
 */
sealed trait DecisiveAgent[Env <: Environment[Env], +Exec <: AgentExecutionLoop]{
  indecisiveSelf: AgentExecution[Env, Exec] =>
}

/**
 *  Stupid agent, that makes decisions based only on current environment perception
 */
trait DummyAgent[Env <: Environment[Env], +Exec <: AgentExecutionLoop] extends DecisiveAgent[Env, Exec]{
  indecisiveSelf: AgentExecution[Env, Exec] =>

  def decide(currentPerception: Perception): ActionExplanation

  def lifetimeCycle = executionSequence(decide)
}

/**
 *  A wiser agent, that analyses the past
 */
trait WiserAgent[Env <: Environment[Env], Exec <: AgentExecutionLoop] extends DecisiveAgent[Env, Exec]{
  self: AgentExecution[Env, Exec] =>

  def decide(past: Past[Env], currentPerception: Perception): ActionExplanation

  def past: Past[Env]

  def lifetimeCycle = executionSequence(decide(past, _))
}

/**
 *  An agent with an inner state; no past participates in decision explicitly, but in can be stored in agent's inner state
 */
trait StatefulAgent[Env <: Environment[Env], Exec <: AgentExecutionLoop, AgState] extends DecisiveAgent[Env, Exec]{
  agent: AgentExecution[Env, Exec] with AgentWithActor[Env, Exec] =>

  def state: AgState

  protected def changeState(f: AgState => AgState): SideEffect[AgState]

  def decide(state: AgState, currentPerception: Perception): ActionExplanation

  def lifetimeCycle = executionSequence(decide(state, _))
}

/**
 *  A basic trait for an agent, that enforces usage of at least one of decision strategy trait ([[feh.tec.agent.DecisiveAgent]]'s child)
 */
trait Agent[Env <: Environment[Env], Exec <: AgentExecutionLoop] extends AgentExecution[Env, Exec]{
  agent: DecisiveAgent[Env, Exec] =>

  def act(a: Env#Action) = env.blocking.affect(a)
}

/**
 *  An agent implemented using [[akka.actor.Actor]]
 */
trait AgentWithActor[Env <: Environment[Env], +Exec <: AgentExecutionLoop] extends AgentExecution[Env, Exec]{
  agent: DecisiveAgent[Env, Exec] =>

  def actorRef: ActorRef
}

trait MeasuredAgent[Env <: Environment[Env], Exec <: AgentExecutionLoop, M <: AgentMeasure[Env, M]]{
  agent: DecisiveAgent[Env, Exec] =>

  def measure: M
}

/**
    For each possible percept sequence, an ideal rational agent should do whatever action is expected to maximize its performance measure,
    on the basis of the evidence provided by the percept sequence and whatever built-in knowledge the agent has.
 */
trait IdealRationalAgent[Env <: Environment[Env] with PredictableEnvironment[Env],
                         Exec <: AgentExecutionLoop,
                         M <: AgentPerformanceMeasure[Env, M]]
  extends IndecisiveAgent[Env] with MeasuredAgent[Env, Exec, M]
{
  agent: DecisiveAgent[Env, Exec] with AgentExecution[Env, Exec] =>

  def calcPerformance(prediction: Env#Prediction): Seq[M#CriterionValue]

  def withCriteria[R](c: M#Criteria)(f: => R): R

  type ActionExplanation = CriteriaReasonedDecision[Env, M]

  type DecisionArg = (agent.type, Set[Env#Action])

  protected def agentPredictedPosition(p: Env#Prediction): Env#Coordinate

  protected def createBehaviorSelectionStrategy:
    DecisionStrategy[Env#Action, DecisionArg, ExtendedCriteriaBasedDecision[ActionExplanation, Env, M]] =
      new MeasureBasedDecisionStrategy[Env, M, agent.type](agentPredictedPosition)
  lazy val behaviorSelectionStrategy = createBehaviorSelectionStrategy
  
  def chooseTheBestBehavior(possibleActions: Set[Env#Action]): ActionExplanation =
    behaviorSelectionStrategy.decide((agent -> possibleActions).asInstanceOf[DecisionArg]).decision

}

/**
 * LOL the name
 */
trait IdealDummyAgent[Env <: Environment[Env] with PredictableEnvironment[Env],
Exec <: AgentExecutionLoop,
                      M <: AgentPerformanceMeasure[Env, M]]
  extends IdealRationalAgent[Env, Exec, M] with DummyAgent[Env, Exec]
{
  self: AgentExecution[Env, Exec] =>

  def possibleBehaviors(currentPerception: Perception): Set[Env#Action]

  def decide(currentPerception: Perception): ActionExplanation = chooseTheBestBehavior(possibleBehaviors(currentPerception).ensuring(_.nonEmpty, "no possible action"))
}


trait IdealForeseeingDummyAgent[Env <: Environment[Env] with ForeseeableEnvironment[Env],
                                Exec <: AgentExecutionLoop,
                                M <: AgentPerformanceMeasure[Env, M]]
  extends IdealDummyAgent[Env, Exec, M] with Debugging
{
  self: AgentExecution[Env, Exec] =>

  def foreseeingDepth: Int

  def perceiveFromSnapshot(sn: EnvironmentSnapshot[Env]): Perception

  protected def notifyRouteChosen: Option[Seq[Env#Action]] => Unit

  override protected def createBehaviorSelectionStrategy: DecisionStrategy[Env#Action, DecisionArg, ExtendedCriteriaBasedDecision[ActionExplanation, Env, M]]
    = new IdealForeseeingAgentDecisionStrategies.MeasureBasedForeseeingDecisionStrategy[Env, M, self.type](foreseeingDepth, debug, notifyRouteChosen)
}
