package feh.tec.agent

import akka.actor.ActorRef
import feh.tec.util._
import java.util.UUID
import scala.collection.mutable
import RandomWrappers._

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

  type EnvRef = Env#Ref
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
  type ExplainedAction

  def executionLoop: Exec

  def lifetimeCycle: EnvRef => SideEffect[EnvRef]
  def execution: Exec#Execution = executionLoop.execution

  def notifyDecision(a: ExplainedAction)

  protected def actionFromExplained(ex: ExplainedAction): Action

  protected def executionSequence(decide: Perception => ExplainedAction): EnvRef => SideEffect[EnvRef] =
    sense _ andThen decide andThen {expl => notifyDecision(expl); actionFromExplained(expl)} andThen act
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

  def decide(currentPerception: Perception): ExplainedAction

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

  def decide(past: Past[Position, EnvState, EnvGlobal, Action, Env, Ag], currentPerception: Perception): ExplainedAction

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
  agent: AgentExecution[Position, EnvState, EnvGlobal, Action, Env, Exec] with AgentWithActor[Position, EnvState, EnvGlobal, Action, Env, Exec] =>

  def state: AgState

  protected def changeState(f: AgState => AgState): SideEffect[AgState]

  def decide(state: AgState, currentPerception: Perception): ExplainedAction

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
trait AgentWithActor[Position, EnvState, EnvGlobal, Action <: AbstractAction, Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                 Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env]]
  extends AgentExecution[Position, EnvState, EnvGlobal, Action, Env, Exec]
{
  agent: DecisiveAgent[Position, EnvState, EnvGlobal, Action, Env, Exec] =>

  def actorRef: ActorRef
}

trait MeasuredAgent[Position, EnvState, EnvGlobal, Action <: AbstractAction, Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                    Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env],
                    M <: AgentMeasure[Position, EnvState, EnvGlobal, Action, Env, M]]{
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
                         M <: AgentPerformanceMeasure[Position, EnvState, EnvGlobal, Action, Env, M]]
  extends IndecisiveAgent[Position, EnvState, EnvGlobal, Action, Env] with MeasuredAgent[Position, EnvState, EnvGlobal, Action, Env, Exec, M]
{
  agent: DecisiveAgent[Position, EnvState, EnvGlobal, Action, Env, Exec] with AgentExecution[Position, EnvState, EnvGlobal, Action, Env, Exec] =>

  protected def calcPerformance(prediction: Env#Prediction): Seq[M#CriterionValue]

  protected def criterionExplainedAction(a: Action, c: M#CriteriaValue, m: M#Measure): ExplainedAction
  protected def explainedActionPerformance: ExplainedAction => M#Measure

  def chooseTheBestBehavior(possibleActions: Set[Action]): ExplainedAction = {
    implicit val num = measure.measureNumeric.asInstanceOf[Numeric[M#Measure]]
    (
      for{
        a <- possibleActions
        prediction = env.predict(a)
        vCriterion = calcPerformance(prediction)
        performance = vCriterion.map(_.value).sum
      } yield criterionExplainedAction(a, vCriterion, performance)
    ).maxBy(explainedActionPerformance)
  }
}

/**
 * LOL the name
 */
trait IdealDummyAgent[Position, EnvState, EnvGlobal, Action <: AbstractAction,
                      Env <: Environment[Position, EnvState, EnvGlobal, Action, Env] with PredictableEnvironment[Position, EnvState, EnvGlobal, Action, Env],
                      Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env],
                      M <: AgentPerformanceMeasure[Position, EnvState, EnvGlobal, Action, Env, M]]
  extends IdealRationalAgent[Position, EnvState, EnvGlobal, Action, Env, Exec, M] with DummyAgent[Position, EnvState, EnvGlobal, Action, Env, Exec]
{
  self: AgentExecution[Position, EnvState, EnvGlobal, Action, Env, Exec] =>

  def possibleBehaviors(currentPerception: Perception): Set[Action]

  def decide(currentPerception: Perception): ExplainedAction = chooseTheBestBehavior(possibleBehaviors(currentPerception).ensuring(_.nonEmpty, "no possible action"))
}


trait IdealForeseeingDummyAgent[Position, EnvState, EnvGlobal, Action <: AbstractAction,
                                Env <: Environment[Position, EnvState, EnvGlobal, Action, Env] with ForeseeableEnvironment[Position, EnvState, EnvGlobal, Action, Env],
                                Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env],
                                M <: AgentPerformanceMeasure[Position, EnvState, EnvGlobal, Action, Env, M]]
  extends IdealDummyAgent[Position, EnvState, EnvGlobal, Action, Env, Exec, M]
{
  self: AgentExecution[Position, EnvState, EnvGlobal, Action, Env, Exec] =>

  type ExplainedAction = CriteriaReasonedDecision

  def foreseeingDepth: Int

  protected def snapshotToPerception(sn: EnvironmentSnapshot[Position, EnvState, EnvGlobal, Action, Env]): Perception

  protected val stackedDecisions = mutable.Queue.empty[CriteriaReasonedDecisions]
  protected var currentDecisions: CriteriaReasonedDecisions = _
  protected val executingDecisions = mutable.Queue.empty[CriteriaReasonedDecision]

  override def decide(currentPerception: Perception): CriteriaReasonedDecision = {
    if(executingDecisions.isEmpty) {
      if(stackedDecisions.isEmpty) createDecisionSeq()
      currentDecisions = stackedDecisions.dequeue()
      executingDecisions.enqueue(currentDecisions.toSeq: _*)
    }

    executingDecisions.dequeue()
  }

  protected def createDecisionSeq() {
    implicit val num = measure.measureNumeric.asInstanceOf[Numeric[M#Measure]]
    def behavioursFunc = snapshotToPerception _ andThen possibleBehaviors
    val tacticalOptions = env.foresee(foreseeingDepth, behavioursFunc)
    val estimatedPerformance =  tacticalOptions.map{ case (actions, result) => actions -> calcPerformance(result) }
    val estimatedMeasures = estimatedPerformance.mapValues(v => v -> v.map(_.value).sum)
    val bestOptions =  estimatedMeasures.filterMax(_._2._2)
    val chosen = bestOptions.randomChoose
    stackedDecisions.enqueue(CriteriaReasonedDecisions(chosen._1, chosen._2._1.toSet, chosen._2._2))
  }

  case class CriteriaReasonedDecisions(decision: Seq[Action], criteria: Set[M#CriterionValue], measure: M#Measure){
    def toSeq = decision.map(d => CriteriaReasonedDecision(d, criteria, measure))
  }
  case class CriteriaReasonedDecision(decision: Action, criteria: Set[M#CriterionValue], measure: M#Measure)

  protected def criterionExplainedAction(a: Action, c: M#CriteriaValue, m: M#Measure): ExplainedAction = CriteriaReasonedDecision(a, c.toSet, m)
  protected def explainedActionPerformance: (ExplainedAction) => M#Measure = _.measure
  protected def actionFromExplained(ex: ExplainedAction): Action = ex.decision
}
