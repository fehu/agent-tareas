package feh.tec.agent

import feh.tec.util.SideEffect
import SideEffect._

trait Environment[Env <: Environment[Env]]{
  self: Env =>
  
  type Coordinate
  type State
  type Global
  type Action <: AbstractAction
  
  type Ref <: EnvironmentRef[Env]

  def states: PartialFunction[Env#Coordinate, Env#State]
  def effects: PartialFunction[Env#Action, Env => Env]
  def definedAt: Seq[Env#Coordinate]

  def globalState: Env#Global
  def stateOf(c: Env#Coordinate): Option[Env#State]
  def visibleStates: Map[Env#Coordinate, Env#State]
  def agentPosition(ag: AgentId): Option[Env#Coordinate]

  def affected(act: Env#Action): SideEffect[Env] //Environment[Coordinate, State, Global, Action]
}

// it shouldn't be possible to mix in traits that have same method defined due to self type definition

/**
  If an agent’s sensory apparatus gives it access to the complete state of the environment,
  then we say that the environment is accessible to that agent. An environment is effectively
  accessible if the sensors detect all aspects that are relevant to the choice of action. An
  accessible environment is convenient because the agent need not maintain any internal state
  to keep track of the world.
 */
trait FullyAccessible[Env <: Environment[Env] with FullyAccessible[Env]] {
  self: Env =>

  def isFullyAccessible = true
  def stateOf(c: Env#Coordinate): Option[Env#State] = states lift c
  def visibleStates: Map[Env#Coordinate, Env#State] = definedAt.map(c => c -> states(c)).toMap // todo override in case Map `states` implementation
}

trait Restricted[Env <: Environment[Env] with Restricted[Env]]{
  self: Env =>

  def isFullyAccessible = false
  def stateOf(c: Env#Coordinate): Option[Env#State] = if(canAccess(c)) states lift c else None
  def visibleStates: Map[Env#Coordinate, Env#State] = definedAt.withFilter(canAccess).map(c => c -> states(c)).toMap // todo override in case Map `states` implementation
  def canAccess(c: Env#Coordinate): Boolean
}


/**
  If the next state of the environment is completely determined by the current state and the
  actions selected by the agents, then we say the environment is deterministic. In principle,
  an agent need not worry about uncertainty in an accessible, deterministic environment. If
  the environment is inaccessible, however, then it may appear to be nondeterministic. This
  is particularly true if the environment is complex, making it hard to keep track of all the
  inaccessible aspects. Thus, it is often better to think of an environment as deterministic or
  nondeterministic from the point of view of the agent.
 */

trait Determinism[Env <: Environment[Env] with Determinism[Env]]{
  self: Env  =>

  def affected(act: Env#Action): SideEffect[Env] = ??? // stub for abstract override
}

trait Deterministic[Env <: Environment[Env] with Deterministic[Env]] extends Determinism[Env]{
  self: Env =>

  def isDeterministic = true

  override def affected(act: Env#Action) = sideEffect{ effects lift act map (_ apply self) getOrElse self }
}

trait NonDeterministic[Env <: Environment[Env] with NonDeterministic[Env]] extends Determinism[Env]{
  self: Env =>

  def isDeterministic = false

  def uncertainty: Uncertainty[Env]

  override def affected(act: Env#Action) = sideEffect{ uncertainty.influence(act, effects lift act, this) }
}

trait Uncertainty[Env <: Environment[Env]]{
  def influence(act: Env#Action, uneffected: Option[Env => Env], env: Env): Env
}

/**
  If the environment can change while an agent is deliberating, then we say the environment
  is dynamic for that agent; otherwise it is static. Static environments are easy to deal with
  because the agent need not keep looking at the world while it is deciding on an action,
  nor need it worry about the passage of time.
 */

trait Static[Env <: Environment[Env] with Static[Env]]{
  self: Env =>

  def isDynamic = false
}

sealed trait Dynamic[Env <: Environment[Env] with Dynamic[Env]]{
  self: Env =>

  def isDynamic = true

  def dynamicChange(env: Env): SideEffect[Env]
}

trait DynamicChangeAfterAction[Env <: Environment[Env] with DynamicChangeAfterAction[Env]]
  extends Dynamic[Env] with Determinism[Env]
{
  self: Env =>

  abstract override def affected(act: Env#Action) = dynamicChange _ flatCompose super.affected apply act
  //new SideEffectResultingFunction1Wrapper(dynamicChange)
}

trait DynamicChangeBeforeAction[Env <: Environment[Env] with DynamicChangeBeforeAction[Env]]
  extends Dynamic[Env] with Determinism[Env]
{
  self: Env =>

  abstract override def affected(act: Env#Action) = dynamicChange(self) flatThen (_ affected act)
}

/**
 * should be used with caution ...
 */
trait AnyTimeDynamicChange[Env <: Environment[Env] with AnyTimeDynamicChange[Env]]
  extends Dynamic[Env] with Determinism[Env]
{
  self: Env =>

  def change(func: Env => Env): Env = func(this)
}

/**
 * an environment that has a behavior prediction interface
 * i think  actual prediction should be handled by [[feh.tec.agent.PredictingEnvironmentOverseer]]
 */
trait PredictableEnvironment[Env <: Environment[Env] with PredictableEnvironment[Env]]
  extends Determinism[Env]
{
  self: Env =>

  type Ref <: EnvironmentRef[Env] with PredictableEnvironmentRef[Env]

  type Prediction
}

trait ForeseeableEnvironment[Env <: Environment[Env] with ForeseeableEnvironment[Env]]
  extends PredictableEnvironment[Env]
{
  self: Env =>

  type Ref <: EnvironmentRef[Env] with ForeseeableEnvironmentRef[Env]
}

trait PredictableDeterministicEnvironment[Env <: Environment[Env] with PredictableDeterministicEnvironment[Env] with Deterministic[Env]]
  extends PredictableEnvironment[Env]
{
  self: Env =>

  type Prediction = EnvironmentSnapshot[Env]
}

trait PredictableNonDeterministicEnvironment[Env <: Environment[Env] with PredictableNonDeterministicEnvironment[Env] with NonDeterministic[Env]]
  extends PredictableEnvironment[Env]
{
  self: Env =>

  /**
   * map probability -> Environment Snapshot in of that case
   */
  type Prediction = Map[Double, EnvironmentSnapshot[Env]]
}

trait EnvironmentImplementation[Env <: EnvironmentImplementation[Env]] extends Environment[Env]{
  self: Env =>
}

trait MutableEnvironment[Env <: Environment[Env] with MutableEnvironment[Env]]
  extends EnvironmentImplementation[Env]
{
  self: Env =>

  def initStates: PartialFunction[Env#Coordinate, Env#State]
  private var _states = initStates
  def states: PartialFunction[Env#Coordinate, Env#State] = _states
  def states_=(pf: PartialFunction[Env#Coordinate, Env#State]) = _states = pf

  def initGlobalState: Env#Global
  private var _globalState = initGlobalState
  def globalState: Env#Global = _globalState
  def globalState_=(g: Env#Global) = _globalState = g
}

trait ImmutableEnvironment[Env <: Environment[Env] with ImmutableEnvironment[Env]]
  extends EnvironmentImplementation[Env]
{
  self: Env =>

  override val states: PartialFunction[Env#Coordinate, Env#State]
  override val globalState: Env#Global
}