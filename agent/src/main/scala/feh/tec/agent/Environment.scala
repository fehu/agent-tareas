package feh.tec.agent

import scala.reflect.runtime.universe._

trait Environment[Coordinate, State, Global, Action <: AbstractAction]{ self =>
  protected def states: PartialFunction[Coordinate, State]
  protected def effects: PartialFunction[
    Action,
    Environment[Coordinate, State, Global, Action] => Environment[Coordinate, State, Global, Action]
  ]
  protected def definedAt: Seq[Coordinate]

  def globalState: Global
  def stateOf(c: Coordinate): Option[State]
  def visibleStates: Map[Coordinate, State]

  def affected(act: Action): self.type //Environment[Coordinate, State, Global, Action]

  protected trait TypeTags{
    implicit def coordinate: TypeTag[Coordinate]
    implicit def state: TypeTag[State]
    implicit def global: TypeTag[Global]
    implicit def action: TypeTag[Action]
    implicit def environment[Env <: Environment[Coordinate, State, Global, Action]]: TypeTag[Env]
  }

  def tags: TypeTags
}

// it shouldn't be possible to mix in traits that have same method defined due to self type definition

/**
  If an agent’s sensory apparatus gives it access to the complete state of the environment,
  then we say that the environment is accessible to that agent. An environment is effectively
  accessible if the sensors detect all aspects that are relevant to the choice of action. An
  accessible environment is convenient because the agent need not maintain any internal state
  to keep track of the world.
 */
trait FullyAccessible[Coordinate, State, Global, Action <: AbstractAction] { self: Environment[Coordinate, State, Global, Action] =>
  def isFullyAccessible = true
  def stateOf(c: Coordinate): Option[State] = states lift c
  def visibleStates: Map[Coordinate, State] = definedAt.map(c => c -> states(c)).toMap // todo override in case Map `states` implementation
}

trait Restricted[Coordinate, State, Global, Action <: AbstractAction]{ self: Environment[Coordinate, State, Global, Action] =>
  def isFullyAccessible = false
  def stateOf(c: Coordinate): Option[State] = if(canAccess(c)) states lift c else None
  def visibleStates: Map[Coordinate, State] = definedAt.withFilter(canAccess).map(c => c -> states(c)).toMap // todo override in case Map `states` implementation
  def canAccess(c: Coordinate): Boolean
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

trait Determinism[Coordinate, State, Global, Action <: AbstractAction]{ self: Environment[Coordinate, State, Global, Action] =>
  def affected(act: Action): Environment[Coordinate, State, Global, Action] = ??? // stub for abstract override
}

trait Deterministic[Coordinate, State, Global, Action <: AbstractAction] extends Determinism[Coordinate, State, Global, Action]{
  self: Environment[Coordinate, State, Global, Action] =>

  def isDeterministic = true

  override def affected(act: Action) = effects lift act map (_ apply this) getOrElse this
}

trait NonDeterministic[Coordinate, State, Global, Action <: AbstractAction] extends Determinism[Coordinate, State, Global, Action]{
  self: Environment[Coordinate, State, Global, Action] =>

  def isDeterministic = false

  def uncertainty: Uncertainty[Coordinate, State, Global, Action]

  override def affected(act: Action) = uncertainty.influence(act, effects lift act, this)
}

trait Uncertainty[Coordinate, State, Global, Action <: AbstractAction]{
  def influence[Env <: Environment[Coordinate, State, Global, Action]](act: Action, uneffected: Option[Env => Env], env: Env): Env
}

/**
  If the environment can change while an agent is deliberating, then we say the environment
  is dynamic for that agent; otherwise it is static. Static environments are easy to deal with
  because the agent need not keep looking at the world while it is deciding on an action,
  nor need it worry about the passage of time.
 */

trait Static[Coordinate, State, Global, Action <: AbstractAction]{ self: Environment[Coordinate, State, Global, Action] =>
  def isDynamic = false
}

sealed trait Dynamic[Coordinate, State, Global, Action <: AbstractAction]{ self: Environment[Coordinate, State, Global, Action] =>
  def isDynamic = true

  def dynamicChange(env: Environment[Coordinate, State, Global, Action]): Environment[Coordinate, State, Global, Action]
}

trait DynamicChangeAfterAction[Coordinate, State, Global, Action <: AbstractAction]
  extends Dynamic[Coordinate, State, Global, Action] with Determinism[Coordinate, State, Global, Action]
{
  self: Environment[Coordinate, State, Global, Action] =>

  abstract override def affected(act: Action) = dynamicChange _ compose super.affected apply act
}

trait DynamicChangeBeforeAction[Coordinate, State, Global, Action <: AbstractAction]
  extends Dynamic[Coordinate, State, Global, Action] with Determinism[Coordinate, State, Global, Action]
{
  self: Environment[Coordinate, State, Global, Action] =>

  abstract override def affected(act: Action) = dynamicChange(this) affected act
}

/**
 * should be used with caution ...
 */
trait AnyTimeDynamicChange[Coordinate, State, Global, Action <: AbstractAction]
  extends Dynamic[Coordinate, State, Global, Action] with Determinism[Coordinate, State, Global, Action]
{
  self: Environment[Coordinate, State, Global, Action] =>

  def change(func: Environment[Coordinate, State, Global, Action] => Environment[Coordinate, State, Global, Action]): Environment[Coordinate, State, Global, Action] = func(this)
}