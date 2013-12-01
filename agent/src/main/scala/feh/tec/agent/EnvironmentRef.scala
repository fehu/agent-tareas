package feh.tec.agent

import scala.concurrent._
import concurrent.duration._
import feh.tec.util.{MapZipperWrapper, SideEffect, ScopedState}
import akka.actor.Scheduler

/**
 * provides access to current environment instance, which is hidden from agent
 */
trait EnvironmentRef[Coordinate, State, Global, Action <: AbstractAction, Env <: Environment[Coordinate, State, Global, Action, Env]]{

//  type EnvRef = EnvironmentRef[Coordinate, State, Global, Action, Env]

  def blocking: BlockingApi
  def async: AsyncApi
  def sys: SystemApi

  trait BlockingApi{
    def globalState: Global
    def stateOf(c: Coordinate): Option[State]
    def affect(act: Action): SideEffect[Env#Ref]
    def visibleStates: Map[Coordinate, State]
    def agentPosition(id: AgentId): Option[Coordinate]

    /**
     * makes a snapshot (a static image) of current environment state
     */
    def snapshot: Env with EnvironmentSnapshot[Coordinate, State, Global, Action, Env]
  }

  trait SystemApi{
    implicit def executionContext: ExecutionContext
    def scheduler: Scheduler
  }

  trait AsyncApi {
    def withTimeout[R](t: Int)(r: => R): R

    def globalState: Future[Global]
    def stateOf(c: Coordinate): Future[Option[State]]
    def affect(act: Action): Future[SideEffect[Env#Ref]]
    def visibleStates: Future[Map[Coordinate, State]]
    def agentPosition(id: AgentId): Future[Option[Coordinate]]

    /**
     * makes a snapshot (a static image) of current environment state
     */
    def snapshot: Future[Env with EnvironmentSnapshot[Coordinate, State, Global, Action, Env]]
  }

}

trait PredictableEnvironmentRef[Coordinate, State, Global, Action <: AbstractAction,
                                Env <: Environment[Coordinate, State, Global, Action, Env] with PredictableEnvironment[Coordinate, State, Global, Action, Env]]
  extends EnvironmentRef[Coordinate, State, Global, Action, Env]
{
  def predict(a: Action): Env#Prediction
  def asyncPredict(a: Action): Future[Env#Prediction]
}

trait ForeseeableEnvironmentRef[Coordinate, State, Global, Action <: AbstractAction,
                                Env <: Environment[Coordinate, State, Global, Action, Env] with ForeseeableEnvironment[Coordinate, State, Global, Action, Env]]
  extends PredictableEnvironmentRef[Coordinate, State, Global, Action, Env]
{
  /**
   *
   * @param possibleActions previous actions => current snapshot => possible actions
   * @param includeShorter include action seqs of length less then foreseeing depth
   * @param excludeTurningBack exclude action seqs that pass twice same state
   */
  def foresee(depth: Int,
              possibleActions: Seq[Action] => EnvironmentSnapshot[Coordinate, State, Global, Action, Env] => Set[Action],
              includeShorter: Boolean,
              excludeTurningBack: Boolean): Map[Seq[Action], Env#Prediction]
  def asyncForesee(depth: Int,
                   possibleActions: Seq[Action] => EnvironmentSnapshot[Coordinate, State, Global, Action, Env] => Set[Action],
                   includeShorter: Boolean,
                   excludeTurningBack: Boolean): Future[Map[Seq[Action], Env#Prediction]]
}

/*
trait EnvironmentRefBlockingApiImpl[Coordinate, State, Global, Action <: AbstractAction, Env <: Environment[Coordinate, State, Global, Action, Env]]
  extends EnvironmentRef[Coordinate, State, Global, Action , Env]
{
  implicit def executionContext = this.sys.executionContext

  def defaultBlockingTimeout: Int
  protected val blockingTimeoutScope = new ScopedState(defaultBlockingTimeout)

  lazy val blocking = new BlockingApi{
    def withTimeout[R](t: Int)(r: => R): R = blockingTimeoutScope.doWith(t)(r)

    private def awaitResult[R](select: AsyncApi => Awaitable[R]): R = Await.result(select(async), blockingTimeoutScope.get millis)
    def globalState: Global = awaitResult(_.globalState)
    def stateOf(c: Coordinate): Option[State] = awaitResult(_.stateOf(c))
    def affect(act: Action): SideEffect[Env#Ref] = awaitResult(_.affect(act))
    def visibleStates: Map[Coordinate, State] = awaitResult(_.visibleStates)
    def snapshot: Env with EnvironmentSnapshot[Coordinate, State, Global, Action, Env] = awaitResult(_.snapshot)
  }
}
*/

object EnvironmentSnapshot{
  /**
   * compares both states, ignores agent's position
   */
  def withStateComparator[State, R](c: (State, State) => Boolean)(f: => R) = StateComparator.doWith(c.asInstanceOf[(Any, Any) => Boolean])(f)
  protected object StateComparator extends ScopedState[(Any, Any) => Boolean](_ == _)
  def stateComparator[State] = StateComparator.get.asInstanceOf[(State, State) => Boolean]
}

/**
 * should be mixed-in last
 */
trait EnvironmentSnapshot[Coordinate, State, Global, Action <: AbstractAction, Env <: Environment[Coordinate, State, Global, Action, Env]]
  extends Environment[Coordinate, State, Global, Action, Env]
{
  self: Env =>

  def states: Map[Coordinate, State]
  val effects: PartialFunction[Action, (Env) => Env]
  val definedAt: Seq[Coordinate]
  val globalState: Global

  /**
   * @return self, no effect should be produced
   */
//  abstract override def affected(act: Action): EnvironmentSnapshot[Coordinate, State, Global, Action, Env] = this
  def affected(act: Action): SideEffect[Env] = SideEffect(this)

  def asEnv: Env with EnvironmentSnapshot[Coordinate, State, Global, Action, Env] = self

  override def equals(obj: scala.Any): Boolean = PartialFunction.cond(obj){
    case snap: EnvironmentSnapshot[Coordinate, State, Global, Action, Env] =>
      snap.globalState == this.globalState &&
      snap.states.zipByKey(this.states).forall{
        case (_, (s1, s2)) => EnvironmentSnapshot.stateComparator[State](s1, s2)
      }
  }

  def diff(that: EnvironmentSnapshot[Coordinate, State, Global, Action, Env]) =
    this.states.zipByKey(that.states).filter{ case (_, (v1, v2)) => v1 != v2 }

  override def toString: String = s"EnvironmentSnapshot($globalState, $states)"
}

/**
 *  Use with copies of current mutable environment for making predictions
 */

trait CustomisableEnvironmentSnapshot[Coordinate, State, Global, Action <: AbstractAction,
                                      Env <: Environment[Coordinate, State, Global, Action, Env]
                                        with MutableEnvironment[Coordinate, State, Global, Action, Env]]
  extends MutableEnvironment[Coordinate, State, Global, Action, Env]
{
  self: Env =>

  def copy(): CustomisableEnvironmentSnapshot[Coordinate, State, Global, Action, Env] with Env

  def snapshot(): EnvironmentSnapshot[Coordinate, State, Global, Action, Env]
}
