package feh.tec.agent

import scala.concurrent._
import feh.tec.util.{MapZipperWrapper, SideEffect}
import akka.actor.Scheduler
import feh.util.ScopedState

/**
 * provides access to current environment instance, which is hidden from agent
 */
trait EnvironmentRef[Env <: Environment[Env]]{

  def blocking: BlockingApi
  def async: AsyncApi
  def sys: SystemApi

  trait BlockingApi{
    def globalState: Env#Global
    def stateOf(c: Env#Coordinate): Option[Env#State]
    def affect(act: Env#Action): SideEffect[Env#Ref]
    def visibleStates: Map[Env#Coordinate, Env#State]
    def agentPosition(id: AgentId): Option[Env#Coordinate]

    /**
     * makes a snapshot (a static image) of current environment state
     */
    def snapshot: Env with EnvironmentSnapshot[Env]
  }

  trait SystemApi{
    implicit def executionContext: ExecutionContext
    def scheduler: Scheduler
  }

  trait AsyncApi {
    def withTimeout[R](t: Int)(r: => R): R

    def globalState: Future[Env#Global]
    def stateOf(c: Env#Coordinate): Future[Option[Env#State]]
    def affect(act: Env#Action): Future[SideEffect[Env#Ref]]
    def visibleStates: Future[Map[Env#Coordinate, Env#State]]
    def agentPosition(id: AgentId): Future[Option[Env#Coordinate]]

    /**
     * makes a snapshot (a static image) of current environment state
     */
    def snapshot: Future[Env with EnvironmentSnapshot[Env]]
  }

}

trait PredictableEnvironmentRef[Env <: Environment[Env] with PredictableEnvironment[Env]] extends EnvironmentRef[Env]{
  def predict(a: Env#Action): Env#Prediction
  def asyncPredict(a: Env#Action): Future[Env#Prediction]
}

trait ForeseeableEnvironmentRef[Env <: Environment[Env] with ForeseeableEnvironment[Env]] extends PredictableEnvironmentRef[Env]{
  /**
   *
   * @param possibleActions previous actions => current snapshot => possible actions
   * @param includeShorter include action seqs of length less then foreseeing depth
   * @param excludeTurningBack exclude action seqs that pass twice same state
   */
  def foresee(depth: Int,
              possibleActions: Seq[Env#Action] => EnvironmentSnapshot[Env] => Set[Env#Action],
              includeShorter: Boolean,
              excludeTurningBack: Boolean): Map[Seq[Env#Action], Env#Prediction]
  def asyncForesee(depth: Int,
                   possibleActions: Seq[Env#Action] => EnvironmentSnapshot[Env] => Set[Env#Action],
                   includeShorter: Boolean,
                   excludeTurningBack: Boolean): Future[Map[Seq[Env#Action], Env#Prediction]]
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
trait EnvironmentSnapshot[Env <: Environment[Env]] extends Environment[Env]{
  self: Env =>

  def states: Map[Env#Coordinate, Env#State]
  val effects: PartialFunction[Env#Action, Env => Env]
  val definedAt: Seq[Env#Coordinate]
  val globalState: Env#Global

  /**
   * @return self, no effect should be produced
   */
  def affected(act: Env#Action): SideEffect[Env] = SideEffect(this)

  def asEnv: Env with EnvironmentSnapshot[Env] = self

  override def equals(obj: Any): Boolean = PartialFunction.cond(obj){
    case snap: EnvironmentSnapshot[Env] with Env =>
      snap.globalState == this.globalState &&
      snap.states.zipByKey(this.states).forall{
        case (_, (s1, s2)) => EnvironmentSnapshot.stateComparator[Env#State](s1, s2)
      }
  }

  def diff(that: EnvironmentSnapshot[Env]) =
    this.states.zipByKey(that.states).filter{ case (_, (v1, v2)) => v1 != v2 }

  override def toString: String = s"EnvironmentSnapshot($globalState, $states)"
}

/**
 *  Use with copies of current mutable environment for making predictions
 */

trait CustomisableEnvironmentSnapshot[Env <: Environment[Env] with MutableEnvironment[Env]] extends MutableEnvironment[Env]{
  self: Env =>

  def copy(): CustomisableEnvironmentSnapshot[Env] with Env

  def snapshot(): EnvironmentSnapshot[Env]
}
