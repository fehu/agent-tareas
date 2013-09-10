package feh.tec.agent

import akka.actor.{Scheduler, ActorSystem, Actor}
import scala.reflect.runtime.universe._
import scala.concurrent.{ExecutionContext, Future}
import java.util.Calendar
import akka.pattern._
import feh.tec.util.{SideEffect, ScopedState}
import akka.util.Timeout
import scala.reflect._

/**
 *  manages the environment, hides actual environment change implementation
 *  produces environment references
 */
trait EnvironmentOverseer[Coordinate, State, Global, Action <: AbstractAction, Env <: Environment[Coordinate, State, Global, Action, Env]] {
  def currentEnvironment: Env
  def env = currentEnvironment

  protected[agent] def updateEnvironment(f: Env => Env): SideEffect[Env]

  def snapshot: EnvironmentSnapshot[Coordinate, State, Global, Action, Env]
  def ref: EnvironmentRef[Coordinate, State, Global, Action, Env]
}

protected[agent] object EnvironmentOverseerActor{
  sealed trait Message
  sealed class MessageResponse[R](val response: R, val ttag: List[TypeTag[_]]) extends Message

  object Get{
    case object GlobalState extends Message
    case class StateOf[Coordinate](c: Coordinate)(implicit val ttag: TypeTag[Coordinate]) extends Message
    case object VisibleStates extends Message
    case object Snapshot extends Message
  }

  case class Act[Action <: AbstractAction](act: Action)(implicit val ttag: TypeTag[Action]) extends Message

  object Response{
    case class GlobalState[Global](global: Global)(implicit ttag: TypeTag[Global]) extends MessageResponse(global, ttag :: Nil)
    case class StateOf[Coordinate, State](c: Coordinate, stateOpt: Option[State])
                                         (implicit ttag1: TypeTag[Coordinate], ttag2: TypeTag[State])
      extends MessageResponse(c -> stateOpt, ttag1 :: ttag2 :: Nil)
    case class VisibleStates[Coordinate, State](states: Map[Coordinate, State])
                                               (implicit ttag1: TypeTag[Coordinate], ttag2: TypeTag[State])
      extends MessageResponse(states, typeTag[Map[_, _]] :: ttag1 :: ttag2 :: Nil)
    case class ActionApplied[Action <: AbstractAction](a: Action)(implicit ttag: TypeTag[Action]) extends MessageResponse(a, ttag :: Nil)
    case class Snapshot[Coordinate, State, Global, Action <: AbstractAction, Env <: Environment[Coordinate, State, Global, Action, Env]]
      (snapshot: EnvironmentSnapshot[Coordinate, State, Global, Action, Env], time: Long)
      (implicit ttag: TypeTag[Env]) extends MessageResponse(snapshot -> time, ttag :: Nil)
  }
}

trait EnvironmentOverseerActor[Coordinate, State, Global, Action <: AbstractAction, Env <: Environment[Coordinate, State, Global, Action, Env]]
  extends Actor with EnvironmentOverseer[Coordinate, State, Global, Action, Env]
{
  overseer: Actor =>

  import EnvironmentOverseerActor._
  val tags = env.tags
  import tags._

  def receive: Actor.Receive = {
    case Get.GlobalState => sender ! Response.GlobalState(env.globalState)
    case g@Get.StateOf(c) if g.ttag.tpe =:= typeOf[Coordinate] => sender ! Response.StateOf(c, env.stateOf(c.asInstanceOf[Coordinate]))
    case Get.VisibleStates => sender ! Response.VisibleStates(env.visibleStates)
    case Get.Snapshot => sender ! Response.Snapshot(snapshot, Calendar.getInstance().getTimeInMillis)
    case a@Act(act) if a.ttag.tpe <:< typeOf[Action] => 
      updateEnvironment(_.affected(act.asInstanceOf[Action]).execute)
      sender ! Response.ActionApplied(act)
  }

  protected def refExecutionContext: ExecutionContext
  protected def scheduler: Scheduler

  def defaultBlockingTimeout: Int
  def defaultFutureTimeout: Int

  /*
   * todo: used typed actors
   */
  def ref: EnvironmentRef[Coordinate, State, Global, Action, Env] =
    new EnvironmentRef[Coordinate, State, Global, Action, Env] with EnvironmentRefBlockingApiImpl[Coordinate, State, Global, Action, Env]
    {
      envRef =>

      def defaultBlockingTimeout: Int = overseer.defaultBlockingTimeout
      protected val futureTimeoutScope = new ScopedState(defaultFutureTimeout)

      lazy val sys: SystemApi = new SystemApi {
        implicit def executionContext: ExecutionContext = refExecutionContext
        def scheduler: Scheduler = overseer.scheduler
      }

      lazy val async: AsyncApi = new AsyncApi{
        def withTimeout[R](t: Int)(r: => R): R = futureTimeoutScope.doWith(t)(r)

        implicit def timeout = Timeout(futureTimeoutScope.get)

        def globalState: Future[Global] = (overseer.self ? Get.GlobalState)
          .mapTo[Response.GlobalState[_]].withFilter(_.ttag.head.tpe <:< typeOf[Global]).map(_.global.asInstanceOf[Global])

        def stateOf(c: Coordinate) = (overseer.self ? Get.StateOf(c))
          .mapTo[Response.StateOf[_, _]]
          .withFilter(_.c == c)
          .withFilter(_.ttag match { case _ :: tt2 :: Nil => tt2.tpe <:< typeOf[State] })
          .map(_.stateOpt.asInstanceOf[Option[State]])

        def affect(act: Action) = (overseer.self ? Act(act))
          .mapTo[Response.ActionApplied[_]]
          .withFilter(_.a == Act)
          .map(_ => SideEffect(envRef))

        def visibleStates = (overseer.self ? Get.VisibleStates)
          .mapTo[Response.VisibleStates[_, _]]
          .withFilter(_.ttag match { case tt0 :: tt1 :: tt2 :: Nil =>
            tt1.tpe =:= typeOf[Coordinate] &&
            tt2.tpe <:< typeOf[State]
          })
          .map(_.states.asInstanceOf[Map[Coordinate, State]])

        def snapshot = (overseer.self ? Get.Snapshot)
          .mapTo[Response.Snapshot[_, _, _, _, _]]
          .withFilter(_.ttag match { case _ :: _ :: _ :: _ :: envTag :: Nil => envTag.tpe =:= typeOf[Env] })
          .map(_.snapshot.asInstanceOf[Env with EnvironmentSnapshot[Coordinate, State, Global, Action, Env]])
      }
  }
}

trait EnvironmentOverseerImplementation[Coordinate, State, Global, Action <: AbstractAction, Env <: Environment[Coordinate, State, Global, Action, Env]]{
  self: EnvironmentOverseer[Coordinate, State, Global, Action, Env] =>
}

trait MutableEnvironmentOverseer[Coordinate, State, Global, Action <: AbstractAction,
    Env <: Environment[Coordinate, State, Global, Action, Env] with MutableEnvironment[Coordinate, State, Global, Action, Env]]
  extends EnvironmentOverseerImplementation[Coordinate, State, Global, Action, Env]
{
  self: EnvironmentOverseer[Coordinate, State, Global, Action, Env] =>

  override val currentEnvironment: Env

  protected[agent] def updateEnvironment(f: (Env) => Env): SideEffect[Env] = SideEffect(f(env))
}

trait ImmutableEnvironmentOverseer[Coordinate, State, Global, Action <: AbstractAction,
    Env <: Environment[Coordinate, State, Global, Action, Env] with ImmutableEnvironment[Coordinate, State, Global, Action, Env]]
  extends EnvironmentOverseerImplementation[Coordinate, State, Global, Action, Env]
{
  self: EnvironmentOverseer[Coordinate, State, Global, Action, Env] =>

  def initialEnvironment: Env
  protected var environment = initialEnvironment
  def currentEnvironment: Env = environment
  protected[agent] def updateEnvironment(f: (Env) => Env): SideEffect[Env] = SideEffect{
    val e = f(env)
    environment = e
    e
  }
}