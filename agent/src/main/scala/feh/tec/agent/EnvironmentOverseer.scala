package feh.tec.agent

import akka.actor.{ActorSystem, Actor}
import scala.reflect.runtime.universe._
import shapeless._
import scala.concurrent.Future
import java.util.Calendar
import akka.pattern._
import feh.tec.util.ScopedState
import akka.util.Timeout
import scala.reflect._

/**
 *  manages the environment, hides actual environment change implementation
 *  produces environment references
 */
trait EnvironmentOverseer[Coordinate, State, Global, Action <: AbstractAction, Env <: Environment[Coordinate, State, Global, Action]] {
  protected[agent] def updateEnvironment(f: Env => Env): Env
  def currentEnvironment: Env
  def env = currentEnvironment

  def snapshot: EnvironmentSnapshot[Coordinate, State, Global, Action, Env]
  def ref: EnvironmentRef[Coordinate, State, Global, Action, Env]
}

protected[agent] object EnvironmentOverseerActor{
  sealed trait Message
  sealed class MessageResponse[R, H <: HList](val response: R, val ttag: H) extends Message

  object Get{
    case object GlobalState extends Message
    case class StateOf[Coordinate](c: Coordinate)(implicit val ttag: TypeTag[Coordinate]) extends Message
    case object VisibleStates extends Message
    case object Snapshot extends Message
  }

  case class Act[Action <: AbstractAction](act: Action)(implicit val ttag: TypeTag[Action]) extends Message

  object Response{
    case class GlobalState[Global](global: Global)(implicit ttag: TypeTag[Global]) extends MessageResponse(global, ttag :: HNil)
    case class StateOf[Coordinate, State](c: Coordinate, stateOpt: Option[State])
                                         (implicit ttag1: TypeTag[Coordinate], ttag2: TypeTag[State])
      extends MessageResponse(c -> stateOpt, ttag1 :: ttag2 :: HNil)
    case class VisibleStates[Coordinate, State](states: Map[Coordinate, State])
                                               (implicit ttag1: TypeTag[Coordinate], ttag2: TypeTag[State])
      extends MessageResponse(states, typeTag[Map[_, _]] :: ttag1 :: ttag2 :: HNil)
    case class Act(errorOpt: Option[Throwable]) extends MessageResponse(errorOpt, typeTag[Option[Throwable]] :: HNil)
    case class Snapshot[Coordinate, State, Global, Action <: AbstractAction, Env <: Environment[Coordinate, State, Global, Action]]
      (snapshot: EnvironmentSnapshot[Coordinate, State, Global, Action, Env], time: Long)
      (implicit ttag: TypeTag[Env]) extends MessageResponse(snapshot -> time, ttag :: HNil)
  }
}

trait EnvironmentOverseerActor[Coordinate, State, Global, Action <: AbstractAction, Env <: Environment[Coordinate, State, Global, Action]]
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
    case a@Act(act) if a.ttag.tpe <:< typeOf[Action] => updateEnvironment(_.affected(act.asInstanceOf[Action]))
  }

  protected def actorSystem: ActorSystem

  def defaultBlockingTimeout: Int
  def defaultFutureTimeout: Int

  def ref: EnvironmentRef[Coordinate, State, Global, Action, Env] =
    new EnvironmentRef[Coordinate, State, Global, Action, Env] with EnvironmentRefBlockingApiImpl[Coordinate, State, Global, Action, Env]
    {
      def defaultBlockingTimeout: Int = overseer.defaultBlockingTimeout
      protected val futureTimeoutScope = new ScopedState(defaultFutureTimeout)

      implicit def actorSystem: ActorSystem = overseer.actorSystem

      def async = new AsyncApi{
        def withTimeout[R](t: Int)(r: => R): R = futureTimeoutScope.doWith(t)(r)

        implicit def timeout = Timeout(futureTimeoutScope.get)

        implicit def typeTagToClassTag[T](ttag: TypeTag[T]): ClassTag[T] = ClassTag(rootMirror runtimeClass ttag.tpe)

        def globalState: Future[Global] = (overseer.self ? Get.GlobalState).mapTo[Global](global)
        def stateOf(c: Coordinate) = (overseer.self ? Get.StateOf(c)).mapTo[Option[State]]//(classTag[Option[State]])
        def affected(act: Action) = (overseer.self ? Act(act)).mapTo[Environment[Coordinate, State, Global, Action]]
        def visibleStates = (overseer.self ? Get.VisibleStates).mapTo[Map[Coordinate, State]]
        def snapshot = (overseer.self ? Get.Snapshot).mapTo[Env with EnvironmentSnapshot[Coordinate, State, Global, Action, Env]]
      }
  }
}

