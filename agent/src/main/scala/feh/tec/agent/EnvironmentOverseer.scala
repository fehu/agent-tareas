package feh.tec.agent

import akka.actor.Actor
import scala.reflect.runtime.universe._
import shapeless._

/**
 *  manages the environment, hides actual environment change implementation
 *  produces environment references
 */
trait EnvironmentOverseer[Coordinate, State, Global, Action <: AbstractAction, Env <: Environment[Coordinate, State, Global, Action]] {
  protected[agent] def updateEnvironment(f: Env => Env): Env
  def currentEnvironment: Env
  def env = currentEnvironment

  def ref: EnvironmentRef[Coordinate, State, Global, Action, Env]
}

protected[agent] object EnvironmentOverseerActor{
  sealed trait Message
  sealed class MessageResponse[R, H <: HList](val response: R, val ttag: H) extends Message

  object Get{
    case object GlobalState extends Message
    case class StateOf[Coordinate](c: Coordinate)(implicit val ttag: TypeTag[Coordinate]) extends Message
    case object VisibleStates extends Message
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
  }
}

trait EnvironmentOverseerActor[Coordinate, State, Global, Action <: AbstractAction, Env <: Environment[Coordinate, State, Global, Action]]
  extends Actor with EnvironmentOverseer[Coordinate, State, Global, Action, Env]
{
  import EnvironmentOverseerActor._
  val tags = env.tags
  import tags._

  def receive: Actor.Receive = {
    case Get.GlobalState => sender ! Response.GlobalState(env.globalState)
    case g@Get.StateOf(c) if g.ttag.tpe =:= typeOf[Coordinate] => sender ! Response.StateOf(c, env.stateOf(c.asInstanceOf[Coordinate]))
    case Get.VisibleStates => sender ! Response.VisibleStates(env.visibleStates)
    case a@Act(act) if a.ttag.tpe <:< typeOf[Action] => updateEnvironment(_.affected(act.asInstanceOf[Action]))
  }
}

