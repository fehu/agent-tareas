package feh.tec.agent

import scala.concurrent.{Await, Future}
import akka.actor.{ActorSystem, ActorRef, Actor}
import scala.concurrent.duration._
import akka.pattern._

trait AgentExecutionLoop[Position, EnvState, EnvGlobal, Action <: AbstractAction, Env <: Environment[Position, EnvState, EnvGlobal, Action, Env]] {
  type EnvRef = EnvironmentRef[Position, EnvState, EnvGlobal, Action, Env]

  type Execution
  def execution: Execution

  def pauseBetweenExecs: FiniteDuration
  def execControlTimeout: FiniteDuration

  def isCurrentlyExecuting: Boolean

}

trait PausableAgentExecution[Position, EnvState, EnvGlobal, Action <: AbstractAction, Env <: Environment[Position, EnvState, EnvGlobal, Action, Env]]{
  self: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env] =>

  def pause()
  def resume()
}

trait ActorAgentExecutionLoop[Position, EnvState, EnvGlobal, Action <: AbstractAction, Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                              +Ag <: AgentWithActor[Position, EnvState, EnvGlobal, Action, Env, _ <: ActorAgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env, Ag]]]
  extends AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env]
{
  def receive: PartialFunction[Any, Option[Any]]
  def agent: Ag

  protected implicit def executionContext = agent.env.sys.executionContext
  protected def scheduler = agent.env.sys.scheduler
}

trait ByTimerAgentExecution // todo: very important

trait AgentInfiniteExecution[Position, EnvState, EnvGlobal, Action <: AbstractAction, Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                             Ag <: AgentWithActor[Position, EnvState, EnvGlobal, Action, Env, _ <: ActorAgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env, Ag]]]
  extends ActorAgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env, Ag] with PausableAgentExecution[Position, EnvState, EnvGlobal, Action, Env]
{
  type Execution = () => StopFunc

  type StopFunc = () => Future[Stopped.type]

  case object Exec
  case object Stop
  case object Stopped
  case object Pause
  case object Paused

  private var pausedFlag: Boolean = true
  def isCurrentlyExecuting: Boolean = !pausedFlag

  def exec(): StopFunc =
    if(isCurrentlyExecuting) sys.error("the actor is already executing some task")
    else {
      pausedFlag = false
      agent.actorRef ! Exec
      stopInfiniteExecution
    }

  def stopInfiniteExecution: StopFunc = () => agent.actorRef
    .ask(Stop)(execControlTimeout)
    .mapTo[Stopped.type]

  def pause() = () => Await.result(
    agent.actorRef
      .ask(Pause)(execControlTimeout)
      .mapTo[Paused.type]
      .ensuring(_.isCompleted, "failed to pause the agent"),
    execControlTimeout
  )

  def resume(): Unit = ???

  def receive: PartialFunction[Any, Option[Any]] = {
    case Exec if pausedFlag => None // do nothing
    case Exec =>
      agent.lifetimeCycle(agent.env)
      if (pauseBetweenExecs.toMillis > 0) scheduler.scheduleOnce(pauseBetweenExecs, agent.actorRef, Exec)
      else agent.actorRef ! Exec
      None
    case Stop =>
      gracefulStop(agent.actorRef, execControlTimeout)
      Some(Stopped) // todo!!
  }

  def execution: Execution = () => exec()
}
