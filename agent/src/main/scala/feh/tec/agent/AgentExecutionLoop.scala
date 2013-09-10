package feh.tec.agent

import scala.concurrent.Future
import akka.actor.{ActorRef, Actor}
import scala.concurrent.duration._
import akka.pattern._

trait AgentExecutionLoop[Position, EnvState, EnvGlobal, Action <: AbstractAction, Env <: Environment[Position, EnvState, EnvGlobal, Action, Env]] {
  type EnvRef = EnvironmentRef[Position, EnvState, EnvGlobal, Action, Env]

  type Execution
  def execution: Execution

  def pauseBetweenExecs: FiniteDuration
  def stopTimeout: FiniteDuration

  def isCurrentlyExecuting: Boolean

}

trait ActorAgentExecutionLoop[Position, EnvState, EnvGlobal, Action <: AbstractAction, Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                              +Ag <: ActorAgent[Position, EnvState, EnvGlobal, Action, Env, _ <: ActorAgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env, Ag]]]
  extends AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env]
{
  def receive: Actor.Receive
  def agent: Ag

  protected implicit def executionContext = agent.context.dispatcher
  protected def scheduler = agent.env.sys.scheduler
}

trait ByTimerAgentExecution // todo: very important

trait AgentInfiniteExecution[Position, EnvState, EnvGlobal, Action <: AbstractAction, Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                             Ag <: ActorAgent[Position, EnvState, EnvGlobal, Action, Env, _ <: ActorAgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env, Ag]]]
  extends ActorAgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env, Ag]
{
  type Execution = () => StopFunc

  type StopFunc = () => Future[Stopped.type]

  case object Exec
  case object Stop
  case object Stopped

  private var stoppedFlag: Boolean = true
  def isCurrentlyExecuting: Boolean = !stoppedFlag

  def exec(): StopFunc =
    if(isCurrentlyExecuting) sys.error("the actor is already executing some task")
    else {
      agent.self ! Exec
      stopInfiniteExecution
    }

  def stopInfiniteExecution: StopFunc = () => agent.self.ask(Stop)(stopTimeout).mapTo[Stopped.type]

  def receive: Actor.Receive = {
    case Exec if stoppedFlag => // to nothing
    case Exec =>
      agent.lifetimeCycle(agent.env)
      if (pauseBetweenExecs.toMillis > 0) scheduler.scheduleOnce(pauseBetweenExecs, agent.self, Exec)
      else agent.self ! Exec
    case Stop =>
      stoppedFlag = true
      agent.sender ! Stopped
  }

  def execution: Execution = () => exec()
}
