package feh.tec.agent

import scala.concurrent.Future
import akka.actor.{ActorRef, Actor}
import scala.concurrent.duration._
import akka.pattern._

trait AgentExecutionLoop[Position, EnvState, EnvGlobal, Action <: AbstractAction, Env <: Environment[Position, EnvState, EnvGlobal, Action, Env]] {
  type EnvRef = EnvironmentRef[Position, EnvState, EnvGlobal, Action, Env]

  type Execution

  def pauseBetweenExecs: FiniteDuration
  def stopTimeout: FiniteDuration

  def env: EnvRef
  def execSequence: EnvRef => Action
  def execution: Execution

  def isCurrentlyExecuting: Boolean

}

trait ActorAgentExecutionLoop[Position, EnvState, EnvGlobal, Action <: AbstractAction, Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                              Ag <: ActorAgent[Position, EnvState, EnvGlobal, Action, Env, _ <: ActorAgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env, Ag]]]
  extends AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env]
{
  protected def actor: Actor

  def receive: Actor.Receive

  protected implicit def executionContext = actor.context.dispatcher
  protected def scheduler = env.sys.scheduler
}

trait AgentInfiniteExecution[Position, EnvState, EnvGlobal, Action <: AbstractAction, Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                             Ag <: ActorAgent[Position, EnvState, EnvGlobal, Action, Env, _ <: ActorAgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env, Ag]]]
  extends ActorAgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env, Ag]
{
  type Execution = this.type

  type StopFunc = () => Future[Stopped.type]

  case object Exec
  case object Stop
  case object Stopped

  private var stoppedFlag: Boolean = true
  def isCurrentlyExecuting: Boolean = !stoppedFlag

  def exec(): StopFunc =
    if(isCurrentlyExecuting) sys.error("the actor is already executing some task")
    else {
      actor.self ! Exec
      stopInfiniteExecution
    }

  def stopInfiniteExecution: StopFunc = () => actor.self.ask(Stop)(stopTimeout).mapTo[Stopped.type]

  def receive: Actor.Receive = {
    case Exec if stoppedFlag => // to nothing
    case Exec =>
      env.async.affect(execSequence(env))
        .map(_.execute).map{
          e =>
            if (pauseBetweenExecs.toMillis > 0) scheduler.scheduleOnce(pauseBetweenExecs, actor.self, Exec)
            else actor.self ! Exec
        }
    case Stop =>
      stoppedFlag = true
      actor.sender ! Stopped
  }
}
