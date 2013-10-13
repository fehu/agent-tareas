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

trait AgentExecutionStopPauseImplementation[Position, EnvState, EnvGlobal, Action <: AbstractAction, Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                                            Ag <: AgentWithActor[Position, EnvState, EnvGlobal, Action, Env, _ <: ActorAgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env, Ag]]]{
  self: ActorAgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env, Ag] with PausableAgentExecution[Position, EnvState, EnvGlobal, Action, Env] =>

  type Execution = () => StopFunc
  type StopFunc = () => Future[Stopped.type]

  case object Exec
  case object Stop
  case object Stopped
  case object Pause
  case object Paused
  case object AlreadyPaused
  case object Resume
  case object Resumed
  case object AlreadyResumed

  protected var pausedFlag: Boolean = true

  def pause(): Unit = Await.result(agent.actorRef.ask(Pause)(execControlTimeout), execControlTimeout)
  def resume(): Unit = Await.result(agent.actorRef.ask(Resume)(execControlTimeout), execControlTimeout)

  protected var stoppedFlag = true
  def isCurrentlyExecuting: Boolean = !stoppedFlag

  def receive: PartialFunction[Any, Option[Any]] = {
    case Exec if pausedFlag || stoppedFlag => None // do nothing
    case Exec => exec(); None
    case Stop =>
      stoppedFlag = true
      gracefulStop(agent.actorRef, execControlTimeout)
      Some(Stopped) // todo!!
    case Pause if ! pausedFlag =>
      pausedFlag = true
      Some(Paused)
    case Resume if pausedFlag =>
      pausedFlag = false
      agent.actorRef ! Exec
      Some(Resumed)
    case Pause => Some(AlreadyPaused)
    case Resume => Some(AlreadyResumed)
  } 

  protected def exec()
  
  protected def stopExec: StopFunc = () => agent.actorRef
    .ask(Stop)(execControlTimeout)
    .mapTo[Stopped.type]
  
  def startExec(): StopFunc =
    if(isCurrentlyExecuting) sys.error("the actor is already executing some task")
    else {
      stoppedFlag = false
      pausedFlag = false
      agent.actorRef ! Exec
      stopExec
    }

  def execution: Execution = () => startExec()
}

trait AgentInfiniteExecution[Position, EnvState, EnvGlobal, Action <: AbstractAction, Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                             Ag <: AgentWithActor[Position, EnvState, EnvGlobal, Action, Env, _ <: ActorAgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env, Ag]]]
  extends ActorAgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env, Ag] with PausableAgentExecution[Position, EnvState, EnvGlobal, Action, Env]
    with AgentExecutionStopPauseImplementation[Position, EnvState, EnvGlobal, Action, Env, Ag]
{
  protected def exec() {
    agent.lifetimeCycle(agent.env)
    if (pauseBetweenExecs.toMillis > 0) scheduler.scheduleOnce(pauseBetweenExecs, agent.actorRef, Exec)
    else agent.actorRef ! Exec
  }
}

trait AgentConditionalExecution[Position, EnvState, EnvGlobal, Action <: AbstractAction, Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                                Ag <: AgentWithActor[Position, EnvState, EnvGlobal, Action, Env, _ <: ActorAgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env, Ag]],
                                StopConditionParams <: Product]
  extends AgentInfiniteExecution[Position, EnvState, EnvGlobal, Action, Env, Ag]
{
  type StopCondition = StopConditionParams => Boolean
  
  def stopConditions: Set[StopCondition]

  def testCondition: StopCondition => Boolean

  def testConditions = (false /: stopConditions)((acc, c) => acc || testCondition(c))

  def notifyFinished()

  override protected def exec() {
    agent.lifetimeCycle(agent.env)
    if(testConditions) notifyFinished()
    else if (pauseBetweenExecs.toMillis > 0) scheduler.scheduleOnce(pauseBetweenExecs, agent.actorRef, Exec)
    else agent.actorRef ! Exec
  }
}

trait AgentExecutionEnvironmentStopCondition[Position, EnvState, EnvGlobal, Action <: AbstractAction, Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                                   Ag <: AgentWithActor[Position, EnvState, EnvGlobal, Action, Env, _ <: ActorAgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env, Ag]]] 
  extends AgentConditionalExecution[Position, EnvState, EnvGlobal, Action, Env, Ag, (EnvGlobal, Set[EnvState])]
{
  private def envApi = agent.env.blocking

  def testCondition: StopCondition => Boolean = _(envApi.globalState -> envApi.visibleStates.values.toSet)
}