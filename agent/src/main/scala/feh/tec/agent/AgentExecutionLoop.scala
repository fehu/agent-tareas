package feh.tec.agent

import scala.concurrent.{ExecutionContext, Await, Future}
import akka.actor.{Scheduler, ActorSystem, ActorRef, Actor}
import scala.concurrent.duration._
import akka.pattern._

trait AgentExecutionLoop{
  type Execution
  def execution: Execution

  def pauseBetweenExecs: FiniteDuration
  def execControlTimeout: FiniteDuration

  def isCurrentlyExecuting: Boolean

}

trait PausableAgentExecution {
  self: AgentExecutionLoop =>

  def pause()
  def resume()
}

trait ActorAgentExecutionLoop[+Ag <: AgentWithActor[Env, ActorAgentExecutionLoop[Ag]] forSome {type Env <: Environment[Env]}]
  extends AgentExecutionLoop
{
  def receive: PartialFunction[Any, Option[Any]]
  def agent: Ag

  protected implicit def executionContext = agent.env.sys.executionContext
  protected def scheduler = agent.env.sys.scheduler
}

trait ActorAgentExec[Ag <: AgentWithActor[Env, ActorAgentExecutionLoop[Ag]] forSome {type Env <: Environment[Env]}]
  extends ActorAgentExecutionLoop[Ag]
{
  protected def exec(ag: Ag)
  protected def exec(): Unit = exec(agent)
}

trait ByTimerAgentExecution // todo: very important

trait AgentExecutionStopPauseImplementation[Ag <: AgentWithActor[Env, ActorAgentExecutionLoop[Ag]] forSome {type Env <: Environment[Env]}]{
  self: ActorAgentExec[Ag] with PausableAgentExecution =>

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
    case Exec => execActor ! Exec; None
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

  import akka.actor.ActorDSL._

  def actorSystem: ActorSystem
  protected val execActor = actor(actorSystem)(new Act {
    become{
      case Exec => exec()
    }
  })
  
  protected def stopExec: StopFunc = () => agent.actorRef
    .ask(Stop)(execControlTimeout)
    .mapTo[Stopped.type]
  
  def startExec(): StopFunc =
    if(isCurrentlyExecuting) sys.error("the actor is already executing some task")
    else {
      stoppedFlag = false
      pausedFlag = false
      agent.actorRef ! Exec
      println("'Exec' message sent to the exec loop")
      stopExec
    }

  def execution: Execution = () => startExec()
}

trait AgentInfiniteExecution[Ag <: AgentWithActor[Env, ActorAgentExecutionLoop[Ag]] forSome {type Env <: Environment[Env]}]
  extends ActorAgentExecutionLoop[Ag] with PausableAgentExecution
    with AgentExecutionStopPauseImplementation[Ag] with ActorAgentExec[Ag]
{
  protected def exec(ag: Ag) {
    ag.lifetimeCycle(ag.env)
    if (pauseBetweenExecs.toMillis > 0) scheduler.scheduleOnce(pauseBetweenExecs, ag.actorRef, Exec)
    else ag.actorRef ! Exec
  }
}

trait AgentConditionalExecution[StopConditionParams <: Product, Ag <: AgentWithActor[Env, ActorAgentExecutionLoop[Ag]] forSome {type Env <: Environment[Env]}]
  extends AgentInfiniteExecution[Ag]
{
  type StopCondition = StopConditionParams => Boolean
  
  def stopConditions: Set[StopCondition]

  def testCondition: StopCondition => Boolean

  def testConditions = (false /: stopConditions)((acc, c) => acc || testCondition(c))

  def notifyFinished()

  override protected def exec(ag: Ag) {
    ag.lifetimeCycle(ag.env)
    if(testConditions) notifyFinished()
    else if (pauseBetweenExecs.toMillis > 0) scheduler.scheduleOnce(pauseBetweenExecs, ag.actorRef, Exec)
    else ag.actorRef ! Exec
  }
}

trait AgentExecutionEnvironmentStopCondition[Env <: Environment[Env], Ag <: AgentWithActor[Env, ActorAgentExecutionLoop[Ag]]]
  extends AgentConditionalExecution[(Env#Global, Set[Env#State]), Ag]
{
  private def envApi = agent.env.blocking

  def testCondition: StopCondition => Boolean = _(envApi.globalState -> envApi.visibleStates.values.toSet)
}

trait SimultaneousAgentsExecutor extends AgentExecutionLoop
{
  final type Ag = Agent[Env, SimultaneousAgentsExecutor] forSome {type Env <: Environment[Env]}

  def register(agent: Ag*)
  protected def agents: Set[Ag]
}
