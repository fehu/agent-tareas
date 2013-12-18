package feh.tec.agentes.tarea1

import feh.tec.agent._
import feh.tec.world._
import scala.concurrent.duration.FiniteDuration
import akka.actor.{ActorRef, Props, ActorSystem, Actor}
import akka.event.Logging
import feh.tec.agent.StatelessAgentPerformanceMeasure.Criterion


object Agent{
  type Position = Environment.Coordinate
  type EnvState = Environment.State
  type EnvGlobal = Environment.Global
  type Action = Environment.Action
  type Env = Environment

  type Tile = Environment.Tile

  type Easel = Environment.Easel // todo: shouldn't be here

  class Measure extends StatelessAgentPerformanceDoubleMeasure[Env, Measure]
}

import Agent._

abstract class AbstractAgent[Exec <: ActorAgentExecutionLoop[AbstractAgent[Exec]]]
                (val env: AbstractAgent[Exec]#EnvRef,
                 val mainPerformanceCriteria: Seq[Criterion[Env, Measure]],
                 val worldStateBuilder: WorldStateBuilder[Env, Tile, Map],
                 val shortestRouteFinder: ShortestRouteFinder[Map, Tile, Position],
                 val measure: Measure)
                (implicit val execLoopBuilder: ExecLoopBuilder[AbstractAgent[Exec], Exec])
  extends Agent[Env, Exec] with AgentWithActor[Env, Exec]
    with IdealRationalAgent[Env, Exec, Measure]
    with InAbstractWorldEnvironment[Env, Tile, Map]
{
  agent: DecisiveAgent[Env, Exec] =>

  protected def actorSystem: ActorSystem

  private var _criteria: Measure#Criteria = mainPerformanceCriteria.asInstanceOf[measure.Criteria]
  def performanceCriteria = _criteria
  def withCriteria[R](c: Measure#Criteria)(f: => R): R = {
    val old = performanceCriteria
    _criteria = c
    val r = f
    _criteria = old
    r
  }


  def calcPerformance(prediction: Env#Prediction) = measure.performance(prediction)(performanceCriteria.asInstanceOf[measure.Criteria])

  lazy val executionLoop: Exec = execLoopBuilder.buildExec(agent)

  protected def abstractAgentActorProps = Props(classOf[AbstractAgentActor], executionLoop.receive)

  val actorRef: ActorRef = actorSystem.actorOf(abstractAgentActorProps)
}

class AbstractAgentActor(agentReceive: PartialFunction[Any, Option[Any]]) extends Actor{
  val log = Logging(context.system, this)
  def receive: Actor.Receive = agentReceive andThen (_ match{
    case Some(msg) => sender ! msg
    case None =>
  })
}


trait ExecLoopBuilder[Ag <: AbstractAgent[Exec], Exec <: ActorAgentExecutionLoop[Ag]]{
  def buildExec(ag: Ag): Exec
}

class AgentInfiniteExecLoopBuilder[Ag <: AbstractAgent[AgentInfiniteExecution[Ag]]]
  (pauseBetweenExecs: FiniteDuration, stopTimeout: FiniteDuration, actorSystem: ActorSystem)
  extends ExecLoopBuilder[Ag, AgentInfiniteExecution[Ag]]
{
  outer =>

  def buildExec(ag: Ag) =
    new AgentInfiniteExecution[Ag]{
      def agent: Ag = ag
      def pauseBetweenExecs: FiniteDuration = outer.pauseBetweenExecs
      def execControlTimeout: FiniteDuration = outer.stopTimeout
      def actorSystem: ActorSystem = outer.actorSystem
    }
}
  