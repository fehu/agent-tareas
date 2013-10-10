package feh.tec.agentes.tarea1

import feh.tec.agent._
import feh.tec.agent.StatelessAgentPerformanceMeasure.Criterion
import feh.tec.map._
import feh.tec.util.SideEffect
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

  class Measure extends StatelessAgentPerformanceDoubleMeasure[Position, EnvState, EnvGlobal, Action, Env, Measure]{
//    override type Snapshot = EnvironmentSnapshot[Position, EnvState, EnvGlobal, Action, Env]
//      with MapEnvironmentSnapshot[Map, Tile, Position, EnvState, EnvGlobal, Action, Env]
  }
}

import Agent._

abstract class AbstractAgent[Exec <: ActorAgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env, AbstractAgent[Exec]]]
                (val env: AbstractAgent[Exec]#EnvRef,
                 val performanceCriteria: Seq[Criterion[Position, EnvState, EnvGlobal, Action, Env, Measure]],
                 val mapStateBuilder: MapStateBuilder[Position, Tile, Map, EnvState],
                 val shortestRouteFinder: ShortestRouteFinder[Map, Tile, Position])
                (implicit val execLoopBuilder: ExecLoopBuilder[AbstractAgent[Exec], Exec])
  extends Agent[Position, EnvState, EnvGlobal, Action, Env, Exec] with AgentWithActor[Position, EnvState, EnvGlobal, Action, Env, Exec]
    with IdealRationalAgent[Position, EnvState, EnvGlobal, Action, Env, Exec, Measure]
    with InAbstractMapEnvironment[Position, EnvState, EnvGlobal, Action, Env, Tile, Map]
{
  agent: DecisiveAgent[Position, EnvState, EnvGlobal, Action, Env, Exec] =>

  protected def actorSystem: ActorSystem

/*
  AgentApp.Agents.DummyExec <: feh.tec.agent.AgentExecutionLoop[(Int, Int),Agent.EnvState,Agent.EnvGlobal,Agent.Action,Agent.Env]
   */
  lazy val measure = new Measure

  protected def calcPerformance(prediction: Env#Prediction) = {
    val nonEmptyTiles = prediction.asEnv.tiles.filter(_.contents.nonEmpty).toList
    measure.performance(prediction)(performanceCriteria)
  }

  lazy val executionLoop: Exec = execLoopBuilder.buildExec(agent)

  protected def abstractAgentActorProps = Props(classOf[AbstractAgentActor], executionLoop.receive)

  val actorRef: ActorRef = actorSystem.actorOf(abstractAgentActorProps)
}

class AbstractAgentActor(agentReceive: Actor.Receive) extends Actor{
  val log = Logging(context.system, this)
  def receive: Actor.Receive = agentReceive
}


trait ExecLoopBuilder[Ag <: AbstractAgent[Exec], Exec <: ActorAgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env, Ag]]{
  def buildExec(ag: Ag): Exec
}

class AgentInfiniteExecLoopBuilder[Ag <: AbstractAgent[AgentInfiniteExecution[Position, EnvState, EnvGlobal, Action, Env, Ag]]]
  (pauseBetweenExecs: FiniteDuration, stopTimeout: FiniteDuration)
  extends ExecLoopBuilder[Ag, AgentInfiniteExecution[Position, EnvState, EnvGlobal, Action, Env, Ag]]
{
  outer =>

  def buildExec(ag: Ag) =
    new AgentInfiniteExecution[Position, EnvState, EnvGlobal, Action, Env, Ag]{
      def agent: Ag = ag
      def pauseBetweenExecs: FiniteDuration = outer.pauseBetweenExecs
      def stopTimeout: FiniteDuration = outer.stopTimeout
    }
}
  