package feh.tec.agentos.tarea1

import feh.tec.agentos.tarea1.DummyMapGenerator.DummyMapGeneratorRandomPositionSelectHelper
import feh.tec.agent.{IdealDummyAgent, AgentInfiniteExecution, AgentId, DummyAgent}
import java.util.UUID
import feh.tec.map.{ShortestRouteFinder, MapStateBuilder, MapSnapshotBuilder}
import akka.actor.ActorSystem
import scala.concurrent.duration._
import feh.tec.agent.StatelessAgentPerformanceMeasure.Criterion
import feh.tec.agentos.tarea1.Tarea1.Agents.MyDummyAgent
import scala.concurrent.Future
import feh.tec.visual.api.{SquareMapDrawOptions, MapRenderer}
import feh.tec.visual.{NicolLike2DEasel, LwjglTile2DIntRenderer, LwjglSquare2DMapRenderer}

object Tarea1 {
  object Agents{
    object Id{
      val dummy = AgentId(UUID.randomUUID())
    }

    import Agent._

    val shortestRouteFinder: ShortestRouteFinder[Map, Tile, Position] = ???

    case class DummyExec(agent: MyDummyAgent,
                         pauseBetweenExecs: FiniteDuration,
                         stopTimeout: FiniteDuration)
      extends AgentInfiniteExecution[Position, EnvState, EnvGlobal, Action, Env, MyDummyAgent]

    implicit def execBuilder: ExecLoopBuilder[/*MyDummyAgent*/ AbstractAgent[DummyExec], DummyExec] = ???
    //

    class MyDummyAgent(e: Env#Ref,
                       criteria: Seq[Criterion[Position, EnvState, EnvGlobal, Action, Env, Measure]],
                       findPossibleActions: MyDummyAgent => Set[Action],
                       _id: AgentId
                        )
      extends AbstractAgent[DummyExec](e, criteria, Environment.mapStateBuilder, shortestRouteFinder)
        with IdealDummyAgent[Position, EnvState, EnvGlobal, Action, Env, DummyExec, Measure]
    {
      override val id: AgentId = _id

      def possibleBehaviors: Set[Action] = findPossibleActions(this)
    }
  }

  object Environment{
    lazy val xRange = 0 until 10
    lazy val yRange = 0 until 10

    def effects: PartialFunction[Move, Environment => Environment] = ??? //{
//      case MoveNorth =>
//    }

    val initGlobal = NoGlobal

    val mapStateBuilder: MapStateBuilder[Agent.Position, Agent.Tile, Map, Agent.EnvState] = ???
  }

  val timeouts = OverseerTimeouts(10, 10, 100 millis, 10 millis, 30 millis)

  def overseer(env: Environment, timeouts: OverseerTimeouts,
               mapRenderer: MapRenderer[Map, Agent.Tile, Agent.Position, Agent.Easel],
               easel: Agent.Easel,
               mapDrawConfig: Agent.Easel#MDrawOptions)
              (implicit actorSystem: ActorSystem) =
    new Overseer(actorSystem, env, mapRenderer, easel, mapDrawConfig, Environment.mapStateBuilder, timeouts)

  def environment(ag: Option[AgentId]) =
    new Environment(
      buildTilesMap = DummyMapGenerator.withHelpers[DummyMapGeneratorRandomPositionSelectHelper]
        .buildTilesMap(Environment.xRange, Environment.yRange)(LwjglTest.mapBuildingFunc(ag)) andThen (_.values.toSeq),
      Environment.xRange,
      Environment.yRange,
      Environment.effects,
      Environment.initGlobal,
      Environment.mapStateBuilder
    )
}

trait Tarea1AppSetup{
  import Agent._

  def criteria: Seq[Criterion[Position, EnvState, EnvGlobal, Action, Env, Measure]]
  def findPossibleActions: MyDummyAgent => Set[Action]
}


object Tarea1App extends App{
  import Tarea1._

  implicit val actorSystem = ActorSystem()

  val setup: Tarea1AppSetup = ???

  object visual{
    val tileSideSize = 50

    val easel = LwjglTest.createEasel
    val mapRenderer = LwjglTest.createMapRenderer
    val howToDrawTheMap = new SquareMapDrawOptions[NicolLike2DEasel]{
      def tileSideSize: NicolLike2DEasel#CoordinateUnit = visual.tileSideSize
    }
  }

  val env = environment(Option(Agents.Id.dummy))
  val overseer = Tarea1.overseer(env, timeouts, visual.mapRenderer, visual.easel, visual.howToDrawTheMap)

  val ag = new MyDummyAgent(overseer.ref, setup.criteria, setup.findPossibleActions, Agents.Id.dummy)

  val agStop = ag.execution()

}

