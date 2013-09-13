package feh.tec.agentos.tarea1

import feh.tec.agentos.tarea1.DummyMapGenerator.DummyMapGeneratorRandomPositionSelectHelper
import feh.tec.agent.{IdealDummyAgent, AgentInfiniteExecution, AgentId, DummyAgent}
import java.util.UUID
import feh.tec.map.{SimpleDirection, ShortestRouteFinder, MapStateBuilder, MapSnapshotBuilder}
import akka.actor.{Props, ActorSystem}
import scala.concurrent.duration._
import feh.tec.agent.StatelessAgentPerformanceMeasure.Criterion
import feh.tec.agentos.tarea1.Tarea1.Agents.MyDummyAgent
import scala.concurrent.Future
import feh.tec.visual.api.{SquareMapDrawOptions, MapRenderer}
import feh.tec.visual.{NicolLike2DEasel, LwjglTile2DIntRenderer, LwjglSquare2DMapRenderer}
import nicol._
import nicol.input.Key._
import feh.tec.agent.AgentId
import feh.tec.agentos.tarea1.OverseerTimeouts
import feh.tec.agent.StatelessAgentPerformanceMeasure.Criterion
import feh.tec.agent.AgentId
import feh.tec.agentos.tarea1.OverseerTimeouts
import feh.tec.agent.StatelessAgentPerformanceMeasure.Criterion
import feh.tec.agentos.tarea1.Agent.Measure
import Map._

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

    def myDummyAgentProps(e: Env#Ref,
                          criteria: Seq[Criterion[Position, EnvState, EnvGlobal, Action, Env, Measure]],
                          findPossibleActions: MyDummyAgent => MyDummyAgent#Perception => Set[Action],
                          _id: AgentId) =
      Props(classOf[MyDummyAgent], e, criteria, Environment.mapStateBuilder, shortestRouteFinder)

    def myDummyAgent(e: Env#Ref,
                     criteria: Seq[Criterion[Position, EnvState, EnvGlobal, Action, Env, Measure]],
                     findPossibleActions: MyDummyAgent => MyDummyAgent#Perception => Set[Action],
                     _id: AgentId)
                    (implicit actorSystem: ActorSystem) =
      actorSystem.actorOf(myDummyAgentProps(e, criteria, findPossibleActions, _id)).asInstanceOf[MyDummyAgent]


    class MyDummyAgent(e: Env#Ref,
                       criteria: Seq[Criterion[Position, EnvState, EnvGlobal, Action, Env, Measure]],
                       findPossibleActions: MyDummyAgent => MyDummyAgent#Perception => Set[Action],
                       _id: AgentId
                        )
      extends AbstractAgent[DummyExec](e, criteria, Environment.mapStateBuilder, shortestRouteFinder)
        with IdealDummyAgent[Position, EnvState, EnvGlobal, Action, Env, DummyExec, Measure]
    {
      override val id: AgentId = _id

      def possibleBehaviors(currentPerception: Perception): Set[Action] = findPossibleActions(this)(currentPerception)
    }
  }

  object Environment{
    lazy val xRange = 0 until 10
    lazy val yRange = 0 until 10

    def moveRecursively(env: Environment,
                        currPosition: (Int, Int),
                        direction: SimpleDirection,
                        moved: Option[MapObj],
                        firstPass: Boolean = true): Environment =
      if(moved.isEmpty && !firstPass) env
      else {
        val old = env.get(currPosition).contents
        if(!firstPass && old.exists(_.isAgent)) return env // if there is only one agent on map, it would mean that we've just moved a whole row/column
        env.transformTile(currPosition)(_.copy(contents = moved))
        moveRecursively(env, env.positionTo(currPosition, direction), direction, old, firstPass = false)
      }

    // todo: needs agent reference to know it's position
    def effects: PartialFunction[Move, Environment => Environment] = Move.direction andThen {
      dir => env =>
        val pos = env.agentsPositions.head._2.coordinate // todo: this will work if there is only one agent on map
        val tile = env.tileTo(pos, dir)
        tile.contents match{
          case Some(Hole(None)) => // cannot move to a hole
            env
          case None | Some(Plug()) | Some(Hole(Some(_))) => // move and move plugs recursively if any
            assert(env.get(pos).contents.exists(_.isAgent), "agent isn't here ...")

            moveRecursively(env, pos, dir, None)
          case other => sys.error(s"unexpected object on map $other")
        }
    }

    val initGlobal = NoGlobal

    val mapStateBuilder: MapStateBuilder[Agent.Position, Agent.Tile, Map, Agent.EnvState] = new MStateBuilder(Agents.Id.dummy) // todo: id
  }

  val timeouts = OverseerTimeouts(10, 10, 100 millis, 10 millis, 30 millis)

  def overseerActorProps(env: Environment,
                         timeouts: OverseerTimeouts,
                         mapRenderer: MapRenderer[Map, Agent.Tile, Agent.Position, Agent.Easel],
                         easel: Agent.Easel,
                         mapDrawConfig: Agent.Easel#MDrawOptions)
                        (implicit actorSystem: ActorSystem) =
    Props(classOf[Overseer], actorSystem, env, mapRenderer, easel, mapDrawConfig, Environment.mapStateBuilder, timeouts)

  def overseer(env: Environment,
               timeouts: OverseerTimeouts,
               mapRenderer: MapRenderer[Map, Agent.Tile, Agent.Position, Agent.Easel],
               easel: Agent.Easel,
               mapDrawConfig: Agent.Easel#MDrawOptions)
              (implicit actorSystem: ActorSystem) =
    actorSystem.actorOf(overseerActorProps(env, timeouts, mapRenderer, easel, mapDrawConfig))

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
  def findPossibleActions: MyDummyAgent => MyDummyAgent#Perception => Set[Action]
}


object Tarea1App extends App{
  import Tarea1._

  implicit val actorSystem = ActorSystem()

  val setup: Tarea1AppSetup = new Tarea1AppSetup {
    import Agent._

    def relativePosition(of: Position, what: Position): SimpleDirection = ???

    def findPossibleActions: MyDummyAgent => MyDummyAgent#Perception => Set[Action] = ag => perc =>
      perc.mapSnapshot.getSnapshot(perc.position).neighboursSnapshots
        .withFilter(_.asTile.contents.filterNot(_.isHole).isDefined)
        .map(tile => relativePosition(perc.position, tile.coordinate))
        .map(Move(_)).toSet

    def criteria(assess: Measure#Snapshot => Measure#Measure) =
      Criterion[Position, EnvState, EnvGlobal, Action, Env, Measure](assess)

    def criteria: Seq[Criterion[Position, EnvState, EnvGlobal, Action, Env, Measure]] =
      criteria(_ => 1) :: Nil // todo
  }

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

  val ag = Tarea1.Agents.myDummyAgent(overseer.ref, setup.criteria, setup.findPossibleActions, Agents.Id.dummy)

  def startNicol() = Init("Tarea1 v. 0.01", 800, 600) >> StubScene

  startNicol()

  val agStop = ag.execution()
}


object StubScene extends LoopScene with SyncableScene with ShowFPS{
  def update: Option[Scene] = {
    sync
    showFPS

    keyEvent {
      e =>
        e released {
          case _ =>
        }
        e pressed {
          case "escape" =>
//            NicolTestApp.stop
            End
        }
    }
  }
}