package feh.tec.agentes.tarea1

import scala.language.postfixOps
import feh.tec.agentes.tarea1.DummyMapGenerator.DummyMapGeneratorRandomPositionSelectHelper
import feh.tec.agent._
import java.util.UUID
import feh.tec.map._
import akka.actor.ActorSystem
import scala.concurrent.duration._
import feh.tec.agentes.tarea1.Tarea1.Agents.MyDummyAgent
import feh.tec.visual.api.{Easel, SquareMapDrawOptions, MapRenderer}
import feh.tec.visual.NicolLike2DEasel
import nicol._
import nicol.input.Key._
import Map._
import feh.tec.util.{DebuggingSetup, GlobalDebugging, GlobalDebuggingSetup, LiftWrapper}
import feh.tec.agentes.tarea1.Criteria.{DistanceToClosestPlugCriterion, ClosestPairIntraDistanceCriterion, NumberOfHolesCriterion, PlugsMovingAgentCriteria}
import feh.tec.agent.StatelessAgentPerformanceMeasure.Criterion
import feh.tec.agent.AgentId
import nicol.Init
import scala.Predef

object Tarea1 {
  object Debug extends GlobalDebuggingSetup

  val pauseBetween = 0.5 seconds span

  object Agents{
    object Id{
      val dummy = AgentId(UUID.randomUUID())
    }

    import Agent._

    val shortestRouteFinder: ShortestRouteFinder[Map, Tile, Position] = new MapShortestRouteFinder

    case class DummyExec(agent: MyDummyAgent,
                         pauseBetweenExecs: FiniteDuration,
                         stopTimeout: FiniteDuration)
      extends AgentInfiniteExecution[Position, EnvState, EnvGlobal, Action, Env, MyDummyAgent]

    case class DummyExecBuilder(pauseBetweenExecs: FiniteDuration,
                                stopTimeout: FiniteDuration)
      extends ExecLoopBuilder[AbstractAgent[DummyExec], DummyExec]
    {
      def buildExec(ag: AbstractAgent[DummyExec]): DummyExec =
        DummyExec(ag.asInstanceOf[MyDummyAgent], pauseBetweenExecs, stopTimeout)
    }

    implicit def execBuilder: ExecLoopBuilder[AbstractAgent[DummyExec], DummyExec] = DummyExecBuilder(pauseBetween, 100 millis)

    class MyDummyAgent(e: Env#Ref,
                       criteria: Seq[Criterion[Position, EnvState, EnvGlobal, Action, Env, Measure]],
                       findPossibleActions: MyDummyAgent => MyDummyAgent#Perception => Set[Action],
                       _id: AgentId,
                       val foreseeingDepth: Int)
                      (implicit val actorSystem: ActorSystem)
      extends AbstractAgent[DummyExec](e, criteria, Environment.mapStateBuilder, shortestRouteFinder)
//        with IdealDummyAgent[Position, EnvState, EnvGlobal, Action, Env, DummyExec, Measure]
        with IdealForeseeingDummyAgent[Position, EnvState, EnvGlobal, Action, Env, DummyExec, Measure]
        with GlobalDebugging
    {
      override val id: AgentId = _id

      def possibleBehaviors(currentPerception: Perception): Set[Action] = {
        currentPerception.debugLog("searching for possible behaviors. position = " + _.position)
        findPossibleActions(this)(currentPerception)
      }

      protected def snapshotToPerception(sn: EnvironmentSnapshot[Position, EnvState, EnvGlobal, Action, Env]): Perception =
        sense(sn.asInstanceOf[MapEnvironmentSnapshot[Map, Tile, Position, EnvState, EnvGlobal, Action, Env]]) // todo: casting

      protected def setup: DebuggingSetup = Tarea1.Debug

      def debugMessagePrefix: String = "[MyDummyAgent]"
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
        val contents = env.get(currPosition).contents
        contents -> moved match{
          case (Some(Hole()), Some(Plug())) => env.transformTile(currPosition)(_.copy(contents = None)) // hole plugged
          case (Some(AgentAvatar(_)), _) if !firstPass => env // if there is only one agent on map, it would mean that we've just moved a whole row/column of plugs
          case  _ =>
            env.transformTile(currPosition)(_.copy(contents = moved))
            moveRecursively(env, env.positionTo(currPosition, direction), direction, contents, firstPass = false)
        }
      }

    // todo: needs agent reference to know it's position
    def effects: PartialFunction[Move, Environment => Environment] = Move.direction andThen {
      dir => env =>
        val pos = env.agentsPositions.head._2.coordinate // todo: this will work if there is only one agent on map
        val tile = env.tileTo(pos, dir)
        tile.contents match{
          case Some(Hole()) => // cannot move to a hole
            env
          case None | Some(Plug()) => // move and move plugs recursively if any
            assert(env.get(pos).contents.exists(_.isAgent), "agent isn't here ...")
            moveRecursively(env, pos, dir, None)
          case other => sys.error(s"unexpected object on map $other")
        }
    }

    val initGlobal = NoGlobal

    val mapStateBuilder: MapStateBuilder[Agent.Position, Agent.Tile, Map, Agent.EnvState] = new MStateBuilder(Agents.Id.dummy) // todo: id
  }

  val timeouts = OverseerTimeouts(
    defaultBlockingTimeout = 10,
    defaultFutureTimeout = 10,
    predictMaxDelay =  100 millis,
    foreseeMaxDelay = 100 millis,
    getMapMaxDelay =  10 millis,
    positionMaxDelay =  30 millis)

  def overseer(env: Environment,
               timeouts: OverseerTimeouts,
               mapRenderer: MapRenderer[Map, Agent.Tile, Agent.Position, Agent.Easel],
               easel: Agent.Easel,
               mapDrawConfig: Agent.Easel#MDrawOptions)
              (implicit actorSystem: ActorSystem) =
    new Overseer(actorSystem, env, mapRenderer, easel, mapDrawConfig, Environment.mapStateBuilder, timeouts)

  def environment(ag: Option[AgentId]): Environment = environment(ag,
    DummyMapGenerator.withHelpers[DummyMapGeneratorRandomPositionSelectHelper]
      .buildTilesMap(Environment.xRange, Environment.yRange)(LwjglTest.mapBuildingFunc(ag))
  )
  def environment(ag: Option[AgentId], buildTiles: (Map) => Predef.Map[(Int, Int), SqTile]): Environment =
    new Environment(
      buildTiles andThen (_.values.toSeq),
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

    def relativePosition(ranges: AbstractSquareMap[SqTile]#CoordinatesMeta)//(xRange: Range, yRange: Range)
                        (of: Position, what: Position): SimpleDirection = {
      import ranges._
      import SimpleDirection._

      of -> what match{
        case ((x1, y1), (x2, y2)) if x1 == x2 && (y2 == y1 + 1 || y1 == yRange.max && y2 == yRange.min) => Up
        case ((x1, y1), (x2, y2)) if x1 == x2 && (y2 == y1 - 1 || y1 == yRange.min && y2 == yRange.max) => Down
        case ((x1, y1), (x2, y2)) if y1 == y2 && (x2 == x1 - 1 || x1 == xRange.min && x2 == xRange.max) => Left
        case ((x1, y1), (x2, y2)) if y1 == y2 && (x2 == x1 + 1 || x1 == xRange.max && x2 == xRange.min) => Right
        case (c1, c2) => sys.error(s"$c1 and $c2 are not neighbouring tiles")
      }
    }

    def findPossibleActions: MyDummyAgent => MyDummyAgent#Perception => Set[Action] = ag => perc =>  {
      val x = perc.mapSnapshot.getSnapshot(perc.position).neighboursSnapshots
      val y = x.filterNot(_.asTile.contents.exists(_.isHole))
      val z = y.map(tile => relativePosition(perc.mapSnapshot.coordinates)(perc.position, tile.coordinate))
      val w = z.map(Move(_)).toSet
      w
    }

    def criteria: Seq[Criterion[Position, EnvState, EnvGlobal, Action, Env, Measure]] =
      new PlugsMovingAgentCriteria
//        with DistanceToClosestPlugCriterion
        with ClosestPairIntraDistanceCriterion
        with NumberOfHolesCriterion
      {
        def numberOfHolesWeight: Double = -10
        def closestHolePlugPairMeanIntraDistanceWeight: Float = -3
//        def distanceToClosestPlugWeight: Float = -1
//        def agentId: AgentId = Tarea1.Agents.Id.dummy // todo: inject or smth

        protected def guardCalculatedClosestHolePlugPairsWithIntraDistances(distMap: Predef.Map[Agent.Position, (Set[Agent.Position], Int)]) {}

        //        protected lazy val shortestRouteFinder: MapShortestRouteFinder = new MapShortestRouteFinder
//        override def toList = criterion(_ => Random.nextDouble()) :: Nil
        def debug: Boolean = true
      }.toList
  }

  object visual{
    val tileSideSize = 50

    val easel = LwjglTest.createEasel
    val mapRenderer = LwjglTest.createMapRenderer
    val howToDrawTheMap = new SquareMapDrawOptions[NicolLike2DEasel]{
      def tileSideSize: NicolLike2DEasel#CoordinateUnit = visual.tileSideSize
    }
  }

  Tarea1.Debug() = true

//  val env = environment(Option(Agents.Id.dummy))
  val env = TestEnvironment.test1(Option(Agents.Id.dummy))
  val overseer = Tarea1.overseer(env, timeouts, visual.mapRenderer, visual.easel, visual.howToDrawTheMap)

  val foreseeingDepth = 5

  val ag = new MyDummyAgent(overseer.ref, setup.criteria, setup.findPossibleActions, Agents.Id.dummy, foreseeingDepth)

  def startNicol() = {
    val game = new Tarea1Game(renderMap(visual.easel).lifted)
    game.start
    game
  }

  val game = startNicol()

  def renderMap(implicit easel: NicolLike2DEasel) = visual.mapRenderer.render(env, visual.howToDrawTheMap)

  val agStop = ag.execution()

  def terminate() = {
    agStop()
    actorSystem.stop(ag.actorRef)
    actorSystem.stop(overseer.actorRef)
    game.stop
    actorSystem.awaitTermination()
    sys.exit(0)
  }
}

class Tarea1Game(renderMap: () => Unit) extends Game(Init("Tarea1 v. 0.01", 800, 600) >> new StubScene(renderMap))

class StubScene(renderMap: () => Unit) extends LoopScene with SyncableScene with ShowFPS{
  def update: Option[Scene] = {
    sync
    showFPS

    renderMap()

    keyEvent {
      e =>
        e released {
          case _ =>
        }
        e pressed {
          case "escape" =>
            Tarea1App.terminate()
            End
        }
    }
  }
}