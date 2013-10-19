package feh.tec.agentes.tarea1

import scala.language.postfixOps
import feh.tec.agentes.tarea1.DummyMapGenerator.DummyMapGeneratorRandomPositionSelectHelper
import feh.tec.agent._
import java.util.UUID
import feh.tec.map._
import akka.actor.ActorSystem
import scala.concurrent.duration._
import feh.tec.agentes.tarea1.Tarea1.Agents.{ConditionalExec, MyDummyAgent}
import feh.tec.visual.api.{SquareMapDrawOptions, MapRenderer}
import feh.tec.visual.{PauseScene, NicolLike2DEasel}
import nicol._
import nicol.input.Key._
import Map._
import feh.tec.util._
import feh.tec.agentes.tarea1.Criteria.{ClosestPairIntraDistanceCriterion, NumberOfHolesCriterion, PlugsMovingAgentCriteria}
import scala.Predef
import scala.concurrent.Await
import feh.tec.visual.api.StringAlignment.Center
import java.awt.Color
import feh.tec.agent.StatelessAgentPerformanceMeasure.Criterion
import scala.Some
import feh.tec.agent.AgentId
import feh.tec.visual.api.BasicStringDrawOps
import nicol.Init

object Tarea1 {
  object Debug extends GlobalDebuggingSetup

  val pauseBetween = 0.5 seconds span

  object Agents{
    object Id{
      val dummy = AgentId(UUID.randomUUID())
    }

    import Agent._

    val shortestRouteFinder: ShortestRouteFinder[Map, Tile, Position] = new MapShortestRouteFinder

    case class InfExec(agent: MyDummyAgent[InfExec],
                         pauseBetweenExecs: FiniteDuration,
                         execControlTimeout: FiniteDuration)
      extends AgentInfiniteExecution[Position, EnvState, EnvGlobal, Action, Env, MyDummyAgent[InfExec]]

    case class ConditionalExec(agent: MyDummyAgent[ConditionalExec],
                               pauseBetweenExecs: FiniteDuration,
                               execControlTimeout: FiniteDuration,
                               stopConditions: Set[ConditionalExec#StopCondition],
                               onFinished: () => Unit)
      extends AgentExecutionEnvironmentStopCondition[Position, EnvState, EnvGlobal, Action, Env, MyDummyAgent[ConditionalExec]]
    {
      def notifyFinished(): Unit = onFinished()
    }

    case class InfExecBuilder(pauseBetweenExecs: FiniteDuration,
                                stopTimeout: FiniteDuration)
      extends ExecLoopBuilder[AbstractAgent[InfExec], InfExec]
    {
      def buildExec(ag: AbstractAgent[InfExec]): InfExec =
        InfExec(ag.asInstanceOf[MyDummyAgent[InfExec]], pauseBetweenExecs, stopTimeout)
    }

    case class ConditionalExecBuilder(pauseBetweenExecs: FiniteDuration,
                                      execControlTimeout: FiniteDuration,
                                      stopConditions: Set[ConditionalExec#StopCondition],
                                      onFinished: () => Unit) extends ExecLoopBuilder[AbstractAgent[ConditionalExec], ConditionalExec]{
      def buildExec(ag: AbstractAgent[ConditionalExec]): ConditionalExec =
        ConditionalExec(ag.asInstanceOf[MyDummyAgent[ConditionalExec]], pauseBetween, execControlTimeout, stopConditions, onFinished)
    }

    object ExecLoopBuilders{
      val execControlTimeout = 1 second
      val stopEnvCondition: ConditionalExec#StopCondition = _ match{
        case (_, states) => !states.exists(_.hole) || !states.exists(_.plug)
      }

      implicit def infinite: ExecLoopBuilder[AbstractAgent[InfExec], InfExec] = InfExecBuilder(pauseBetween, execControlTimeout)
      implicit def environmentCondition: ExecLoopBuilder[AbstractAgent[ConditionalExec], ConditionalExec] =
        ConditionalExecBuilder(pauseBetween, execControlTimeout, Set(stopEnvCondition), Tarea1App.setFinishedScene.lifted)
    }


    object MyDummyAgent{
      type Exec[Self <: Exec[Self]] = ActorAgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env, MyDummyAgent[Self]]
    }
    class MyDummyAgent[Exec <: ActorAgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env, MyDummyAgent[Exec]]](
                       e: Env#Ref,
                       criteria: Seq[Criterion[Position, EnvState, EnvGlobal, Action, Env, Measure]],
                       findPossibleActions: MyDummyAgent[Exec] => MyDummyAgent[Exec]#Perception => Set[Action],
                       _id: AgentId,
                       val foreseeingDepth: Int)
                      (implicit val actorSystem: ActorSystem, exBuilder: ExecLoopBuilder[AbstractAgent[Exec], Exec])
      extends AbstractAgent[Exec](e, criteria, Environment.mapStateBuilder, shortestRouteFinder)
        with IdealForeseeingDummyAgent[Position, EnvState, EnvGlobal, Action, Env, Exec, Measure]
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

      private var _currentDecisionExplanation: ExplainedAction = _
      def currentDecisionExplanation = Option(_currentDecisionExplanation)
      protected def currentDecisionExplanation_=(ea: ExplainedAction) = _currentDecisionExplanation = ea

      def notifyDecision(a: ExplainedAction): Unit = {
        currentDecisionExplanation = a
        debugLog(s"Decision taken: $a")
      }
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
      .buildTilesMap(Environment.xRange, Environment.yRange)(Lwjgl.mapBuildingFunc(ag))
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
  def findPossibleActions[Exec <: ActorAgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env, MyDummyAgent[Exec]]]
    (ag: MyDummyAgent[Exec], perc: MyDummyAgent[Exec]#Perception): Set[Action]
}


object Tarea1App extends App{
  val CriteriaDebug = false

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

    def findPossibleActions[Exec <: ActorAgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env, MyDummyAgent[Exec]]]
      (ag: MyDummyAgent[Exec], perc: MyDummyAgent[Exec]#Perception): Set[Action] = {
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

        protected def guardCalculatedClosestHolePlugPairsWithIntraDistances(distMap: Predef.Map[Agent.Position, (Agent.Position, Int)]) {}

        def debug: Boolean = CriteriaDebug
      }.toList
  }

  object visual{
    val tileSideSize = Lwjgl.Settings.tileSideSize
    val showLabels = Lwjgl.Settings.showLabels

    implicit val easel = Lwjgl.createEasel
    val mapRenderer = Lwjgl.createMapRenderer
    def howToDrawTheMap = Lwjgl.Settings.howToDrawTheMap
  }

//  Tarea1.Debug() = true

  val env = environment(Option(Agents.Id.dummy))
//  val env = TestEnvironment.test1(Option(Agents.Id.dummy))
  val overseer = Tarea1.overseer(env, timeouts, visual.mapRenderer, visual.easel, visual.howToDrawTheMap)

  val foreseeingDepth = 5

  import Tarea1.Agents.ExecLoopBuilders._
  import visual.easel
  type Exec = ConditionalExec
  val ag = new MyDummyAgent[Exec](
    overseer.ref,
    setup.criteria,
    (setup.findPossibleActions[Exec] _).curried,
    Agents.Id.dummy,
    foreseeingDepth)

  lazy val finishedScene = new FinishedScene(renderMap.lifted)

  def startNicol() = {
    val game = new Tarea1Game(renderMap(visual.easel).lifted, finishedScene.lifted)
    game.start
    game
  }

  val game = startNicol()

  def renderMap(implicit easel: NicolLike2DEasel) = visual.mapRenderer.render(env, visual.howToDrawTheMap)

  val agStop = ag.execution()

  def terminate() = {
    Await.ready(agStop(), 1 second)
    actorSystem.stop(ag.actorRef)
    actorSystem.stop(overseer.actorRef)
    game.stop
  }

  def setFinishedScene() = Finished.flag = true
  def isFinished = Finished.flag
}

class Tarea1Game(render: () => Unit, finishedScene: Lifted[FinishedScene])(implicit easel: NicolLike2DEasel)
  extends Game(Init("Tarea1 v. 0.01", 800, 600) >> new StubScene(render, finishedScene))

object Tarea1EndScene extends End(Tarea1App.terminate())

protected object Finished{
  var flag = false
}

class StubScene(renderMap: () => Unit, finishedScene: Lifted[FinishedScene])(implicit easel: NicolLike2DEasel) extends LoopScene with SyncableScene with ShowFPS{
  def update: Option[Scene] = {
    sync
    showFPS

    renderMap()

    if(Finished.flag) finishedScene()
    else keyEvent {
      e =>
        e released {
          case _ =>
        }
        e pressed {
          case "escape" => Tarea1EndScene
          case "space" =>
            Tarea1LastSceneKeeper.scene = this
            Tarea1PauseSceneKeeper.sceneOpt.getOrElse{
              val p = new Tarea1PauseScene(renderMap)
              Tarea1PauseSceneKeeper.scene = p
              p
            }
        }
    }
  }
}

object Tarea1LastSceneKeeper{
  var scene: Scene = _
}
object Tarea1PauseSceneKeeper{
  var scene: Scene = _
  def sceneOpt = Option(scene)
}

class Tarea1PauseScene(render: () => Unit)(implicit easel: NicolLike2DEasel) extends PauseScene[NicolLike2DEasel](
  onPause = {
    Tarea1App.ag.executionLoop.pause()
    render()
  }.lifted,
  onResume = Tarea1App.ag.executionLoop.resume().lifted,
  endScene = Tarea1EndScene.lifted,
  resumeScene = Tarea1LastSceneKeeper.scene.lifted,
  pausedMessage = "Agent Execution Paused" -> BasicStringDrawOps[NicolLike2DEasel](Center, Color.lightGray, "Arial", 0F, 20F)
)

class FinishedScene(render: () => Unit)(implicit easel: NicolLike2DEasel) extends PauseScene[NicolLike2DEasel] (
  onPause = render,
  onResume = {}.lifted,
  endScene = Tarea1EndScene.lifted,
  resumeScene = () => null,
  pausedMessage = "Agent Execution Finished" -> BasicStringDrawOps[NicolLike2DEasel](Center, Color.green, "Arial", 0F, 20F)
){
  override def processPressedKey: PartialFunction[String, Scene] = PartialFunction[String, Scene]{
    case `resumeKey` => this
    case `quitKey` => endScene()
  }
}