package feh.tec.agentes.tarea1

import scala.language.postfixOps
import feh.tec.agentes.tarea1.DummyMapGenerator.DummyMapGeneratorRandomPositionSelectHelper
import feh.tec.agent._
import java.util.{Calendar, UUID}
import feh.tec.map._
import akka.actor.ActorSystem
import scala.concurrent.duration._
import feh.tec.agentes.tarea1.Tarea1.Agents.{ConditionalExec, MyDummyAgent}
import feh.tec.visual.api.{StringAlignment, MapRenderer, BasicStringDrawOps}
import feh.tec.visual.{PauseScene, NicolLike2DEasel}
import nicol._
import Map._
import feh.tec.util._
import feh.tec.agentes.tarea1.Criteria.{DistanceToClosestPlugAndHoleCriterion, NumberOfHolesCriterion, PlugsMovingAgentCriteria}
import scala.Predef
import scala.concurrent.Await
import feh.tec.visual.api.StringAlignment.Center
import java.awt.Color
import feh.tec.agent.StatelessAgentPerformanceMeasure.Criterion
import scala.Some
import feh.tec.agent.AgentId
import scala.collection.mutable
import feh.tec.agent.AgentDecision.{FailsafeDecisionStrategy, ExtendedCriteriaBasedDecision, DecisionStrategy}
import feh.tec.agentes.tarea1.Tarea1.Agents.ExecLoopBuilders.PauseBetweenExecs

object Tarea1 {
  object Debug extends GlobalDebuggingSetup

  lazy val defaultPauseBetweenExecs = 0.1 seconds span

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
        ConditionalExec(ag.asInstanceOf[MyDummyAgent[ConditionalExec]], pauseBetweenExecs, execControlTimeout, stopConditions, onFinished)
    }

    object ExecLoopBuilders{
      val execControlTimeout = 1 second
      val stopEnvCondition: ConditionalExec#StopCondition = _ match{
        case (_, states) => !states.exists(_.hole) || !states.exists(_.plug)
      }

      case class PauseBetweenExecs(dur: FiniteDuration)
      
      implicit def infinite(implicit pause: PauseBetweenExecs): ExecLoopBuilder[AbstractAgent[InfExec], InfExec] = InfExecBuilder(pause.dur, execControlTimeout)
      implicit def environmentCondition(implicit pause: PauseBetweenExecs): ExecLoopBuilder[AbstractAgent[ConditionalExec], ConditionalExec] =
        ConditionalExecBuilder(pause.dur, execControlTimeout, Set(stopEnvCondition), Tarea1App.setFinishedScene.lifted)
    }


    object MyDummyAgent{
      type Exec[Self <: Exec[Self]] = ActorAgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env, MyDummyAgent[Self]]
    }
    class MyDummyAgent[Exec <: ActorAgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env, MyDummyAgent[Exec]]](
                       e: Env#Ref,
                       criteria: Measure#Criteria,
                       backupCriteria: Measure#Criteria,
                       findPossibleActions: MyDummyAgent[Exec] => MyDummyAgent[Exec]#Perception => Set[Action],
                       _id: AgentId,
                       val foreseeingDepth: Int)
                      (implicit val actorSystem: ActorSystem, exBuilder: ExecLoopBuilder[AbstractAgent[Exec], Exec])
      extends AbstractAgent[Exec](e, criteria, Environment.mapStateBuilder, shortestRouteFinder, Measure)
        with IdealForeseeingDummyAgent[Position, EnvState, EnvGlobal, Action, Env, Exec, Measure]
        with GlobalDebugging
    {
      agent =>

      override val id: AgentId = _id

      def possibleBehaviors(currentPerception: Perception): Set[Action] = {
        currentPerception.debugLog("searching for possible behaviors. position = " + _.position)
        findPossibleActions(this)(currentPerception)
      }

      def perceiveFromSnapshot(sn: EnvironmentSnapshot[Position, EnvState, EnvGlobal, Action, Env]): Perception =
        sense(sn.asInstanceOf[MapEnvironmentSnapshot[Map, Tile, Position, EnvState, EnvGlobal, Action, Env]]) // todo: casting

      protected def setup: DebuggingSetup = Tarea1.Debug

      def debugMessagePrefix: String = "[MyDummyAgent]"

      def lastDecision = currentDecisionExplanation

      private var _currentDecisionExplanation: ActionExplanation = _
      def currentDecisionExplanation = Option(_currentDecisionExplanation)
      protected def currentDecisionExplanation_=(ea: ActionExplanation) = _currentDecisionExplanation = ea

      def notifyDecision(a: ActionExplanation): Unit = {
        currentDecisionExplanation = a
        debugLog(s"Decision taken: $a")
      }

      override protected def createBehaviorSelectionStrategy: DecisionStrategy[Action, DecisionArg, ExtendedCriteriaBasedDecision[ActionExplanation, Position, EnvState, EnvGlobal, Action, Env, Exec, Measure]] =
        {
          val strategy = super.createBehaviorSelectionStrategy
          FailsafeDecisionStrategy.Builder(strategy)
            .append(
              p => {
                val t = p
                val y = t.consideredOptionsCriteriaValues.flatten.distinct.size == 1
                y
              },
              new IdealForeseeingAgentDecisionStrategies.MeasureBasedForeseeingDecisionStrategy[Position, EnvState, EnvGlobal, Action, Env, Exec, Measure, agent.type](foreseeingDepth, debug){
                override lazy val rewriteCriteria: Option[Measure#Criteria] = Some(backupCriteria)
              }
            )
            .build()
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
  def backupCriteria: Measure#Criteria
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
                        (of: Position, relativelyTo: Position): SimpleDirection = {
      import ranges._
      import SimpleDirection._

      of -> relativelyTo match{
        case ((x1, y1), (x2, y2)) if x1 == x2 && (y2 == y1 + 1 || y1 == yRange.max && y2 == yRange.min) => Down
        case ((x1, y1), (x2, y2)) if x1 == x2 && (y2 == y1 - 1 || y1 == yRange.min && y2 == yRange.max) => Up
        case ((x1, y1), (x2, y2)) if y1 == y2 && (x2 == x1 - 1 || x1 == xRange.min && x2 == xRange.max) => Left
        case ((x1, y1), (x2, y2)) if y1 == y2 && (x2 == x1 + 1 || x1 == xRange.max && x2 == xRange.min) => Right
        case (c1, c2) => sys.error(s"$c1 and $c2 are not neighbouring tiles")
      }
    }

    def findPossibleActions[Exec <: ActorAgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env, MyDummyAgent[Exec]]]
      (ag: MyDummyAgent[Exec], perc: MyDummyAgent[Exec]#Perception): Set[Action] =
        perc.mapSnapshot.getSnapshot(perc.position).neighboursSnapshots
          .filterNot(_.asTile.contents.exists(_.isHole))
          .map(tile => relativePosition(perc.mapSnapshot.coordinates)(perc.position, tile.coordinate))
          .map(Move(_)).toSet

    def criteria: Seq[Criterion[Position, EnvState, EnvGlobal, Action, Env, Measure]] =
      new PlugsMovingAgentCriteria
        with NumberOfHolesCriterion
      {
        def numberOfHolesWeight: Double = -10

//        def distanceToClosestPlugWeight: Float = -1
//        protected def shortestRouteFinder: MapShortestRouteFinder = new MapShortestRouteFinder

        def debug: Boolean = CriteriaDebug
      }.toList

    def backupCriteria: Measure#Criteria = new PlugsMovingAgentCriteria with DistanceToClosestPlugAndHoleCriterion{
      def agentId: AgentId = Agents.Id.dummy
      def distanceToClosetPlugWeight: Float = -1
      def distanceFromPlugToClosestHoleWeight: Float = -1

      lazy val floydWarshall = new FloydWarshall
      protected def clearDistanceMap(): Unit = minDistsMapCache.clear()


      val minDistsMapCache = mutable.HashMap.empty[Set[Position], FloydWarshall#MinDistMap]

      def getHolesCoordinates(s: Measure.Snapshot) = s.asEnv.mapSnapshot.tilesSnapshots.withFilter(_.asTile.exists(_.isHole)).map(_.coordinate).toSet

      def minDists(s: Measure.Snapshot) = {
        val holesCoords = getHolesCoordinates(s)
        minDistsMapCache.getOrElse(holesCoords, {
          val t1 = Calendar.getInstance.getTimeInMillis
          val gr = Graph.mapAsGraph(s.asEnv.mapSnapshot)
          val t2 = Calendar.getInstance.getTimeInMillis
          val d = floydWarshall.findMinimalDistances(gr)
          val t3 = Calendar.getInstance.getTimeInMillis
          debugLog(s"graph by snapshot building time: ${t2 - t1}")
          debugLog(s"min distance by graph building time: ${t3 - t2}")

          minDistsMapCache += holesCoords -> d
          d
        })
      }

      def findClosetRespectingHoles(relativelyTo: Position, cond: (Tile#Snapshot) => Boolean, sn: Measure.Snapshot): Seq[Tile#Snapshot] =
        minDists(sn).withFilter{case ((c1, c2), d) => c1 == relativelyTo && cond(sn.asEnv.mapSnapshot.getSnapshot(c2))}.map(p => sn.asEnv.mapSnapshot.getSnapshot(p._1._2)).toSeq
      def findClosetDisregardingHoles(relativelyTo: Position, cond: Tile#Snapshot => Boolean, sn: Measure.Snapshot): Seq[Tile#Snapshot] =
        sn.asEnv.mapSnapshot.tilesSnapshots.filter(cond).filterMin(distanceDisregardingHoles(relativelyTo, _, sn))

      def distanceRespectingHoles(p1: Position, p2: Position, sn: Measure.Snapshot): Int =
        minDists(sn).collectFirst{
          case ((`p1`, `p2`), d) => d
        }.get
      def distanceDisregardingHoles(p1: Position, p2: Position, sn: Measure.Snapshot): Int = {
        import scala.math._
        val distX = min(abs(p1._1 - p2._1), abs(p2._1 - p1._1))
        val distY = min(abs(p1._2 - p2._2), abs(p2._2 - p1._2))
        distX + distY
      }

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

  def processArgsForTimeSpan(): Option[FiniteDuration] =
    if(args.nonEmpty) {
      val dur = Duration(args.mkString(" ")).ensuring(_.isFinite())
      Some(FiniteDuration(dur.length, dur.unit))
    }
    else None

  implicit val pauseBetweenExecs = PauseBetweenExecs(processArgsForTimeSpan getOrElse defaultPauseBetweenExecs)

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
    setup.backupCriteria,
    (setup.findPossibleActions[Exec] _).curried,
    Agents.Id.dummy,
    foreseeingDepth)

  lazy val finishedScene = new FinishedScene(renderMap.lifted)

  val game = new NicolLikeTarea1Game(env, AgentRef.create(ag))

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

  game.start()
}

object Tarea1EndScene extends End(Tarea1App.terminate())

protected object Finished{
  var flag = false
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
  pausedMessage = "Agent Execution Paused" -> BasicStringDrawOps[NicolLike2DEasel](Center, Color.lightGray, "Arial", 20F, 5)
)

class FinishedScene(render: () => Unit)(implicit easel: NicolLike2DEasel) extends PauseScene[NicolLike2DEasel] (
  onPause = render,
  onResume = {}.lifted,
  endScene = Tarea1EndScene.lifted,
  resumeScene = () => null,
  pausedMessage = "Agent Execution Finished" -> BasicStringDrawOps[NicolLike2DEasel](StringAlignment.Left, Color.green, "Arial", 20F, 5)
){
  override def processPressedKey: PartialFunction[String, Scene] = PartialFunction[String, Scene]{
    case `resumeKey` => this
    case `quitKey` => endScene()
  }

  override def messagePosition = (530, 300)
}