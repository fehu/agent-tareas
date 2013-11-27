package feh.tec.agentes.tarea1

import scala.language.postfixOps
import feh.tec.agent._
import java.util.{Calendar, UUID}
import feh.tec.world._
import akka.actor.ActorSystem
import scala.concurrent.duration._
import feh.tec.agentes.tarea1.Tarea1.Agents.MyDummyAgent
import feh.tec.visual.api._
import feh.tec.visual.{PauseScene, NicolLike2DEasel}
import nicol._
import Map._
import feh.tec.util._
import feh.tec.agentes.tarea1.Criteria.{DistanceToClosestPlugAndHoleCriterion, NumberOfHolesCriterion, PlugsMovingAgentCriteria}
import scala.{math, Predef, Some}
import scala.concurrent.{ExecutionContext, Await}
import java.awt.Color
import scala.collection.mutable
import feh.tec.agent.AgentDecision.{FailsafeDecisionStrategy, DecisionStrategy}
import feh.tec.agent.StatelessAgentPerformanceMeasure.Criterion
import feh.tec.agentes.tarea1.Tarea1.Agents.ConditionalExec
import feh.tec.agent.AgentDecision.ExtendedCriteriaBasedDecision
import feh.tec.agentes.tarea1.Tarea1.Agents.ExecLoopBuilders.PauseBetweenExecs
import feh.tec.agent.AgentId
import feh.tec.visual.api.BasicStringDrawOps
import feh.tec.map.visual.WorldVisualisationCalls

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
                      (implicit val actorSystem: ActorSystem)
      extends AgentInfiniteExecution[Position, EnvState, EnvGlobal, Action, Env, MyDummyAgent[InfExec]]

    case class ConditionalExec(agent: MyDummyAgent[ConditionalExec],
                               pauseBetweenExecs: FiniteDuration,
                               execControlTimeout: FiniteDuration,
                               stopConditions: Set[ConditionalExec#StopCondition],
                               onFinished: () => Unit)
                              (implicit val actorSystem: ActorSystem)
      extends AgentExecutionEnvironmentStopCondition[Position, EnvState, EnvGlobal, Action, Env, MyDummyAgent[ConditionalExec]]
    {
      def notifyFinished(): Unit = onFinished()
    }

    case class InfExecBuilder(pauseBetweenExecs: FiniteDuration,
                              stopTimeout: FiniteDuration)
                             (implicit val actorSystem: ActorSystem)
      extends ExecLoopBuilder[AbstractAgent[InfExec], InfExec]
    {
      def buildExec(ag: AbstractAgent[InfExec]): InfExec =
        InfExec(ag.asInstanceOf[MyDummyAgent[InfExec]], pauseBetweenExecs, stopTimeout)
    }

    case class ConditionalExecBuilder(pauseBetweenExecs: FiniteDuration,
                                      execControlTimeout: FiniteDuration,
                                      stopConditions: Set[ConditionalExec#StopCondition],
                                      onFinished: () => Unit)
                                     (implicit val actorSystem: ActorSystem) extends ExecLoopBuilder[AbstractAgent[ConditionalExec], ConditionalExec]{
      def buildExec(ag: AbstractAgent[ConditionalExec]): ConditionalExec =
        ConditionalExec(ag.asInstanceOf[MyDummyAgent[ConditionalExec]], pauseBetweenExecs, execControlTimeout, stopConditions, onFinished)
    }

    object ExecLoopBuilders{
      val execControlTimeout = 1 second
      val stopEnvCondition: ConditionalExec#StopCondition = _ match{
        case (_, states) => !states.exists(_.hole) || !states.exists(_.plug)
      }

      case class PauseBetweenExecs(dur: FiniteDuration)
      
      implicit def infinite(implicit pause: PauseBetweenExecs, actorSystem: ActorSystem): ExecLoopBuilder[AbstractAgent[InfExec], InfExec] = InfExecBuilder(pause.dur, execControlTimeout)
      implicit def environmentCondition(implicit pause: PauseBetweenExecs, actorSystem: ActorSystem): ExecLoopBuilder[AbstractAgent[ConditionalExec], ConditionalExec] =
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
                       override val id: AgentId,
                       val foreseeingDepth: Int,
                       worldVisualisationCalls: WorldVisualisationCalls[Tile, Position])
                      (implicit val actorSystem: ActorSystem, exBuilder: ExecLoopBuilder[AbstractAgent[Exec], Exec])
      extends AbstractAgent[Exec](e, criteria, Environment.mapStateBuilder, shortestRouteFinder, Measure)
        with IdealForeseeingDummyAgent[Position, EnvState, EnvGlobal, Action, Env, Exec, Measure]
        with GlobalDebugging
    {
      agent =>

      def possibleBehaviors(currentPerception: Perception): Set[Action] = {
        currentPerception.debugLog("searching for possible behaviors. position = " + _.position)
        findPossibleActions(this)(currentPerception)
      }

      def perceiveFromSnapshot(sn: EnvironmentSnapshot[Position, EnvState, EnvGlobal, Action, Env]): Perception =
        sense(sn.asInstanceOf[WorldEnvironmentSnapshot[Map, Tile, Position, EnvState, EnvGlobal, Action, Env]]) // todo: casting

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

      private class BackupMeasureBasedForeseeingDecisionStrategy
        extends IdealForeseeingAgentDecisionStrategies.MeasureBasedForeseeingDecisionStrategy[Position, EnvState, EnvGlobal, Action, Env, Exec, Measure, agent.type](foreseeingDepth, debug, notifyRouteChosen)
      {
        override lazy val rewriteCriteria: Option[Measure#Criteria] = Some(backupCriteria)
      }

      def criteriaFailed: ExtendedCriteriaBasedDecision[ActionExplanation, Position, EnvState, EnvGlobal, Action, Env, Exec, Measure] => Boolean =
        _.consideredOptionsCriteriaValues |> {
          ops =>
            ops.length != 1 && ops.distinct.length == 1
        }

      override protected def createBehaviorSelectionStrategy: DecisionStrategy[Action, DecisionArg, ExtendedCriteriaBasedDecision[ActionExplanation, Position, EnvState, EnvGlobal, Action, Env, Exec, Measure]] =
        {
          val strategy = super.createBehaviorSelectionStrategy
          FailsafeDecisionStrategy.Builder(strategy)
            .append(
            criteriaFailed,
              new BackupMeasureBasedForeseeingDecisionStrategy
            )
            .build()
        }

      protected def agentPredictedPosition(p: Env#Prediction) = p.asEnv.agentsPositions(id).coordinate

      private lazy val (xRange, yRange) = env.worldSnapshot.asWorld.coordinates |> {
        c => c.xRange -> c.yRange
      }

      private def isNeighbour(c1: Position, c2: Position) = c1 -> c2 match{
        case ((x1, y1), (x2, y2)) =>
          val diffX = math.abs(x1 - x2)
          val diffY = math.abs(y1 - y2)

          ((diffX == 1 || diffX == xRange.length - 1) && diffY == 0) ||
          ((diffY == 1 || diffY == yRange.length - 1) && diffX == 0)
      }

      protected def notifyRouteChosen: (Option[Seq[Action]]) => Unit = {
        def agentPos = e.blocking.agentPosition(id).get
        val pseudoMap = e.worldSnapshot.asWorld
        def positionTo(from: (Int, Int), direction: SimpleDirection) = pseudoMap.positionTo(from, direction)
        def positions(a: Seq[Action]) = Y[(Seq[Action], Position), Seq[Position]](
          rec => {
            case (Nil, _) => Nil
            case (h :: t, pos) =>
              val np = positionTo(pos, Move.direction(h))
              np +: rec(t -> np)
          }
        )(a -> agentPos)
        _
          .map(acts => NonEmptyCoordinatesChain[Position](positions(acts))(isNeighbour)).getOrElse(new EmptyCoordinatesChain[Position])
          .pipe(seq => Option(worldVisualisationCalls).foreach(_.drawRoute(seq)))
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
          case (Some(Hole()), Some(Plug())) => env.transformAtom(currPosition)(_.copy(contents = None)) // hole plugged
          case (Some(AgentAvatar(_)), _) if !firstPass => env // if there is only one agent on map, it would mean that we've just moved a whole row/column of plugs
          case  _ =>
            env.transformAtom(currPosition)(_.copy(contents = moved))
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

    val mapStateBuilder: WorldStateBuilder[Agent.Position, Agent.Tile, Map, Agent.EnvState] = new MStateBuilder(Agents.Id.dummy) // todo: id
  }

  val timeouts = OverseerTimeouts(
    defaultBlockingTimeout = 10,
    defaultFutureTimeout = 10,
    predictMaxDelay =  100 millis,
    foreseeMaxDelay = 100 millis,
    getMapMaxDelay =  10 millis,
    positionMaxDelay =  30 millis)

  def overseer(env: Environment,
               timeouts: OverseerTimeouts)
              (implicit actorSystem: ActorSystem) =
    new Overseer(actorSystem, env, Environment.mapStateBuilder, timeouts)

  def environment(ag: Option[AgentId]): Environment = environment(ag, Maps.randomMap(Environment.xRange, Environment.yRange, ag))

  def environment(ag: Option[AgentId], map: Map): Environment = new Environment(
    map,
    Environment.effects,
    Environment.initGlobal,
    Environment.mapStateBuilder
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


object Tarea1App {
  val CriteriaDebug = false

  import Tarea1._
  import Agent._

  implicit val actorSystem = ActorSystem()



  val setup: Tarea1AppSetup = new Tarea1AppSetup {
    def findPossibleActions[Exec <: ActorAgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env, MyDummyAgent[Exec]]]
      (ag: MyDummyAgent[Exec], perc: MyDummyAgent[Exec]#Perception): Set[Action] =
        perc.worldSnapshot.getSnapshot(perc.position).neighboursSnapshots
          .filterNot(_.asAtom.contents.exists(_.isHole))
          .map(tile => perc.worldSnapshot.asWorld.relativeNeighboursPosition(tile.coordinate, perc.position))
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

      def getHolesCoordinates(s: Measure.Snapshot) = s.asEnv.worldSnapshot.tilesSnapshots.withFilter(_.asAtom.exists(_.isHole)).map(_.coordinate).toSet

      def minDists(s: Measure.Snapshot) = {
        val holesCoords = getHolesCoordinates(s)
        minDistsMapCache.getOrElse(holesCoords, {
          val t1 = Calendar.getInstance.getTimeInMillis
          val gr = Graph.mapAsGraph(s.asEnv.worldSnapshot)
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
        minDists(sn).withFilter{case ((c1, c2), d) => c1 == relativelyTo && cond(sn.asEnv.worldSnapshot.getSnapshot(c2))}.map(p => sn.asEnv.worldSnapshot.getSnapshot(p._1._2)).toSeq
      def findClosetDisregardingHoles(relativelyTo: Position, cond: Tile#Snapshot => Boolean, sn: Measure.Snapshot): Seq[Tile#Snapshot] =
        sn.asEnv.worldSnapshot.tilesSnapshots.filter(cond).filterMin(distanceDisregardingHoles(relativelyTo, _, sn)) // might be slow; can search recursively, increasing search distance

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
    val tileSideSize = 50
    val showLabels = false

    implicit val easel = new NicolLike2DEasel
    val mapDrawConfig = BasicSquareMapDrawOptions(visual.tileSideSize, showLabels, Color.yellow)
  }

//  Tarea1.Debug() = true

//  val env = environment(Option(Agents.Id.dummy), Maps.failExample1(Agents.Id.dummy))
  lazy val env = environment(Option(Agents.Id.dummy))
//  val env = TestEnvironment.test1(Option(Agents.Id.dummy))
  lazy val overseer = Tarea1.overseer(env, timeouts)

  implicit def agResolver = new AgentResolver{
    def byId(id: AgentId): Option[AgentRef] = PartialFunction.condOpt(id){
      case Agents.Id.dummy => AgentRef.create(ag)
    }
  }

  lazy val app = new NicolBasedTarea1AgentApp(env, Agents.Id.dummy)


  val foreseeingDepth = 5

  import visual.easel
  import Tarea1.Agents.ExecLoopBuilders._

  var argsForTimeSpan: Option[FiniteDuration] = None
  implicit val pauseBetweenExecs = PauseBetweenExecs(argsForTimeSpan getOrElse defaultPauseBetweenExecs)

  type Exec = ConditionalExec
  lazy val ag: MyDummyAgent[Exec] = new MyDummyAgent[Exec](
    overseer.ref,
    setup.criteria,
    setup.backupCriteria,
    (setup.findPossibleActions[Exec] _).curried,
    Agents.Id.dummy,
    foreseeingDepth,
    app.mapRenderer)

  lazy val finishedScene = new FinishedScene(renderMap.lifted)

  def renderMap(implicit easel: NicolLike2DEasel) = app.mapRenderer.render(env)

  val agStop = ag.execution()

  def terminate() = {
    Await.ready(agStop(), 1 second)
    actorSystem.stop(ag.actorRef)
    actorSystem.stop(overseer.actorRef)
    app.stop
  }

  def setFinishedScene() = Finished.flag = true
  def isFinished = Finished.flag

  implicit def execContext: ExecutionContext = ExecutionContext.global

  def start() = {
    app.start()
//    val f = overseer.ref.async.visibleStates
//    println("!f + " +  f)
//    f.map{
//      case x => println("visibleStates: " + x)
//    }
  }
}

object Tarea1Application extends App{
  def processArgsForTimeSpan(): Option[FiniteDuration] =
    if(args.nonEmpty) {
      val dur = Duration(args.mkString(" ")).ensuring(_.isFinite())
      Some(FiniteDuration(dur.length, dur.unit))
    }
    else None

  Tarea1App.argsForTimeSpan = processArgsForTimeSpan()
  Tarea1App.start()
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
  onPause = render,
  onResume = Tarea1App.ag.executionLoop.resume().lifted,
  endScene = Tarea1EndScene.lifted,
  resumeScene = Tarea1LastSceneKeeper.scene.lifted,
  pausedMessage = "Agent Execution Paused" -> BasicStringDrawOps[NicolLike2DEasel](StringAlignment.Left, Color.lightGray, "Arial", 20F, 5)
){
  override def messagePosition = (530, 300)
 }

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