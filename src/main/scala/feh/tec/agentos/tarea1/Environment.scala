package feh.tec.agentos.tarea1

import feh.tec.map._
import feh.tec.agent._
import scala.reflect.runtime.universe._
import akka.actor.{ActorSystem, Scheduler}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration
import scala.collection
import scala.collection.immutable
import feh.tec.visiual.AsyncMapDrawingEnvironmentOverseer
import feh.tec.visual.NicolLike2DEasel
import feh.tec.visual.api.MapRenderer

object Environment{
  type Tile = SqTile
  type Coordinate = Map#Coordinate
  type State = MapState[Coordinate, Tile, Map]
  type Global = NoGlobal
  type Action = Move
  type Easel = NicolLike2DEasel
}

import Environment._

class Environment(buildTilesMap: Map => Seq[Tile],
                  val xRange: Range,
                  val yRange: Range,
                  val effects: PartialFunction[Action, Environment => Environment],
                  val initGlobalState: Global,
                  mapStateBuilder: MapStateBuilder[Coordinate, Tile, Map, State])
  extends Map(null, xRange, yRange)
  with MutableMapEnvironment[Map, Tile, Coordinate, State, Global, Action, Environment]
  with FullyAccessible[Coordinate, State, Global, Action, Environment]
  with Deterministic[Coordinate, State, Global, Action, Environment]
  with PredictableDeterministicEnvironment[Coordinate, State, Global, Action, Environment]
  with Static[Coordinate, State, Global, Action, Environment]
{

  type Ref = PredictableEnvironmentRef[Coordinate, State, Global, Action, Environment] with MapEnvironmentRef[Coordinate, State, Global, Action, Environment, Tile, Map]
  lazy val definedAt: Seq[Coordinate] = xRange zip yRange

  lazy val tags = new TypeTags{
    implicit def coordinate: TypeTag[Coordinate] = typeTag[Coordinate]
    implicit def state: TypeTag[State] = typeTag[State]
    implicit def global: TypeTag[Global] = typeTag[Global]
    implicit def action: TypeTag[Action] = typeTag[Action]
    implicit def environment: TypeTag[Environment] = typeTag[Environment]
  }

  val initStates: PartialFunction[Coordinate, State] = tilesMap.mapValues(mapStateBuilder.build)

  def initTiles: Seq[Tile] = buildTilesMap(this)

  protected def agentInTile(tile: Tile): Option[AgentId] = tile.contents.collect{
    case AgentAvatar(agentId) => agentId
  }

  override lazy val tilesMap = asMap

  override def tilesAsMap = super[MutableMapEnvironment].tilesAsMap
  override def tiles: Seq[Tile] = super[MutableMapEnvironment].tiles
  override def agentsPositions: collection.Map[AgentId, Tile] = super[MutableMapEnvironment].agentsPositions
}

class Overseer(actorSystem: ActorSystem,
               initEnvironment: Environment,
               val mapRenderer: MapRenderer[Map, Tile, Coordinate, Easel],
               implicit val easel: Easel,
               val mapDrawConfig: Easel#MDrawOptions,
               val mapStateBuilder: MapStateBuilder[Coordinate, Tile, Map, State],
               timeouts: OverseerTimeouts)
  extends EnvironmentOverseerActor[Coordinate, State, Global, Action, Environment]
  with MutableEnvironmentOverseer[Coordinate, State, Global, Action, Environment]
  with PredictingMutableDeterministicEnvironmentOverseer[Coordinate, State, Global, Action, Environment]
  with PredictingEnvironmentOverseerActor[Coordinate, State, Global, Action, Environment]
  with MapEnvironmentOverseerActor[Map, Tile, Coordinate, State, Global, Action, Environment]
  with AsyncMapDrawingEnvironmentOverseer[Map, Tile, Coordinate, State, Global, Action, Environment, Easel]
{
  
  overseer =>
  
  protected def externalExecutionContext: ExecutionContext = actorSystem.dispatcher
  protected def scheduler: Scheduler = actorSystem.scheduler

  override val currentEnvironment: Environment = initEnvironment


  def snapshot: EnvironmentSnapshot[Coordinate, State, Global, Action, Environment] = SnapshotBuilder.snapshot()

  protected lazy val SnapshotBuilder = new SnapshotBuilder(env)

  class SnapshotBuilder(env: Environment){
    def snapshot(_states: PartialFunction[Coordinate, State] = env.states,
                 _globalState: Global = env.globalState,
                 _tilesMap: collection.Map[(Int, Int), SqTile] = env.tilesAsMap ): EnvironmentSnapshot[Coordinate, State, Global, Action, Environment] =
      new Environment(null, env.xRange, env.yRange, env.effects, _globalState, mapStateBuilder)
        with EnvironmentSnapshot[Coordinate, State, Global, Action, Environment]
      {
        override val states = _states
        override def states_=(pf: PartialFunction[Coordinate, State]) {}
        override val globalState = _globalState
        override def globalState_=(g: Global) {}
        override lazy val tilesMap = _tilesMap
        override def affected(act: Action) = super[EnvironmentSnapshot].affected(act)
      }
  }

  /**
   * a snapshot of mutable environment that have setter functions active and has
   */
  def mutableSnapshot(): CustomisableEnvironmentSnapshot[Coordinate, State, Global, Action, Environment] with Environment =
    new Environment(null, env.xRange, env.yRange, env.effects, env.globalState, mapStateBuilder)
      with CustomisableEnvironmentSnapshot[Coordinate, State, Global, Action, Environment]
    {
      lazy val SnapshotBuilder = new SnapshotBuilder(this)

      def snapshot(): EnvironmentSnapshot[Coordinate, State, Global, Action, Environment] = SnapshotBuilder.snapshot()
    }

  lazy val mapSnapshotBuilder = new Map.SnapshotBuilder

  def getMap(): MapSnapshot[Map, Tile, Coordinate] = mapSnapshotBuilder.snapshot(env)

  def getMap(s: EnvironmentSnapshot[Coordinate, State, Global, Action, Environment]): MapSnapshot[Map, Tile, Coordinate] =
    mapSnapshotBuilder.snapshot(s.asInstanceOf[Environment with EnvironmentSnapshot[Coordinate, State, Global, Action, Environment]])


  def position(a: feh.tec.agent.AbstractAgent[Coordinate, State, Global, Action, Environment] with InAbstractMapEnvironment[Coordinate, State, Global, Action, Environment, Tile, Map]): Coordinate =
    env.agentsPositions(a.id).coordinate


  def ref: Environment#Ref = new BaseEnvironmentRef with PredictableEnvironmentRefImpl with MapEnvironmentRefImpl{}

  def defaultBlockingTimeout: Int = timeouts.defaultBlockingTimeout
  def defaultFutureTimeout: Int = timeouts.defaultFutureTimeout
  def getMapMaxDelay: FiniteDuration = timeouts.getMapMaxDelay
  def positionMaxDelay: FiniteDuration = timeouts.positionMaxDelay
  def predictMaxDelay: FiniteDuration = timeouts.predictMaxDelay
}

trait Move extends MapAction[Coordinate, Tile, Map]
case object MoveNorth extends Move
case object MoveSouth extends Move
case object MoveWest extends Move
case object MoveEast extends Move

trait NoGlobal extends MapGlobalState[Coordinate, Tile, Map]
case object NoGlobal extends NoGlobal

case class OverseerTimeouts(defaultBlockingTimeout: Int,
                            defaultFutureTimeout: Int,
                            predictMaxDelay: FiniteDuration,
                            getMapMaxDelay: FiniteDuration,
                            positionMaxDelay: FiniteDuration)