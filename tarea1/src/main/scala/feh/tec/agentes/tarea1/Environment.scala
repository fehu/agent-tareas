package feh.tec.agentes.tarea1

import feh.tec.map._
import feh.tec.agent._
import scala.reflect.runtime.universe._
import akka.actor._
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.collection
import feh.tec.visiual.AsyncMapDrawingEnvironmentOverseer
import feh.tec.visual.NicolLike2DEasel
import feh.tec.visual.api.MapRenderer
import scala.Some
import feh.tec.agent.AgentId
import akka.event.Logging
import feh.tec.util.{DebuggingSetup, GlobalDebugging, Debugging}

object Environment{
  type Tile = SqTile
  type Coordinate = Map#Coordinate
  type State = MState
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
  with ForeseeableEnvironment[Coordinate, State, Global, Action, Environment]
  with Static[Coordinate, State, Global, Action, Environment]
{

  type Ref = ForeseeableEnvironmentRef[Coordinate, State, Global, Action, Environment] with MapEnvironmentRef[Coordinate, State, Global, Action, Environment, Tile, Map]
  type Snapshot = EnvironmentSnapshot[Coordinate, State, Global, Action, Environment]

  lazy val definedAt: Seq[Coordinate] = xRange.flatMap(x => yRange.map(x ->))

  lazy val tags = new TypeTags{
    implicit def coordinate: TypeTag[Coordinate] = typeTag[Coordinate]
    implicit def state: TypeTag[State] = typeTag[State]
    implicit def global: TypeTag[Global] = typeTag[Global]
    implicit def action: TypeTag[Action] = typeTag[Action]
    implicit def environment: TypeTag[Environment] = typeTag[Environment]
  }

  assertDefinedAtAllCoordinates()

  def initStates: PartialFunction[Coordinate, State] = tilesInitMap.mapValues(mapStateBuilder.build)

  def initTiles: Seq[Tile] = buildTilesMap(this)

  protected def agentInTile(tile: Tile): Option[AgentId] = tile.contents.collect{
    case AgentAvatar(agentId) => agentId
  }

  override lazy val tilesInitMap = initTiles.map(t => t.coordinate -> t).toMap
  override def get: PartialFunction[Coordinate, Tile] = super[MutableMapEnvironment].get

  override def tilesToMap: Predef.Map[(Int, Int), SqTile] = tilesAsMap

  override def tiles: Seq[Tile] = super[MutableMapEnvironment].tiles
  override def agentsPositions: Predef.Map[AgentId, Tile] = super[MutableMapEnvironment].agentsPositions
}

class Overseer(actorSystem: ActorSystem,
               initEnvironment: Environment,
               val mapRenderer: MapRenderer[Map, Tile, Coordinate, Easel],
               implicit val easel: Easel,
               val mapDrawConfig: Easel#MDrawOptions,
               val mapStateBuilder: MapStateBuilder[Coordinate, Tile, Map, State],
               timeouts: OverseerTimeouts)
  extends EnvironmentOverseerWithActor[Coordinate, State, Global, Action, Environment]
  with MutableEnvironmentOverseer[Coordinate, State, Global, Action, Environment]
  with PredictingMutableDeterministicEnvironmentOverseer[Coordinate, State, Global, Action, Environment]
  with ForeseeingMutableDeterministicEnvironmentOverseer[Coordinate, State, Global, Action, Environment]
  with PredictingEnvironmentOverseerWithActor[Coordinate, State, Global, Action, Environment]
  with ForeseeingEnvironmentOverseerWithActor[Coordinate, State, Global, Action, Environment]
  with MapEnvironmentOverseerWithActor[Map, Tile, Coordinate, State, Global, Action, Environment]
  with AsyncMapDrawingEnvironmentOverseer[Map, Tile, Coordinate, State, Global, Action, Environment, Easel]
  with GlobalDebugging
{
  
  overseer =>
  
  protected def executionContext: ExecutionContext = actorSystem.dispatcher
  protected def scheduler: Scheduler = actorSystem.scheduler

  override val currentEnvironment: Environment = initEnvironment


  def snapshot: EnvironmentSnapshot[Coordinate, State, Global, Action, Environment] = SnapshotBuilder.snapshot()

  protected lazy val SnapshotBuilder = new SnapshotBuilder(env)

  class SnapshotBuilder(env: Environment){
    def snapshot(_states: PartialFunction[Coordinate, State] = env.states,
                 _globalState: Global = env.globalState,
                 _tilesMap: Predef.Map[(Int, Int), SqTile] = env.tilesAsMap ): EnvironmentSnapshot[Coordinate, State, Global, Action, Environment] =
      new Environment(null, env.xRange, env.yRange, env.effects, _globalState, mapStateBuilder)
        with EnvironmentSnapshot[Coordinate, State, Global, Action, Environment]
        with MapEnvironmentSnapshot[Map, Tile, Coordinate, State, Global, Action, Environment]
      {

        val mapSnapshot: MapSnapshot[Map, Tile, Coordinate] = Map.snapshotBuilder.snapshot(env)

        override def initTiles: Seq[Tile] = _tilesMap.values.toSeq
        override val states = _states
        override def states_=(pf: PartialFunction[Coordinate, State]) {}
        override val globalState = _globalState
        override def globalState_=(g: Global) {}
        override lazy val tilesInitMap = _tilesMap.toMap
        override def affected(act: Action) = super[EnvironmentSnapshot].affected(act)
      }
  }

  /**
   * a snapshot of mutable environment that have setter functions active and has
   */
  def mutableSnapshot(): CustomisableEnvironmentSnapshot[Coordinate, State, Global, Action, Environment] with Environment =
    buildMutableSnapshot(env.xRange, env.yRange, env.effects, env.globalState, env.tiles)


  protected def buildMutableSnapshot(xRange: Range,
                                     yRange: Range,
                                     effects: PartialFunction[Action, Environment => Environment],
                                     globalState: Global,
                                     _initTiles: Seq[Tile]): CustomisableEnvironmentSnapshot[Coordinate, State, Global, Action, Environment] with Environment =
    new Environment(null, xRange, yRange, effects, globalState, mapStateBuilder)
      with CustomisableEnvironmentSnapshot[Coordinate, State, Global, Action, Environment]
    {
      lazy val SnapshotBuilder = new SnapshotBuilder(this)

      override def initTiles: Seq[Tile] = _initTiles

      def snapshot(): EnvironmentSnapshot[Coordinate, State, Global, Action, Environment] = SnapshotBuilder.snapshot()

      def copy(): CustomisableEnvironmentSnapshot[Coordinate, State, Global, Action, Environment] with Environment =
        buildMutableSnapshot(this.xRange, this.yRange, this.effects, this.globalState, this.tiles)
    }

  lazy val mapSnapshotBuilder = Map.snapshotBuilder

  def mapSnapshot(): MapSnapshot[Map, Tile, Coordinate] = mapSnapshotBuilder.snapshot(env)

  def mapSnapshot(s: EnvironmentSnapshot[Coordinate, State, Global, Action, Environment]): MapSnapshot[Map, Tile, Coordinate] =
    mapSnapshotBuilder.snapshot(s.asInstanceOf[Environment with EnvironmentSnapshot[Coordinate, State, Global, Action, Environment]])


  def position(a: feh.tec.agent.AbstractAgent[Coordinate, State, Global, Action, Environment] with InAbstractMapEnvironment[Coordinate, State, Global, Action, Environment, Tile, Map]): Coordinate =
    position(a.id).get
  def position(id: AgentId): Option[Coordinate] = env.agentsPositions.get(id).map(_.coordinate)

  def actorResponseFuncs = baseActorResponses :: predictingActorResponses :: foreseeingActorResponses :: mapActorResponses :: Nil
  def actorResponseFunc: PartialFunction[Any, () => Unit] = actorResponseFuncs.reduceLeft(_ orElse _)
  def ref: Environment#Ref = new BaseEnvironmentRef with PredictableEnvironmentRefImpl
    with ForeseeableEnvironmentRefImpl with MapEnvironmentRefImpl{}

  def defaultBlockingTimeout: Int = timeouts.defaultBlockingTimeout
  def defaultFutureTimeout: Int = timeouts.defaultFutureTimeout
  def getMapMaxDelay: FiniteDuration = timeouts.getMapMaxDelay
  def positionMaxDelay: FiniteDuration = timeouts.positionMaxDelay
  def predictMaxDelay: FiniteDuration = timeouts.predictMaxDelay
  def foreseeMaxDelay: FiniteDuration = ???

  protected def environmentOverseerActorProps = Props(classOf[EnvironmentOverseerActor], actorResponseFuncs)

  val actorRef: ActorRef = actorSystem.actorOf(environmentOverseerActorProps)

  def debugMessagePrefix: String = "[Overseer]"

  override def predict(a: Action): Environment#Prediction = {
    a debugLog "Predicting for action"
    super.predict(a)
  }

  protected def setup: DebuggingSetup = Tarea1.Debug
}

class EnvironmentOverseerActor(responses: PartialFunction[Any, () => Unit]) extends Actor{
  val log = Logging(context.system, this)

  def externalExec(f: PartialFunction[Any, () => Unit]): PartialFunction[Any, Unit] =
    f andThen (exec => context.system.scheduler.scheduleOnce(Duration.Zero)(exec())(context.dispatcher))


  def receive: Actor.Receive = externalExec(responses)
//    PartialFunction(externalExec(responses andThen sender.!)) // todo: doesn't seem good
}


trait Move extends MapAction[Coordinate, Tile, Map]
case object MoveNorth extends Move
case object MoveSouth extends Move
case object MoveWest extends Move
case object MoveEast extends Move

object Move{
  def apply(direction: SimpleDirection): Move = move(direction)
  def apply(move: Move): SimpleDirection = direction(move)

  def move: PartialFunction[SimpleDirection, Move] = {
    case SimpleDirection.Up => MoveNorth
    case SimpleDirection.Down => MoveSouth
    case SimpleDirection.Left => MoveWest
    case SimpleDirection.Right => MoveEast
  }

  def direction: PartialFunction[Move, SimpleDirection] = {
    case MoveNorth => SimpleDirection.Up
    case MoveSouth => SimpleDirection.Down
    case MoveWest => SimpleDirection.Left
    case MoveEast => SimpleDirection.Right
  }
}

trait NoGlobal extends MapGlobalState[Coordinate, Tile, Map]
case object NoGlobal extends NoGlobal

case class OverseerTimeouts(defaultBlockingTimeout: Int,
                            defaultFutureTimeout: Int,
                            predictMaxDelay: FiniteDuration,
                            foreseeMaxDelay: FiniteDuration,
                            getMapMaxDelay: FiniteDuration,
                            positionMaxDelay: FiniteDuration)

/**
 *  seems to be not used ...
 */
case class MState(self: Boolean = false,
                  otherAgent: Boolean = false,
                  hole: Boolean = false,
                  plug: Boolean = false) extends MapState[Coordinate, Tile, Map]{
  def empty = !(self || otherAgent || hole || plug)
}

class MStateBuilder(selfId: AgentId) extends MapStateBuilder[Coordinate, Tile, Map,MState]{
  def build(tile: Tile): MState = tile.contents match {
    case Some(AgentAvatar(id@AgentId(_))) if selfId == id => MState(self = true)
    case Some(AgentAvatar(_)) => MState(otherAgent = true)
    case Some(Plug()) => MState(plug = true)
    case Some(Hole()) => MState(hole = true)
    case None => MState()
  }
  def build(snapshot: TileSnapshot[Tile, Coordinate]): MState = build(snapshot: Tile)
}