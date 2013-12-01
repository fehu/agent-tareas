package feh.tec.agentes.tarea1

import feh.tec.world._
import feh.tec.agent._
import akka.actor._
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import feh.tec.visual.NicolLike2DEasel
import feh.tec.visual.api.WorldRenderer
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
                  mapStateBuilder: WorldStateBuilder[Coordinate, Tile, Map, State])
  extends Map(xRange, yRange, null)
  with MutableWorldEnvironment[Map, Tile, Coordinate, State, Global, Action, Environment]
  with FullyAccessible[Coordinate, State, Global, Action, Environment]
  with Deterministic[Coordinate, State, Global, Action, Environment]
  with PredictableDeterministicEnvironment[Coordinate, State, Global, Action, Environment]
  with ForeseeableEnvironment[Coordinate, State, Global, Action, Environment]
  with Static[Coordinate, State, Global, Action, Environment]
{
  def this(map: Map,
           effects: PartialFunction[Action, Environment => Environment],
           initGlobalState: Global,
           mapStateBuilder: WorldStateBuilder[Coordinate, Tile, Map, State]) =
    this(_ => map.atoms, map.coordinates.xRange, map.coordinates.yRange, effects, initGlobalState, mapStateBuilder)

  type Ref = ForeseeableEnvironmentRef[Coordinate, State, Global, Action, Environment] with WorldEnvironmentRef[Coordinate, State, Global, Action, Environment, Tile, Map]
  type Snapshot = EnvironmentSnapshot[Coordinate, State, Global, Action, Environment]

  lazy val definedAt: Seq[Coordinate] = xRange.flatMap(x => yRange.map(x ->))

  assertDefinedAtAllCoordinates()

  def initStates: PartialFunction[Coordinate, State] = tilesInitMap.mapValues(mapStateBuilder.build)

  def initAtoms: Seq[Tile] = buildTilesMap(this)

  def stateByAtom(tile: Tile): State = mapStateBuilder.build(tile)

  protected def agentAtAtom(tile: Tile): Option[AgentId] = tile.contents.collect{
    case AgentAvatar(agentId) => agentId
  }

  override lazy val tilesInitMap = initAtoms.map(t => t.coordinate -> t).toMap
  override def get: PartialFunction[Coordinate, Tile] = super[MutableWorldEnvironment].get

  override def tilesToMap: Predef.Map[(Int, Int), SqTile] = atomsMap

  override def atoms: Seq[Tile] = super[MutableWorldEnvironment].atoms
  override def agentsPositions: Predef.Map[AgentId, Tile] = super[MutableWorldEnvironment].agentsPositions

  def worldSnapshot: WorldSnapshot[Map, Tile, Coordinate] = Map.snapshotBuilder.snapshot(this)
}

class Overseer(actorSystem: ActorSystem,
               initEnvironment: Environment,
               val mapStateBuilder: WorldStateBuilder[Coordinate, Tile, Map, State],
               timeouts: OverseerTimeouts)
  extends EnvironmentOverseerWithActor[Coordinate, State, Global, Action, Environment]
  with MutableEnvironmentOverseer[Coordinate, State, Global, Action, Environment]
  with PredictingMutableDeterministicEnvironmentOverseer[Coordinate, State, Global, Action, Environment]
  with ForeseeingMutableDeterministicEnvironmentOverseer[Coordinate, State, Global, Action, Environment]
  with PredictingEnvironmentOverseerWithActor[Coordinate, State, Global, Action, Environment]
  with ForeseeingEnvironmentOverseerWithActor[Coordinate, State, Global, Action, Environment]
  with WorldEnvironmentOverseerWithActor[Map, Tile, Coordinate, State, Global, Action, Environment]
  with GlobalDebugging
{
  
  overseer =>

  def terminate() = actorSystem.stop(actorRef)

  protected def executionContext: ExecutionContext = actorSystem.dispatcher
  protected def scheduler: Scheduler = actorSystem.scheduler

  override val currentEnvironment: Environment = initEnvironment


  def snapshot: EnvironmentSnapshot[Coordinate, State, Global, Action, Environment] = SnapshotBuilder.snapshot()

  protected lazy val SnapshotBuilder = new SnapshotBuilder(env)

  class SnapshotBuilder(env: Environment){
    def snapshot(_states: Predef.Map[Coordinate, State] = env.statesMap,
                 _globalState: Global = env.globalState,
                 _tilesMap: Predef.Map[(Int, Int), SqTile] = env.atomsMap ): EnvironmentSnapshot[Coordinate, State, Global, Action, Environment] =
      new Environment(null, env.xRange, env.yRange, env.effects, _globalState, mapStateBuilder)
        with EnvironmentSnapshot[Coordinate, State, Global, Action, Environment]
        with WorldEnvironmentSnapshot[Map, Tile, Coordinate, State, Global, Action, Environment]
      {

        override val worldSnapshot: WorldSnapshot[Map, Tile, Coordinate] = Map.snapshotBuilder.snapshot(env)

        override def initAtoms: Seq[Tile] = _tilesMap.values.toSeq
        override val states: Predef.Map[Environment.Coordinate, Environment.State] = _states
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
    buildMutableSnapshot(env.xRange, env.yRange, env.effects, env.globalState, env.atoms)


  protected def buildMutableSnapshot(xRange: Range,
                                     yRange: Range,
                                     effects: PartialFunction[Action, Environment => Environment],
                                     globalState: Global,
                                     _initTiles: Seq[Tile]): CustomisableEnvironmentSnapshot[Coordinate, State, Global, Action, Environment] with Environment =
    new Environment(null, xRange, yRange, effects, globalState, mapStateBuilder)
      with CustomisableEnvironmentSnapshot[Coordinate, State, Global, Action, Environment]
    {
      lazy val SnapshotBuilder = new SnapshotBuilder(this)

      override def initAtoms: Seq[Tile] = _initTiles

      def snapshot(): EnvironmentSnapshot[Coordinate, State, Global, Action, Environment] = SnapshotBuilder.snapshot()

      def copy(): CustomisableEnvironmentSnapshot[Coordinate, State, Global, Action, Environment] with Environment =
        buildMutableSnapshot(this.xRange, this.yRange, this.effects, this.globalState, this.atoms)
    }

  lazy val mapSnapshotBuilder = Map.snapshotBuilder

  def worldSnapshot(): WorldSnapshot[Map, Tile, Coordinate] = mapSnapshotBuilder.snapshot(env)

  def worldSnapshot(s: EnvironmentSnapshot[Coordinate, State, Global, Action, Environment]): WorldSnapshot[Map, Tile, Coordinate] =
    mapSnapshotBuilder.snapshot(s.asInstanceOf[Environment with EnvironmentSnapshot[Coordinate, State, Global, Action, Environment]])


  def position(a: feh.tec.agent.AbstractAgent[Coordinate, State, Global, Action, Environment] with InAbstractWorldEnvironment[Coordinate, State, Global, Action, Environment, Tile, Map]): Coordinate =
    position(a.id).get
  def position(id: AgentId): Option[Coordinate] = env.agentsPositions.get(id).map(_.coordinate)

  def actorResponseFuncs = baseActorResponses :: predictingActorResponses :: foreseeingActorResponses :: worldActorResponses :: Nil
  def ref: Environment#Ref = new BaseEnvironmentRef with PredictableEnvironmentRefImpl
    with ForeseeableEnvironmentRefImpl with WorldEnvironmentRefImpl{}

  def defaultBlockingTimeout: Int = timeouts.defaultBlockingTimeout
  def defaultFutureTimeout: Int = timeouts.defaultFutureTimeout
  def getWorldMaxDelay: FiniteDuration = timeouts.getMapMaxDelay
  def positionMaxDelay: FiniteDuration = timeouts.positionMaxDelay
  def predictMaxDelay: FiniteDuration = timeouts.predictMaxDelay
  def foreseeMaxDelay: FiniteDuration = timeouts.foreseeMaxDelay

  protected def environmentOverseerActorProps = Props(classOf[EnvironmentOverseerActor], actorResponseFuncs)

  lazy val actorRef: ActorRef = actorSystem.actorOf(environmentOverseerActorProps)

  def debugMessagePrefix: String = "[Overseer]"

  override def predict(a: Action): Environment#Prediction = {
    a debugLog "Predicting for action"
    super.predict(a)
  }

  protected def setup: DebuggingSetup = Tarea1.Debug
}

class EnvironmentOverseerActor(responses: PartialFunction[Any, () => Any]) extends Actor{
  val log = Logging(context.system, this)

  private def scheduler = context.system.scheduler
  import context.dispatcher

  def receive: Actor.Receive = responses andThen { // todo ??
    response => scheduler.scheduleOnce(0 millis)({
      response()
    } match {
      case Unit =>
      case msg => sender ! msg
    })
  }
}


trait Move extends WorldAction[Coordinate, Tile, Map]
case object MoveNorth extends Move
case object MoveSouth extends Move
case object MoveWest extends Move
case object MoveEast extends Move

object Move{
  def apply(direction: SimpleDirection): Move = move(direction)
  def apply(move: Move): SimpleDirection = direction(move)

  def move: PartialFunction[SimpleDirection, Move] = {
    case Simple2dDirection.Up => MoveNorth
    case Simple2dDirection.Down => MoveSouth
    case Simple2dDirection.Left => MoveWest
    case Simple2dDirection.Right => MoveEast
  }

  def direction: PartialFunction[Move, SimpleDirection] = {
    case MoveNorth => Simple2dDirection.Up
    case MoveSouth => Simple2dDirection.Down
    case MoveWest => Simple2dDirection.Left
    case MoveEast => Simple2dDirection.Right
  }

  def all = Set(MoveNorth, MoveSouth, MoveEast, MoveWest)
}

trait NoGlobal extends WorldState[Coordinate, Tile, Map]
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
                  plug: Boolean = false) extends AtomState[Coordinate, Tile, Map]{
  def empty = !(self || otherAgent || hole || plug)
}

class MStateBuilder(selfId: AgentId) extends WorldStateBuilder[Coordinate, Tile, Map,MState]{
  def build(tile: Tile): MState = tile.contents match {
    case Some(AgentAvatar(id@AgentId(_))) if selfId == id => MState(self = true)
    case Some(AgentAvatar(_)) => MState(otherAgent = true)
    case Some(Plug()) => MState(plug = true)
    case Some(Hole()) => MState(hole = true)
    case None => MState()
  }
  def build(snapshot: AtomSnapshot[Tile, Coordinate]): MState = build(snapshot: Tile)
}