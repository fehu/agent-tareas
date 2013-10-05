package feh.tec.map

import feh.tec.agent._
import feh.tec.map.tile.AbstractTile
import feh.tec.util.HasUUID
import scala.concurrent.Future
import java.util.UUID
import scala.concurrent.duration.FiniteDuration
import HasUUID._
import scala.collection.mutable
import feh.tec.util._

trait MapEnvironment[Map <: AbstractMap[Tile, Coordinate],
                     Tile <: AbstractTile[Tile, Coordinate],
                     Coordinate,
                     State <: MapState[Coordinate, Tile, Map],
                     Global <: MapGlobalState[Coordinate, Tile, Map],
                     Action <: MapAction[Coordinate, Tile, Map],
                     Env <: MapEnvironment[Map, Tile, Coordinate, State, Global, Action, Env]]
  extends Environment[Coordinate, State, Global, Action, Env] with AbstractMap[Tile, Coordinate]
{
  self: Map =>

  override type Ref <: MapEnvironmentRef[Coordinate, State, Global, Action, Env, Tile, Map]
}

trait MapAction[Coordinate, Tile <: AbstractTile[Tile, Coordinate], Map <: AbstractMap[Tile, Coordinate]] extends AbstractAction

trait MapGlobalState[Coordinate, Tile <: AbstractTile[Tile, Coordinate], Map <: AbstractMap[Tile, Coordinate]]

trait MapState[Coordinate, Tile <: AbstractTile[Tile, Coordinate], Map <: AbstractMap[Tile, Coordinate]]
trait MapStateBuilder[Coordinate, Tile <: AbstractTile[Tile, Coordinate], Map <: AbstractMap[Tile, Coordinate],
                      State <: MapState[Coordinate, Tile, Map]]{
  def build(tile: Tile): State
  def build(snapshot: TileSnapshot[Tile, Coordinate]): State
}

trait MutableMapEnvironment[Map <: AbstractMap[Tile, Coordinate],
                            Tile <: AbstractTile[Tile, Coordinate],
                            Coordinate,
                            State <: MapState[Coordinate, Tile, Map],
                            Global <: MapGlobalState[Coordinate, Tile, Map],
                            Action <: MapAction[Coordinate, Tile, Map],
                            Env <: MutableMapEnvironment[Map, Tile, Coordinate, State, Global, Action, Env] with Map]
//                              with MutableEnvironment[Coordinate, State, Global, Action, Env]]
  extends MapEnvironment[Map, Tile, Coordinate, State, Global, Action, Env] with MutableEnvironment[Coordinate, State, Global, Action, Env]
    with AgentsPositionsProvidingMap[Tile, Coordinate]
{
  self: Env =>

  def initTiles: Seq[Tile]
  private val _tilesMap = mutable.HashMap(zipTilesWithCoords(initTiles): _*)
  private val _agentsPositions = mutable.HashMap(calcAgentsPositions: _*)

  def get: PartialFunction[Coordinate, Tile] = _tilesMap

  private def zipTilesWithCoords(tiles: Seq[Tile]) = tiles.map(t => t.coordinate -> t)
  private def setTiles(tiles: Seq[Tile]) = _tilesMap ++= zipTilesWithCoords(tiles)
  private def setTile(tile: Tile) = _tilesMap += tile.coordinate -> tile

  def tilesAsMap = _tilesMap.toMap

  def tiles: Seq[Tile] = tilesAsMap.values.toSeq

  protected def updateAgentInTile(newTile: Tile): Tile = {
    agentInTile(newTile).foreach{
      _agentsPositions += _ -> newTile
    }
    newTile
  }

  private implicit class TileTransformFuncWrapper(f: Tile => Tile){
    def ensuringSameCoordinates: Tile => Tile = old => f(old).ensuring(_.coordinate == old.coordinate)
  }

  def transformTiles(f: Tile => Tile): Env = {
    setTiles(tiles.map(updateAgentInTile _ compose f.ensuringSameCoordinates))
    this
  }

  def transformTile(pos: Coordinate)(f: Tile => Tile): Env = {
    setTile(get(pos) |> (updateAgentInTile _ compose f.ensuringSameCoordinates))
    this
  }

  private def calcAgentsPositions = tiles.flatMap(t => agentInTile(t).map(_ -> t))

  protected def agentInTile(tile: Tile): Option[AgentId]

  def agentsPositions = _agentsPositions.toMap
}

trait MapEnvironmentRef[Coordinate, State <: MapState[Coordinate, Tile, Map], Global <: MapGlobalState[Coordinate, Tile, Map],
                        Action <: MapAction[Coordinate, Tile, Map], Env <: MapEnvironment[Map, Tile, Coordinate, State, Global, Action, Env],
                        Tile <: AbstractTile[Tile, Coordinate], Map <: AbstractMap[Tile, Coordinate]]
  extends EnvironmentRef[Coordinate, State, Global, Action, Env]
{
  def getMap(e: Env#Ref): MapSnapshot[Map, Tile, Coordinate]
  def getMap(s: EnvironmentSnapshot[Coordinate, State, Global, Action, Env]): MapSnapshot[Map, Tile, Coordinate]
  def position(a: AbstractAgent[Coordinate, State, Global, Action, Env] with InAbstractMapEnvironment[Coordinate, State, Global, Action, Env, Tile, Map]): Coordinate
  def position(id: AgentId): Option[Coordinate]

  def asyncGetMap(e: Env#Ref): Future[MapSnapshot[Map, Tile, Coordinate]]
  def asyncGetMap(s: EnvironmentSnapshot[Coordinate, State, Global, Action, Env]): Future[MapSnapshot[Map, Tile, Coordinate]]
  def asyncPosition(a: AbstractAgent[Coordinate, State, Global, Action, Env] with InAbstractMapEnvironment[Coordinate, State, Global, Action, Env, Tile, Map]): Future[Coordinate]
}

trait InAbstractMapEnvironment[Position,
                               EnvState <: MapState[Position, Tile, Map],
                               EnvGlobal <: MapGlobalState[Position, Tile, Map],
                               Action <: MapAction[Position, Tile, Map],
                               Env <: Environment[Position, EnvState, EnvGlobal, Action, Env] with MapEnvironment[Map, Tile, Position, EnvState, EnvGlobal, Action, Env],
                               Tile <: AbstractTile[Tile, Position],
                               Map <: AbstractMap[Tile, Position]]
  extends AbstractAgent[Position, EnvState, EnvGlobal, Action, Env]
{

  agent =>

  type Perception = MapPerception
  type DetailedPerception = MapDetailedPerception

  trait MapPerception extends AbstractGlobalPerception{
    def connectedStates: TilesConnections

    def mapSnapshot: MapSnapshot[Map, Tile, Position] with Map
  }

  trait MapDetailedPerception extends AbstractDetailedPerception{
    def shortcut: Route[Position]
  }

  //todo: doubling
  protected def sense(snapshot: MapEnvironmentSnapshot[Map, Tile, Position, EnvState, EnvGlobal, Action, Env]) =
    new MapPerception {
      val mapSnapshot = env.getMap(snapshot).asInstanceOf[MapSnapshot[Map, Tile, Position] with Map]

      lazy val connectedStates: TilesConnections = TilesConnections(mapSnapshot, perceived, position)

      val perceived: Seq[Position] = snapshot.visibleStates.keys.toSeq

      // todo: need access to AgentsPositionsProvidingMap's agentsPositions method
      val position: Position = snapshot.asEnv.asInstanceOf[AgentsPositionsProvidingMap[Tile, Position]].agentsPositions(agent.id).coordinate
    }

  def sense(env: EnvRef): Perception =
    new MapPerception{
      val mapSnapshot = env.getMap(env).asInstanceOf[MapSnapshot[Map, Tile, Position] with Map] // todo: casting

      lazy val connectedStates: TilesConnections = TilesConnections(mapSnapshot, perceived, position)

      val perceived: Seq[Position] = env.blocking.visibleStates.keys.toSeq

      val position: Position = env.position(agent)
    }

  def detailed(env: EnvRef, c: Position): Option[DetailedPerception] =     {
    val sensed = sense(env)
    val shortestRoutes = shortestRouteFinder.shortestRoutes(sensed.mapSnapshot: MapSnapshot[Map, Tile, Position])(sensed.position, sensed.perceived.toSet)
    if(sensed.perceived contains c) Some(
      new MapDetailedPerception {
        def shortcut: Route[Position] = shortestRoutes(c)
        type ActualDetailedPerception = MapState[Position, Tile, Map]
        def where: Position = c
        def what: ActualDetailedPerception = mapStateBuilder.build(sensed.mapSnapshot.getSnapshot(c))
      })
    else None
  }



  def shortestRouteFinder: ShortestRouteFinder[Map, Tile, Position]

  def mapStateBuilder: MapStateBuilder[Position, Tile, Map, EnvState]

  /**
   *   root should contain `tileState` of agent's position
   */
  case class TilesConnections(tileState: EnvState, connections: Set[TilesConnections])

  object TilesConnections{
    def apply(map: MapSnapshot[Map, Tile, Position], within: Seq[Position], starting: Position): TilesConnections = ???
    def withFilter(map: MapSnapshot[Map, Tile, Position], within: Seq[Position], starting: Position, filter: Position => Boolean) = ???
  }

}

trait MapEnvironmentOverseer[Map <: AbstractMap[Tile, Coordinate],
                             Tile <: AbstractTile[Tile, Coordinate],
                             Coordinate,
                             State <: MapState[Coordinate, Tile, Map],
                             Global <: MapGlobalState[Coordinate, Tile, Map],
                             Action <: MapAction[Coordinate, Tile, Map],
                             Env <: MapEnvironment[Map, Tile, Coordinate, State, Global, Action, Env]]{
  self: EnvironmentOverseer[Coordinate, State, Global, Action, Env] =>

  def mapSnapshot(): MapSnapshot[Map, Tile, Coordinate]
  def mapSnapshot(s: EnvironmentSnapshot[Coordinate, State, Global, Action, Env]): MapSnapshot[Map, Tile, Coordinate]
  def position(a: AbstractAgent[Coordinate, State, Global, Action, Env] with InAbstractMapEnvironment[Coordinate, State, Global, Action, Env, Tile, Map]): Coordinate
  def position(id: AgentId): Option[Coordinate]

}

trait MapEnvironmentOverseerWithActor[Map <: AbstractMap[Tile, Coordinate],
                                  Tile <: AbstractTile[Tile, Coordinate],
                                  Coordinate,
                                  State <: MapState[Coordinate, Tile, Map],
                                  Global <: MapGlobalState[Coordinate, Tile, Map],
                                  Action <: MapAction[Coordinate, Tile, Map],
                                  Env <: MapEnvironment[Map, Tile, Coordinate, State, Global, Action, Env]]
  extends MapEnvironmentOverseer[Map, Tile, Coordinate, State, Global, Action, Env] with EnvironmentOverseerWithActor[Coordinate, State, Global, Action, Env]
{
  overseer =>

  private type Ag = AbstractAgent[Coordinate, State, Global, Action, Env] with InAbstractMapEnvironment[Coordinate, State, Global, Action, Env, Tile, Map]

  case class GetMapByEnvRef(e: Env#Ref) extends UUIDed
  case class MapByEnvRef(uuid: UUID, snapshot: MapSnapshot[Map, Tile, Coordinate]) extends HasUUID

  case class GetMapBySnapshot(s: EnvironmentSnapshot[Coordinate, State, Global, Action, Env]) extends UUIDed
  case class MapBySnapshot(uuid: UUID, snapshot: MapSnapshot[Map, Tile, Coordinate]) extends HasUUID

  case class GetPosition(a: Ag) extends UUIDed
  case class Position(uuid: UUID, position: Coordinate) extends HasUUID

  def getMapMaxDelay: FiniteDuration
  def positionMaxDelay: FiniteDuration

  protected def mapActorResponses: PartialFunction[Any, () => Unit] = {
    case msg@GetMapByEnvRef(e) =>
      MapByEnvRef(msg.uuid, mapSnapshot()).liftUnit
    case msg@GetMapBySnapshot(s) => MapBySnapshot(msg.uuid, mapSnapshot(s)).liftUnit
    case msg@GetPosition(a) => Position(msg.uuid, position(a)).liftUnit
  }

  trait MapEnvironmentRefImpl extends MapEnvironmentRef[Coordinate, State, Global, Action, Env, Tile, Map]{
    def getMap(e: Env#Ref): MapSnapshot[Map, Tile, Coordinate] = overseer.mapSnapshot()
    def getMap(s: EnvironmentSnapshot[Coordinate, State, Global, Action, Env]): MapSnapshot[Map, Tile, Coordinate] = overseer.mapSnapshot(s)
    def position(a: Ag): Coordinate = overseer.position(a)
    def position(id: AgentId): Option[Coordinate] = overseer.position(id)

    def asyncGetMap(e: Env#Ref): Future[MapSnapshot[Map, Tile, Coordinate]] =
      overseer.actorRef.send(GetMapByEnvRef(e)).awaitingResponse[MapBySnapshot](positionMaxDelay).map(_.snapshot)
    def asyncGetMap(s: EnvironmentSnapshot[Coordinate, State, Global, Action, Env]): Future[MapSnapshot[Map, Tile, Coordinate]] =
      overseer.actorRef.send(GetMapBySnapshot(s)).awaitingResponse[MapBySnapshot](positionMaxDelay).map(_.snapshot)
    def asyncPosition(a: Ag): Future[Coordinate] =
      overseer.actorRef.send(GetPosition(a)).awaitingResponse[Position](positionMaxDelay).map(_.position)
  }

}


trait MapEnvironmentSnapshot[Map <: AbstractMap[Tile, Coordinate], Tile <: AbstractTile[Tile, Coordinate], Coordinate,
                             State, Global, Action <: AbstractAction, Env <: Environment[Coordinate, State, Global, Action, Env]]
  extends EnvironmentSnapshot[Coordinate, State, Global, Action, Env]
{
  self: Env =>

  def mapSnapshot: MapSnapshot[Map, Tile, Coordinate]
}