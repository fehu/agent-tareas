package feh.tec.map

import feh.tec.agent._
import feh.tec.map.tile.AbstractTile
import feh.tec.util.{HasUUID, UUIDed, SideEffect}
import scala.concurrent.{ExecutionContext, Future}
import akka.actor.Actor
import java.util.UUID
import feh.tec.util.PipeWrapper
import akka.pattern._
import scala.concurrent.duration.{Duration, FiniteDuration}
import HasUUID._
import scala.Predef
import scala.collection.mutable

trait MapEnvironment[Map <: AbstractMap[Tile, Coordinate],
                             Tile <: AbstractTile[Tile, Coordinate],
                             Coordinate,
                             State <: MapState[Coordinate, Tile, Map],
                             Global <: MapGlobalState[Coordinate, Tile, Map],
                             Action <: MapAction[Coordinate, Tile, Map],
                             Env <: MapEnvironment[Map, Tile, Coordinate, State, Global, Action, Env]]
  extends Environment[Coordinate, State, Global, Action, Env] with AbstractMap[Tile, Coordinate]
{
  override type Ref <: MapEnvironmentRef[Coordinate, State, Global, Action, Env, Tile, Map]
}

trait MapAction[Coordinate, Tile <: AbstractTile[Tile, Coordinate], Map <: AbstractMap[Tile, Coordinate]] extends AbstractAction

trait MapGlobalState[Coordinate, Tile <: AbstractTile[Tile, Coordinate], Map <: AbstractMap[Tile, Coordinate]]

trait MapState[Coordinate, Tile <: AbstractTile[Tile, Coordinate], Map <: AbstractMap[Tile, Coordinate]]
trait MapStateBuilder[Coordinate, Tile <: AbstractTile[Tile, Coordinate], Map <: AbstractMap[Tile, Coordinate],
                      State <: MapState[Coordinate, Tile, Map]]{
  def build: Tile => State
}

trait MutableMapEnvironment[Map <: AbstractMap[Tile, Coordinate],
                            Tile <: AbstractTile[Tile, Coordinate],
                            Coordinate,
                            State <: MapState[Coordinate, Tile, Map],
                            Global <: MapGlobalState[Coordinate, Tile, Map],
                            Action <: MapAction[Coordinate, Tile, Map],
                            Env <: MutableMapEnvironment[Map, Tile, Coordinate, State, Global, Action, Env]]
//                              with MutableEnvironment[Coordinate, State, Global, Action, Env]]
  extends MapEnvironment[Map, Tile, Coordinate, State, Global, Action, Env] with MutableEnvironment[Coordinate, State, Global, Action, Env]
  with AgentsPositionsProvidingMap[Tile, Coordinate]
{
  self: Env =>

  def initTiles: Seq[Tile]
  private val _tilesMap = mutable.HashMap(zipTilesWithCoords(initTiles): _*)
  private val _agentsPositions = mutable.HashMap(calcAgentsPositions: _*)

  private def zipTilesWithCoords(tiles: Seq[Tile]) = initTiles.map(t => t.coordinate -> t)
  private def setTiles(tiles: Seq[Tile]) = _tilesMap ++= zipTilesWithCoords(tiles)

  def tilesAsMap = _tilesMap.toMap

  def tiles: Seq[Tile] = tilesAsMap.values.toSeq
  def transformTiles(f: Tile => Tile): Env = {
    setTiles(tiles.map{ tile =>
      val newTile = f(tile)

      agentInTile(newTile).foreach{
        _agentsPositions += _ -> newTile
      }

      newTile
    })
    this
  }

  private def calcAgentsPositions = tiles.flatMap(t => agentInTile(t).map(_ -> t))

  protected def agentInTile(tile: Tile): Option[AgentId]

  def agentsPositions: collection.Map[AgentId, Tile] = _agentsPositions
}

trait MapEnvironmentRef[Coordinate, State <: MapState[Coordinate, Tile, Map], Global <: MapGlobalState[Coordinate, Tile, Map],
                        Action <: MapAction[Coordinate, Tile, Map], Env <: MapEnvironment[Map, Tile, Coordinate, State, Global, Action, Env],
                        Tile <: AbstractTile[Tile, Coordinate], Map <: AbstractMap[Tile, Coordinate]]
  extends EnvironmentRef[Coordinate, State, Global, Action, Env]
{
  def getMap(e: Env#Ref): MapSnapshot[Map, Tile, Coordinate]
  def getMap(s: EnvironmentSnapshot[Coordinate, State, Global, Action, Env]): MapSnapshot[Map, Tile, Coordinate]
  def position(a: AbstractAgent[Coordinate, State, Global, Action, Env] with InAbstractMapEnvironment[Coordinate, State, Global, Action, Env, Tile, Map]): Coordinate

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
  }

  trait MapDetailedPerception extends AbstractDetailedPerception{
    def shortcut: Route[Position]
  }

  def sense(env: EnvRef): Perception =
    new MapPerception{
      lazy val connectedStates: TilesConnections = TilesConnections(env.getMap(env), perceived, position)

      val perceived: Seq[Position] = env.blocking.visibleStates.keys.toSeq

      val position: Position = env.position(agent)
    }


  def shortestRoute(from: Position, to: Position): Route[Position]

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
                             Env <: /*Environment[Coordinate, State, Global, Action, Env]
                               with */MapEnvironment[Map, Tile, Coordinate, State, Global, Action, Env]]{
  self: EnvironmentOverseer[Coordinate, State, Global, Action, Env] =>

  def getMap(): MapSnapshot[Map, Tile, Coordinate]
  def getMap(s: EnvironmentSnapshot[Coordinate, State, Global, Action, Env]): MapSnapshot[Map, Tile, Coordinate]
  def position(a: AbstractAgent[Coordinate, State, Global, Action, Env] with InAbstractMapEnvironment[Coordinate, State, Global, Action, Env, Tile, Map]): Coordinate



}

trait MapEnvironmentOverseerActor[Map <: AbstractMap[Tile, Coordinate],
                                  Tile <: AbstractTile[Tile, Coordinate],
                                  Coordinate,
                                  State <: MapState[Coordinate, Tile, Map],
                                  Global <: MapGlobalState[Coordinate, Tile, Map],
                                  Action <: MapAction[Coordinate, Tile, Map],
                                  Env <: MapEnvironment[Map, Tile, Coordinate, State, Global, Action, Env]]
  extends MapEnvironmentOverseer[Map, Tile, Coordinate, State, Global, Action, Env] with EnvironmentOverseerActor[Coordinate, State, Global, Action, Env]
{
  agent =>

  private type Ag = AbstractAgent[Coordinate, State, Global, Action, Env] with InAbstractMapEnvironment[Coordinate, State, Global, Action, Env, Tile, Map]

  case class GetMapByEnvRef(e: Env#Ref) extends UUIDed
  case class MapByEnvRef(uuid: UUID, snapshot: MapSnapshot[Map, Tile, Coordinate]) extends HasUUID

  case class GetMapBySnapshot(s: EnvironmentSnapshot[Coordinate, State, Global, Action, Env]) extends UUIDed
  case class MapBySnapshot(uuid: UUID, snapshot: MapSnapshot[Map, Tile, Coordinate]) extends HasUUID

  case class GetPosition(a: Ag) extends UUIDed
  case class Position(uuid: UUID, position: Coordinate) extends HasUUID

  def getMapMaxDelay: FiniteDuration
  def positionMaxDelay: FiniteDuration

  private implicit def executionContext: ExecutionContext = externalExecutionContext

  override def receive: Actor.Receive = super.receive orElse {
    case msg@GetMapByEnvRef(e) => scheduler.scheduleOnce(Duration.Zero){
      sender ! MapByEnvRef(msg.uuid, getMap())
    }
    case msg@GetMapBySnapshot(s) => scheduler.scheduleOnce(Duration.Zero){
      sender ! MapBySnapshot(msg.uuid, getMap(s))
    }
    case msg@GetPosition(a) => scheduler.scheduleOnce(Duration.Zero){
      sender ! Position(msg.uuid, position(a))
    }
  }

  trait MapEnvironmentRefImpl extends MapEnvironmentRef[Coordinate, State, Global, Action, Env, Tile, Map]{
    def getMap(e: Env#Ref): MapSnapshot[Map, Tile, Coordinate] = agent.getMap()
    def getMap(s: EnvironmentSnapshot[Coordinate, State, Global, Action, Env]): MapSnapshot[Map, Tile, Coordinate] = agent.getMap(s)
    def position(a: Ag): Coordinate = agent.position(a)

    def asyncGetMap(e: Env#Ref): Future[MapSnapshot[Map, Tile, Coordinate]] =
      agent.send(GetMapByEnvRef(e)).awaitingResponse[MapBySnapshot](positionMaxDelay).map(_.snapshot)
    def asyncGetMap(s: EnvironmentSnapshot[Coordinate, State, Global, Action, Env]): Future[MapSnapshot[Map, Tile, Coordinate]] =
      agent.send(GetMapBySnapshot(s)).awaitingResponse[MapBySnapshot](positionMaxDelay).map(_.snapshot)
    def asyncPosition(a: Ag): Future[Coordinate] =
      agent.send(GetPosition(a)).awaitingResponse[Position](positionMaxDelay).map(_.position)
  }

}
