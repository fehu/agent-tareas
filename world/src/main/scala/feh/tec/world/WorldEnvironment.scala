package feh.tec.world

import feh.tec.agent._
import feh.tec.util.HasUUID
import scala.concurrent.Future
import java.util.UUID
import scala.concurrent.duration.FiniteDuration
import HasUUID._
import scala.collection.mutable
import feh.tec.util._

trait WorldEnvironment[World <: AbstractWorld[Atom, Coordinate],
                       Atom <: WorldAtom[Atom, Coordinate],
                       Coordinate,
                       State <: AtomState[Coordinate, Atom, World],
                       Global <: WorldState[Coordinate, Atom, World],
                       Action <: WorldAction[Coordinate, Atom, World],
                       Env <: WorldEnvironment[World, Atom, Coordinate, State, Global, Action, Env]]
  extends Environment[Coordinate, State, Global, Action, Env] with AbstractWorld[Atom, Coordinate]
{
  self: World =>

  override type Ref <: WorldEnvironmentRef[Coordinate, State, Global, Action, Env, Atom, World]

  def worldSnapshot: WorldSnapshot[World, Atom, Coordinate]
}

trait WorldAction[Coordinate, Atom <: WorldAtom[Atom, Coordinate], World <: AbstractWorld[Atom, Coordinate]] extends AbstractAction

trait WorldState[Coordinate, Atom <: WorldAtom[Atom, Coordinate], World <: AbstractWorld[Atom, Coordinate]]

trait AtomState[Coordinate, Atom <: WorldAtom[Atom, Coordinate], World <: AbstractWorld[Atom, Coordinate]]
trait WorldStateBuilder[Coordinate, Atom <: WorldAtom[Atom, Coordinate], World <: AbstractWorld[Atom, Coordinate],
                      State <: AtomState[Coordinate, Atom, World]]{
  def build(atom: Atom): State
  def build(snapshot: AtomSnapshot[Atom, Coordinate]): State
}

trait MutableWorldEnvironment[World <: AbstractWorld[Atom, Coordinate],
                              Atom <: WorldAtom[Atom, Coordinate],
                              Coordinate,
                              State <: AtomState[Coordinate, Atom, World],
                              Global <: WorldState[Coordinate, Atom, World],
                              Action <: WorldAction[Coordinate, Atom, World],
                              Env <: MutableWorldEnvironment[World, Atom, Coordinate, State, Global, Action, Env] with World]
  extends WorldEnvironment[World, Atom, Coordinate, State, Global, Action, Env] with MutableEnvironment[Coordinate, State, Global, Action, Env]
    with AgentsPositionsProvidingWorld[Atom, Coordinate]
{
  self: Env =>

  def initAtoms: Seq[Atom]
  private val _atomsMap = mutable.HashMap(zipAtomsWithCoords(initAtoms): _*)
  private val _agentsPositions = mutable.HashMap(calcAgentsPositions: _*)

  def get: PartialFunction[Coordinate, Atom] = _atomsMap

  private def zipAtomsWithCoords(atoms: Seq[Atom]) = atoms.map(t => t.coordinate -> t)
  private def setAtoms(atoms: Seq[Atom]) = _atomsMap ++= zipAtomsWithCoords(atoms)
  private def setAtom(atom: Atom) = _atomsMap += atom.coordinate -> atom

  def atomsMap = _atomsMap.toMap

  def atoms: Seq[Atom] = atomsMap.values.toSeq

  def stateByAtom(atom: Atom): State
  def statesMap = atomsMap mapValues stateByAtom
  override def states: PartialFunction[Coordinate, State] = PartialFunction(atomsMap.apply _ andThen stateByAtom)
  override def states_=(pf: PartialFunction[Coordinate, State]): Unit = {}

  protected def updateAgentAtAtom(newAtom: Atom): Atom = {
    agentAtAtom(newAtom).foreach{
      _agentsPositions += _ -> newAtom
    }
    newAtom
  }

  private implicit class AtomTransformFuncWrapper(f: Atom => Atom){
    def ensuringSameCoordinates: Atom => Atom = old => f(old).ensuring(_.coordinate == old.coordinate)
  }

  def transformAtoms(f: Atom => Atom): Env = {
    setAtoms(atoms.map(updateAgentAtAtom _ compose f.ensuringSameCoordinates))
    this
  }

  def transformAtom(pos: Coordinate)(f: Atom => Atom): Env = {
    setAtom(get(pos) |> (updateAgentAtAtom _ compose f.ensuringSameCoordinates))
    this
  }

  private def calcAgentsPositions = atoms.flatMap(t => agentAtAtom(t).map(_ -> t))

  protected def agentAtAtom(atom: Atom): Option[AgentId]

  def agentsPositions = _agentsPositions.toMap

  def agentPosition(id: AgentId): Option[Coordinate] = agentsPositions.get(id).map(_.coordinate)
}

trait WorldEnvironmentRef[Coordinate, State <: AtomState[Coordinate, Atom, World], Global <: WorldState[Coordinate, Atom, World],
                          Action <: WorldAction[Coordinate, Atom, World], Env <: WorldEnvironment[World, Atom, Coordinate, State, Global, Action, Env],
                          Atom <: WorldAtom[Atom, Coordinate], World <: AbstractWorld[Atom, Coordinate]]
  extends EnvironmentRef[Coordinate, State, Global, Action, Env]
{
  def worldSnapshot: WorldSnapshot[World, Atom, Coordinate]
  def worldSnapshot(s: EnvironmentSnapshot[Coordinate, State, Global, Action, Env]): WorldSnapshot[World, Atom, Coordinate]

  def asyncWorldSnapshot(e: Env#Ref): Future[WorldSnapshot[World, Atom, Coordinate]]
  def asyncWorldSnapshot(s: EnvironmentSnapshot[Coordinate, State, Global, Action, Env]): Future[WorldSnapshot[World, Atom, Coordinate]]
}

trait InAbstractWorldEnvironment[Position,
                                 EnvState <: AtomState[Position, Atom, World],
                                 EnvGlobal <: WorldState[Position, Atom, World],
                                 Action <: WorldAction[Position, Atom, World],
                                 Env <: Environment[Position, EnvState, EnvGlobal, Action, Env] with WorldEnvironment[World, Atom, Position, EnvState, EnvGlobal, Action, Env],
                                 Atom <: WorldAtom[Atom, Position],
                                 World <: AbstractWorld[Atom, Position]]
  extends AbstractAgent[Position, EnvState, EnvGlobal, Action, Env]
{

  agent =>

  type Perception = WorldPerception
  type DetailedPerception = AtomPerception

  trait WorldPerception extends AbstractGlobalPerception{
    def worldSnapshot: WorldSnapshot[World, Atom, Position] with World
  }

  trait AtomPerception extends AbstractDetailedPerception{
    def shortcut: Route[Position]
  }

  //todo: doubling
  protected def sense(snapshot: WorldEnvironmentSnapshot[World, Atom, Position, EnvState, EnvGlobal, Action, Env]) =
    new WorldPerception {
      val worldSnapshot = env.worldSnapshot(snapshot).asInstanceOf[WorldSnapshot[World, Atom, Position] with World]

      val perceived: Seq[Position] = snapshot.visibleStates.keys.toSeq

      // todo: need access to AgentsPositionsProvidingMap's agentsPositions method
      val position: Position = snapshot.asEnv.asInstanceOf[AgentsPositionsProvidingWorld[Atom, Position]].agentsPositions(agent.id).coordinate
    }

  def sense(env: EnvRef): Perception =
    new WorldPerception{
      val worldSnapshot = env.worldSnapshot.asInstanceOf[WorldSnapshot[World, Atom, Position] with World] // todo: casting

      val perceived: Seq[Position] = env.blocking.visibleStates.keys.toSeq

      val position: Position = env.blocking.agentPosition(agent.id).get
    }

  def detailed(env: EnvRef, c: Position): Option[DetailedPerception] =     {
    val sensed = sense(env)
    val shortestRoutes = shortestRouteFinder.shortestRoutes(sensed.worldSnapshot: WorldSnapshot[World, Atom, Position])(sensed.position, sensed.perceived.toSet)
    if(sensed.perceived contains c) Some(
      new AtomPerception {
        def shortcut: Route[Position] = shortestRoutes(c)
        type ActualDetailedPerception = AtomState[Position, Atom, World]
        def where: Position = c
        def what: ActualDetailedPerception = worldStateBuilder.build(sensed.worldSnapshot.getSnapshot(c))
      })
    else None
  }



  def shortestRouteFinder: ShortestRouteFinder[World, Atom, Position]

  def worldStateBuilder: WorldStateBuilder[Position, Atom, World, EnvState]

}

trait WorldEnvironmentOverseer[World <: AbstractWorld[Atom, Coordinate],
                               Atom <: WorldAtom[Atom, Coordinate],
                               Coordinate,
                               State <: AtomState[Coordinate, Atom, World],
                               Global <: WorldState[Coordinate, Atom, World],
                               Action <: WorldAction[Coordinate, Atom, World],
                               Env <: WorldEnvironment[World, Atom, Coordinate, State, Global, Action, Env]]{
  self: EnvironmentOverseer[Coordinate, State, Global, Action, Env] =>

  def worldSnapshot(): WorldSnapshot[World, Atom, Coordinate]
  def worldSnapshot(s: EnvironmentSnapshot[Coordinate, State, Global, Action, Env]): WorldSnapshot[World, Atom, Coordinate]

}

trait WorldEnvironmentOverseerWithActor[World <: AbstractWorld[Atom, Coordinate],
                                        Atom <: WorldAtom[Atom, Coordinate],
                                        Coordinate,
                                        State <: AtomState[Coordinate, Atom, World],
                                        Global <: WorldState[Coordinate, Atom, World],
                                        Action <: WorldAction[Coordinate, Atom, World],
                                        Env <: WorldEnvironment[World, Atom, Coordinate, State, Global, Action, Env]]
  extends WorldEnvironmentOverseer[World, Atom, Coordinate, State, Global, Action, Env] with EnvironmentOverseerWithActor[Coordinate, State, Global, Action, Env]
{
  overseer =>

  private type Ag = AbstractAgent[Coordinate, State, Global, Action, Env] with InAbstractWorldEnvironment[Coordinate, State, Global, Action, Env, Atom, World]

  case class GetWorldByEnvRef(e: Env#Ref) extends UUIDed
  case class WorldByEnvRef(uuid: UUID, snapshot: WorldSnapshot[World, Atom, Coordinate]) extends HasUUID

  case class GetWorldBySnapshot(s: EnvironmentSnapshot[Coordinate, State, Global, Action, Env]) extends UUIDed
  case class WorldBySnapshot(uuid: UUID, snapshot: WorldSnapshot[World, Atom, Coordinate]) extends HasUUID



  def getWorldMaxDelay: FiniteDuration
  def positionMaxDelay: FiniteDuration

  protected def worldActorResponses: PartialFunction[Any, () => Unit] = {
    case msg@GetWorldByEnvRef(e) =>
      WorldByEnvRef(msg.uuid, worldSnapshot()).liftUnit
    case msg@GetWorldBySnapshot(s) => WorldBySnapshot(msg.uuid, worldSnapshot(s)).liftUnit
  }

  trait WorldEnvironmentRefImpl extends WorldEnvironmentRef[Coordinate, State, Global, Action, Env, Atom, World]{
    def worldSnapshot: WorldSnapshot[World, Atom, Coordinate] = overseer.worldSnapshot()
    def worldSnapshot(s: EnvironmentSnapshot[Coordinate, State, Global, Action, Env]): WorldSnapshot[World, Atom, Coordinate] = overseer.worldSnapshot(s)

    def asyncWorldSnapshot(e: Env#Ref): Future[WorldSnapshot[World, Atom, Coordinate]] =
      overseer.actorRef.send(GetWorldByEnvRef(e)).awaitingResponse[WorldBySnapshot](positionMaxDelay).map(_.snapshot)
    def asyncWorldSnapshot(s: EnvironmentSnapshot[Coordinate, State, Global, Action, Env]): Future[WorldSnapshot[World, Atom, Coordinate]] =
      overseer.actorRef.send(GetWorldBySnapshot(s)).awaitingResponse[WorldBySnapshot](positionMaxDelay).map(_.snapshot)
  }

}


trait WorldEnvironmentSnapshot[World <: AbstractWorld[Atom, Coordinate], Atom <: WorldAtom[Atom, Coordinate], Coordinate,
                               State, Global, Action <: AbstractAction, Env <: Environment[Coordinate, State, Global, Action, Env]]
  extends EnvironmentSnapshot[Coordinate, State, Global, Action, Env]
{
  self: Env =>

  def worldSnapshot: WorldSnapshot[World, Atom, Coordinate]
}