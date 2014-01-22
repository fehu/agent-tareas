package feh.tec.world

import feh.tec.agent._
import feh.util.{UUIDed, HasUUID}
import scala.concurrent.Future
import java.util.UUID
import scala.concurrent.duration.FiniteDuration
import scala.collection.mutable
import feh.tec.util._
import feh.tec.util.HasUUIDForAkka.AsyncSendMsgHasUUIDWrapper

trait WorldEnvironment[Env <: Environment[Env],
                       World <: AbstractWorld[Atom, Env#Coordinate],
                       Atom <: WorldAtom[Atom, Env#Coordinate]]
  extends Environment[Env] with AbstractWorld[Atom, Env#Coordinate]
{
  self: World with Env =>

  override type Ref <: WorldEnvironmentRef[Env, Atom, World]

  def worldSnapshot: WorldSnapshot[World, Atom, Env#Coordinate]
}

trait WorldAction[Coordinate, Atom <: WorldAtom[Atom, Coordinate], World <: AbstractWorld[Atom, Coordinate]] extends AbstractAction

trait WorldState[Coordinate, Atom <: WorldAtom[Atom, Coordinate], World <: AbstractWorld[Atom, Coordinate]]

trait AtomState[Coordinate, Atom <: WorldAtom[Atom, Coordinate], World <: AbstractWorld[Atom, Coordinate]]
trait WorldStateBuilder[Env <: Environment[Env],
                        Atom <: WorldAtom[Atom, Env#Coordinate],
                        World <: AbstractWorld[Atom, Env#Coordinate]]{
  def build(atom: Atom): Env#State
  def build(snapshot: AtomSnapshot[Atom, Env#Coordinate]): Env#State
}

trait MutableWorldEnvironment[Env <: MutableEnvironment[Env] ,
                              World <: AbstractWorld[Atom, Env#Coordinate],
                              Atom <: WorldAtom[Atom, Env#Coordinate]]
  extends WorldEnvironment[Env, World, Atom] with MutableEnvironment[Env]
    with AgentsPositionsProvidingWorld[Atom, Env#Coordinate]
{
  self: Env with World =>

  def initAtoms: Seq[Atom]
  private val _atomsMap = mutable.HashMap(zipAtomsWithCoords(initAtoms): _*)
  private val _agentsPositions = mutable.HashMap(calcAgentsPositions: _*)

  def get: PartialFunction[Env#Coordinate, Atom] = _atomsMap

  private def zipAtomsWithCoords(atoms: Seq[Atom]) = atoms.map(t => t.coordinate -> t)
  private def setAtoms(atoms: Seq[Atom]) = _atomsMap ++= zipAtomsWithCoords(atoms)
  private def setAtom(atom: Atom) = _atomsMap += atom.coordinate -> atom

  def atomsMap = _atomsMap.toMap

  def atoms: Seq[Atom] = atomsMap.values.toSeq

  def stateByAtom(atom: Atom): Env#State
  def statesMap = atomsMap mapValues stateByAtom
  override def states: PartialFunction[Env#Coordinate, Env#State] = PartialFunction(atomsMap.apply _ andThen stateByAtom)
  override def states_=(pf: PartialFunction[Env#Coordinate, Env#State]): Unit = {}

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

  def transformAtom(pos: Env#Coordinate)(f: Atom => Atom): Env = {
    setAtom(get(pos) |> (updateAgentAtAtom _ compose f.ensuringSameCoordinates))
    this
  }

  private def calcAgentsPositions = atoms.flatMap(t => agentAtAtom(t).map(_ -> t))

  protected def agentAtAtom(atom: Atom): Option[AgentId]

  def agentsPositions = _agentsPositions.toMap

  def agentPosition(id: AgentId): Option[Env#Coordinate] = agentsPositions.get(id).map(_.coordinate)
}

trait WorldEnvironmentRef[Env <: Environment[Env],
                          Atom <: WorldAtom[Atom, Env#Coordinate],
                          World <: AbstractWorld[Atom, Env#Coordinate]]
  extends EnvironmentRef[Env]
{
  def worldSnapshot: WorldSnapshot[World, Atom, Env#Coordinate]
  def worldSnapshot(s: EnvironmentSnapshot[Env]): WorldSnapshot[World, Atom, Env#Coordinate]

  def asyncWorldSnapshot(e: Env#Ref): Future[WorldSnapshot[World, Atom, Env#Coordinate]]
  def asyncWorldSnapshot(s: EnvironmentSnapshot[Env]): Future[WorldSnapshot[World, Atom, Env#Coordinate]]
}

trait InAbstractWorldEnvironment[Env <: WorldEnvironment[Env, World, Atom] { type State <: AtomState[Env#Coordinate, Atom, World] },
                                 Atom <: WorldAtom[Atom, Env#Coordinate],
                                 World <: AbstractWorld[Atom, Env#Coordinate]]
  extends AbstractAgent[Env]
{

  agent =>

  type Perception = WorldPerception
  type DetailedPerception = AtomPerception

  trait WorldPerception extends AbstractGlobalPerception{
    def worldSnapshot: WorldSnapshot[World, Atom, Env#Coordinate] with World
  }

  trait AtomPerception extends AbstractDetailedPerception{
    def shortcut: Route[Env#Coordinate]
  }

  //todo: doubling
  protected def sense(snapshot: WorldEnvironmentSnapshot[Env, World, Atom]) =
    new WorldPerception {
      val worldSnapshot = env.worldSnapshot(snapshot).asInstanceOf[WorldSnapshot[World, Atom, Env#Coordinate] with World]

      val perceived: Seq[Env#Coordinate] = snapshot.visibleStates.keys.toSeq

      // todo: need access to AgentsPositionsProvidingMap's agentsPositions method
      val position: Env#Coordinate = snapshot.asEnv.asInstanceOf[AgentsPositionsProvidingWorld[Atom, Env#Coordinate]].agentsPositions(agent.id).coordinate
    }

  def sense(env: EnvRef): Perception =
    new WorldPerception{
      val worldSnapshot = env.worldSnapshot.asInstanceOf[WorldSnapshot[World, Atom, Env#Coordinate] with World] // todo: casting

      val perceived: Seq[Env#Coordinate] = env.blocking.visibleStates.keys.toSeq

      val position: Env#Coordinate = env.blocking.agentPosition(agent.id).get
    }

  def detailed(env: EnvRef, c: Env#Coordinate): Option[DetailedPerception] =     {
    val sensed = sense(env)
    val shortestRoutes = shortestRouteFinder.shortestRoutes(sensed.worldSnapshot: WorldSnapshot[World, Atom, Env#Coordinate])(sensed.position, sensed.perceived.toSet)
    if(sensed.perceived contains c) Some(
      new AtomPerception {
        def shortcut: Route[Env#Coordinate] = shortestRoutes(c)
        type ActualDetailedPerception = AtomState[Env#Coordinate, Atom, World]
        def where: Env#Coordinate = c
        def what: ActualDetailedPerception = worldStateBuilder.build(sensed.worldSnapshot.getSnapshot(c))
      })
    else None
  }



  def shortestRouteFinder: ShortestRouteFinder[World, Atom, Env#Coordinate]

  def worldStateBuilder: WorldStateBuilder[Env, Atom, World]

}

trait WorldEnvironmentOverseer[Env <: WorldEnvironment[Env, World, Atom],
                               World <: AbstractWorld[Atom, Env#Coordinate],
                               Atom <: WorldAtom[Atom, Env#Coordinate]]{
  self: EnvironmentOverseer[Env] =>

  def worldSnapshot(): WorldSnapshot[World, Atom, Env#Coordinate]
  def worldSnapshot(s: EnvironmentSnapshot[Env]): WorldSnapshot[World, Atom, Env#Coordinate]

}

trait WorldEnvironmentOverseerWithActor[Env <: WorldEnvironment[Env, World, Atom] { type State <: AtomState[Env#Coordinate, Atom, World] },
                                        World <: AbstractWorld[Atom, Env#Coordinate],
                                        Atom <: WorldAtom[Atom, Env#Coordinate]]
  extends WorldEnvironmentOverseer[Env, World, Atom] with EnvironmentOverseerWithActor[Env]
{
  overseer =>

  private type Ag = AbstractAgent[Env] with InAbstractWorldEnvironment[Env, Atom, World]

  case class GetWorldByEnvRef(e: Env#Ref) extends UUIDed
  case class WorldByEnvRef(uuid: UUID, snapshot: WorldSnapshot[World, Atom, Env#Coordinate]) extends HasUUID

  case class GetWorldBySnapshot(s: EnvironmentSnapshot[Env]) extends UUIDed
  case class WorldBySnapshot(uuid: UUID, snapshot: WorldSnapshot[World, Atom, Env#Coordinate]) extends HasUUID



  def getWorldMaxDelay: FiniteDuration
  def positionMaxDelay: FiniteDuration

  protected def worldActorResponses: PartialFunction[Any, () => Any] = {
    case msg@GetWorldByEnvRef(e) =>
      WorldByEnvRef(msg.uuid, worldSnapshot()).lifted
    case msg@GetWorldBySnapshot(s) => WorldBySnapshot(msg.uuid, worldSnapshot(s)).lifted
  }

  trait WorldEnvironmentRefImpl extends WorldEnvironmentRef[Env, Atom, World]{
    def worldSnapshot: WorldSnapshot[World, Atom, Env#Coordinate] = overseer.worldSnapshot()
    def worldSnapshot(s: EnvironmentSnapshot[Env]): WorldSnapshot[World, Atom, Env#Coordinate] = overseer.worldSnapshot(s)

    def asyncWorldSnapshot(e: Env#Ref): Future[WorldSnapshot[World, Atom, Env#Coordinate]] =
      overseer.actorRef.send(GetWorldByEnvRef(e)).awaitingResponse[WorldBySnapshot](positionMaxDelay).map(_.snapshot)
    def asyncWorldSnapshot(s: EnvironmentSnapshot[Env]): Future[WorldSnapshot[World, Atom, Env#Coordinate]] =
      overseer.actorRef.send(GetWorldBySnapshot(s)).awaitingResponse[WorldBySnapshot](positionMaxDelay).map(_.snapshot)
  }

}


trait WorldEnvironmentSnapshot[Env <: Environment[Env],
                               World <: AbstractWorld[Atom, Env#Coordinate],
                               Atom <: WorldAtom[Atom, Env#Coordinate]]
  extends EnvironmentSnapshot[Env]
{
  self: Env with World =>

  def worldSnapshot: WorldSnapshot[World, Atom, Env#Coordinate]
}