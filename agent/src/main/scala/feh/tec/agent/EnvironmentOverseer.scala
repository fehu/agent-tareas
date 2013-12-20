package feh.tec.agent

import akka.actor.{ActorRef, Scheduler}
import scala.concurrent.{ExecutionContext, Future}
import java.util.{UUID, Calendar}
import akka.pattern._
import akka.util.Timeout
import scala.concurrent.duration._
import feh.tec.util._

/**
 *  manages the environment, hides actual environment change implementation
 *  produces environment references
 */
trait EnvironmentOverseer[Env <: Environment[Env]] {
  def currentEnvironment: Env
  def env = currentEnvironment

  protected[agent] def updateEnvironment(f: Env => Env): SideEffect[Env]

  def snapshot: EnvironmentSnapshot[Env] with Env
  def ref: Env#Ref

  def affect(a: Env#Action): SideEffect[Env]
}

protected[agent] object EnvironmentOverseerWithActor{
  sealed trait Message
  sealed class MessageResponse[R](val response: R) extends Message

  object Get{
    case object GlobalState extends Message
    case class StateOf[Coordinate](c: Coordinate) extends Message
    case object VisibleStates extends Message
    case object Snapshot extends Message
    case class Position(a: AgentId) extends UUIDed
  }

  case class Act[Action <: AbstractAction](act: Action) extends Message

  object Response{
    case class GlobalState[Global](global: Global) extends MessageResponse(global)
    case class StateOf[Coordinate, State](c: Coordinate, stateOpt: Option[State]) extends MessageResponse(c -> stateOpt)
    case class VisibleStates[Coordinate, State](states: Map[Coordinate, State]) extends MessageResponse(states)
    case class ActionApplied[Action <: AbstractAction](a: Action) extends MessageResponse(a)
    case class Snapshot[Env <: Environment[Env]](snapshot: EnvironmentSnapshot[Env], time: Long) extends MessageResponse(snapshot -> time)
    case class Position[Coordinate](uuid: UUID, position: Option[Coordinate]) extends HasUUID
  }
}

trait EnvironmentOverseerWithActor[Env <: Environment[Env]] extends EnvironmentOverseer[Env]{
  overseer =>

  def actorRef: ActorRef

  import EnvironmentOverseerWithActor._

  def affect(act: Env#Action): SideEffect[Env] = {
    updateEnvironment{_.affected(act).execute}
  }


  protected def baseActorResponses: PartialFunction[Any, () => Any] = {
    val localEnv = env
    val pf: PartialFunction[Any, () => Any] = {
      case Get.GlobalState => Response.GlobalState(localEnv.globalState).lifted
      case g@Get.StateOf(c: Env#Coordinate) => Response.StateOf(c, localEnv.stateOf(c)).lifted
      case Get.VisibleStates => Response.VisibleStates(localEnv.visibleStates).lifted
      case Get.Snapshot => Response.Snapshot(snapshot, Calendar.getInstance().getTimeInMillis).lifted
      case a@Act(act: Env#Action) =>
        affect(act).execute
        Response.ActionApplied(act).lifted
      case msg@Get.Position(id) => Response.Position(msg.uuid, env.agentPosition(id)).lifted
    }
    pf
  }

  protected implicit def executionContext: ExecutionContext
  protected def scheduler: Scheduler

  def defaultBlockingTimeout: Int
  def defaultFutureTimeout: Int

  trait BaseEnvironmentRef extends EnvironmentRef[Env]
    {
      self =>

      def envRef = self.asInstanceOf[Env#Ref]

      def defaultBlockingTimeout: Int = overseer.defaultBlockingTimeout
      protected val futureTimeoutScope = new ScopedState(defaultFutureTimeout)

      lazy val sys: SystemApi = new SystemApi {
        implicit def executionContext: ExecutionContext = overseer.executionContext
        def scheduler: Scheduler = overseer.scheduler
      }


    lazy val blocking: BlockingApi = new BlockingApi {
      def visibleStates: Map[Env#Coordinate, Env#State] = overseer.env.visibleStates
      def snapshot: Env with EnvironmentSnapshot[Env] = overseer.snapshot
      def stateOf(c: Env#Coordinate): Option[Env#State] = overseer.env.stateOf(c)
      def globalState: Env#Global = overseer.env.globalState
      def affect(act: Env#Action): SideEffect[Env#Ref] = overseer.affect(act).more(overseer.ref).flatExec
      def agentPosition(id: AgentId): Option[Env#Coordinate] = overseer.env.agentPosition(id)
    }

    lazy val async: AsyncApi = new AsyncApi{
        def withTimeout[R](t: Int)(r: => R): R = futureTimeoutScope.doWith(t)(r)

        implicit def timeout = Timeout(futureTimeoutScope.get)

        def globalState: Future[Env#Global] = (overseer.actorRef ? Get.GlobalState)
          .mapTo[Response.GlobalState[_]].map(_.global.asInstanceOf[Env#Global])

        def stateOf(c: Env#Coordinate) = (overseer.actorRef ? Get.StateOf(c))
          .mapTo[Response.StateOf[Env#Coordinate, Env#State]]
          .withFilter(_.c == c)
          .map(_.stateOpt)

        def affect(act: Env#Action) = (overseer.actorRef ? Act(act))
          .mapTo[Response.ActionApplied[_]]
          .withFilter(_.a == Act)
          .map(_ => SideEffect(envRef))

        def visibleStates = (overseer.actorRef ? Get.VisibleStates)
          .mapTo[Response.VisibleStates[_, _]]
          .map(_.states.asInstanceOf[Map[Env#Coordinate, Env#State]])

        def snapshot = (overseer.actorRef ? Get.Snapshot)
          .mapTo[Response.Snapshot[Env]]
          .map(_.snapshot.asInstanceOf[Env with EnvironmentSnapshot[Env]])

      def agentPosition(id: AgentId): Future[Option[Env#Coordinate]] = Get.Position(id) |> (msg =>
        (overseer.actorRef ? msg).mapTo[Response.Position[Env#Coordinate]].havingSameUUID(msg).map(_.position)
        )
    }
  }
}

trait EnvironmentOverseerImplementation[Env <: Environment[Env]]{
  self: EnvironmentOverseer[Env] =>
}

trait MutableEnvironmentOverseer[Env <: Environment[Env] with MutableEnvironment[Env]]
  extends EnvironmentOverseerImplementation[Env]
{
  self: EnvironmentOverseer[Env] =>

  override val currentEnvironment: Env

  protected[agent] def updateEnvironment(f: (Env) => Env): SideEffect[Env] = SideEffect(f(env))
}

trait ImmutableEnvironmentOverseer[Env <: Environment[Env] with ImmutableEnvironment[Env]]
  extends EnvironmentOverseerImplementation[Env]
{
  self: EnvironmentOverseer[Env] =>

  def initialEnvironment: Env
  protected var environment = initialEnvironment
  def currentEnvironment: Env = environment
  protected[agent] def updateEnvironment(f: (Env) => Env): SideEffect[Env] = SideEffect{
    val e = f(env)
    environment = e
    e
  }
}

trait PredictingEnvironmentOverseer[Env <: Environment[Env] with PredictableEnvironment[Env]]{
  self: EnvironmentOverseer[Env] =>

  def predict(a: Env#Action): Env#Prediction

}

trait PredictingEnvironmentOverseerWithActor[Env <: Environment[Env] with PredictableEnvironment[Env]]
  extends PredictingEnvironmentOverseer[Env]
{
  overseer: EnvironmentOverseerWithActor[Env] =>

  case class Predict(a: Env#Action) extends UUIDed
  case class Prediction(uuid: UUID, p: Env#Prediction) extends HasUUID

  def predictMaxDelay: FiniteDuration

  protected def predictingActorResponses: PartialFunction[Any, () => Any] = {
    case msg@Predict(a) => Prediction(msg.uuid, predict(a)).lifted
  }

  trait PredictableEnvironmentRefImpl extends PredictableEnvironmentRef[Env]{
    def predict(a: Env#Action): Env#Prediction = overseer predict a

    def asyncPredict(a: Env#Action): Future[Env#Prediction] = Predict(a) |> { msg =>
      (overseer.actorRef ? msg)(predictMaxDelay)
        .mapTo[Prediction].havingSameUUID(msg).map(_.p)
    }
  }
}

trait PredictingMutableEnvironmentOverseer[Env <: Environment[Env]
                                             with PredictableEnvironment[Env]
                                             with MutableEnvironment[Env]]
  extends PredictingEnvironmentOverseer[Env] with MutableEnvironmentOverseer[Env]
{
  self: EnvironmentOverseer[Env] =>

  /**
   * a snapshot of mutable environment that have setter functions active and has
   */
  def mutableSnapshot(): CustomisableEnvironmentSnapshot[Env] with Env
}

trait PredictingMutableDeterministicEnvironmentOverseer[Env <: Environment[Env]
                                                         with Deterministic[Env]
                                                         with PredictableDeterministicEnvironment[Env]
                                                         with MutableEnvironment[Env]]
  extends PredictingMutableEnvironmentOverseer[Env]
{
  self: EnvironmentOverseer[Env] =>

  protected def predictOnSnapshot(a: Env#Action, snapshot: CustomisableEnvironmentSnapshot[Env] with Env) =
    env.effects(a)(snapshot).asInstanceOf[CustomisableEnvironmentSnapshot[Env] with Env]

  def predict(a: Env#Action): Env#Prediction = predictOnSnapshot(a, mutableSnapshot()).snapshot()
}

trait ForeseeingEnvironmentOverseer[Env <: Environment[Env] with ForeseeableEnvironment[Env]]{
  self: PredictingEnvironmentOverseer[Env] =>

  /**
   *
   * @param possibleActions previous actions => current snapshot => possible actions
   * @param includeShorter include action seqs of length less then foreseeing depth
   * @param excludeTurningBack exclude action seqs that pass twice same state
   */
  def foresee(depth: Int,
              possibleActions: (Seq[Env#Action]) => (EnvironmentSnapshot[Env]) => Set[Env#Action],
              includeShorter: Boolean,
              excludeTurningBack: Boolean): Map[Seq[Env#Action], Env#Prediction]
}

trait ForeseeingEnvironmentOverseerWithActor[Env <: Environment[Env] with ForeseeableEnvironment[Env]]
  extends ForeseeingEnvironmentOverseer[Env] with EnvironmentOverseerWithActor[Env]
{
  overseer: PredictingEnvironmentOverseer[Env] =>

  case class Foresee(possibleActions: EnvironmentSnapshot[Env] => Set[Env#Action], depth: Int) extends UUIDed
  case class ForeseeExtended(possibleActions: Seq[Env#Action] => EnvironmentSnapshot[Env] => Set[Env#Action],
                             depth: Int, includeShorter: Boolean, excludeTurningBack: Boolean) extends UUIDed
  case class Foresight(uuid: UUID, predictions: Map[Seq[Env#Action], Env#Prediction]) extends HasUUID

  def foreseeMaxDelay: FiniteDuration

  protected def foreseeingActorResponses: PartialFunction[Any, () => Any] = {
    case msg@ForeseeExtended(possibleActions, depth, includeShorter, excludeTurningBack) =>
      overseer.foresee(depth, possibleActions, includeShorter, excludeTurningBack).lifted
  }

  trait ForeseeableEnvironmentRefImpl extends ForeseeableEnvironmentRef[Env]{
    def foresee(depth: Int,
                possibleActions: (Seq[Env#Action]) => (EnvironmentSnapshot[Env]) => Set[Env#Action],
                includeShorter: Boolean,
                excludeTurningBack: Boolean): Map[Seq[Env#Action], Env#Prediction] =
      overseer.foresee(depth, possibleActions, includeShorter, excludeTurningBack)

    def asyncForesee(depth: Int,
                     possibleActions: (Seq[Env#Action]) => (EnvironmentSnapshot[Env]) => Set[Env#Action],
                     includeShorter: Boolean,
                     excludeTurningBack: Boolean): Future[Map[Seq[Env#Action], Env#Prediction]] =
      ForeseeExtended(possibleActions, depth, includeShorter, excludeTurningBack) |> {
        msg =>
          (overseer.actorRef ? msg)(foreseeMaxDelay)
            .mapTo[Foresight].havingSameUUID(msg).map(_.predictions)
      }

  }
}

trait ForeseeingMutableDeterministicEnvironmentOverseer[Env <: Environment[Env]
                                                          with Deterministic[Env]
                                                          with PredictableDeterministicEnvironment[Env]
                                                          with ForeseeableEnvironment[Env]
                                                          with MutableEnvironment[Env]]
  extends ForeseeingEnvironmentOverseer[Env] with Debugging
{
  self: PredictingMutableDeterministicEnvironmentOverseer[Env] =>

  def foresee(maxDepth: Int,
              possibleActions: (Seq[Env#Action]) => (EnvironmentSnapshot[Env]) => Set[Env#Action],
              includeShorter: Boolean,
              excludeTurningBack: Boolean): Map[Seq[Env#Action], Env#Prediction] = {
    debugLog(s"foreseeing for $maxDepth possible actions ahead")

    def getPrediction(act: Env#Action, snapshot: CustomisableEnvironmentSnapshot[Env] with Env) = predictOnSnapshot(act, snapshot.copy())

    def getPossActions(previous: Seq[Env#Action], mSnapshot: CustomisableEnvironmentSnapshot[Env] with Env) = possibleActions(previous)(mSnapshot.snapshot())

    def rec(depth: Int,
            snapshot: CustomisableEnvironmentSnapshot[Env] with Env,
            previousActsWithPredictions: Seq[(Env#Action,  EnvironmentSnapshot[Env])],
            currentAction: Env#Action): Set[(Seq[Env#Action], Env#Prediction)] = {
      val previousActs = previousActsWithPredictions.map(_._1)
      debugLog(s"foreseeing recursively: depth=$depth, previous actions=$previousActs, current action=$currentAction")
      val currentActions =  previousActs :+ currentAction
      val prediction = getPrediction(currentAction, snapshot)
      val currentSnapshot = snapshot.snapshot()
      def nextLengthPossActions = getPossActions(currentActions, snapshot) // todo: slow
        .flatMap(rec(depth + 1, prediction, previousActsWithPredictions :+ (currentAction -> currentSnapshot), _))
      def currentActionsWithPrediction = (previousActs :+ currentAction) -> prediction.snapshot()

      if(excludeTurningBack && previousActsWithPredictions.exists(_._2 == currentSnapshot)) return Set()

      if(depth == maxDepth) Set(currentActionsWithPrediction)
      else if(includeShorter) Set(currentActionsWithPrediction) ++ nextLengthPossActions
      else nextLengthPossActions
    }

    val initSnapshot = mutableSnapshot()
    val poss = getPossActions(Nil, initSnapshot)
    val res = poss.flatMap(rec(1, initSnapshot, Nil, _)).toMap
    res
  }


}