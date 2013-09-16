package feh.tec.agent

import akka.actor.{ActorRef, Scheduler, ActorSystem, Actor}
import scala.reflect.runtime.universe._
import scala.concurrent.{ExecutionContext, Future}
import java.util.{UUID, Calendar}
import akka.pattern._
import feh.tec.util.{HasUUID, UUIDed, SideEffect, ScopedState}
import akka.util.Timeout
import scala.reflect._
import scala.concurrent.duration.FiniteDuration
import feh.tec.util.PipeWrapper
import scala.concurrent.duration._
import feh.tec.util._

/**
 *  manages the environment, hides actual environment change implementation
 *  produces environment references
 */
trait EnvironmentOverseer[Coordinate, State, Global, Action <: AbstractAction, Env <: Environment[Coordinate, State, Global, Action, Env]] {
  def currentEnvironment: Env
  def env = currentEnvironment

  protected[agent] def updateEnvironment(f: Env => Env): SideEffect[Env]

  def snapshot: EnvironmentSnapshot[Coordinate, State, Global, Action, Env]
  def ref: Env#Ref

  protected def affect(a: Action): SideEffect[Env]
}

protected[agent] object EnvironmentOverseerWithActor{
  sealed trait Message
  sealed class MessageResponse[R](val response: R, val ttag: List[TypeTag[_]]) extends Message

  object Get{
    case object GlobalState extends Message
    case class StateOf[Coordinate](c: Coordinate)(implicit val ttag: TypeTag[Coordinate]) extends Message
    case object VisibleStates extends Message
    case object Snapshot extends Message
  }

  case class Act[Action <: AbstractAction](act: Action)(implicit val ttag: TypeTag[Action]) extends Message

  object Response{
    case class GlobalState[Global](global: Global)(implicit ttag: TypeTag[Global]) extends MessageResponse(global, ttag :: Nil)
    case class StateOf[Coordinate, State](c: Coordinate, stateOpt: Option[State])
                                         (implicit ttag1: TypeTag[Coordinate], ttag2: TypeTag[State])
      extends MessageResponse(c -> stateOpt, ttag1 :: ttag2 :: Nil)
    case class VisibleStates[Coordinate, State](states: Map[Coordinate, State])
                                               (implicit ttag1: TypeTag[Coordinate], ttag2: TypeTag[State])
      extends MessageResponse(states, typeTag[Map[_, _]] :: ttag1 :: ttag2 :: Nil)
    case class ActionApplied[Action <: AbstractAction](a: Action)(implicit ttag: TypeTag[Action]) extends MessageResponse(a, ttag :: Nil)
    case class Snapshot[Coordinate, State, Global, Action <: AbstractAction, Env <: Environment[Coordinate, State, Global, Action, Env]]
      (snapshot: EnvironmentSnapshot[Coordinate, State, Global, Action, Env], time: Long)
      (implicit ttag: TypeTag[Env]) extends MessageResponse(snapshot -> time, ttag :: Nil)
  }
}

trait EnvironmentOverseerWithActor[Coordinate, State, Global, Action <: AbstractAction, Env <: Environment[Coordinate, State, Global, Action, Env]]
  extends EnvironmentOverseer[Coordinate, State, Global, Action, Env]
{
  overseer =>

  def actorRef: ActorRef

  import EnvironmentOverseerWithActor._
  lazy val tags = env.tags
  import tags._


  protected def affect(act: Action): SideEffect[Env] = updateEnvironment(_.affected(act).execute)


  protected def baseActorResponses: PartialFunction[Any, () => Unit] = {
    case Get.GlobalState => Response.GlobalState(env.globalState).liftUnit
    case g@Get.StateOf(c) if g.ttag.tpe =:= typeOf[Coordinate] => Response.StateOf(c, env.stateOf(c.asInstanceOf[Coordinate])).liftUnit
    case Get.VisibleStates => Response.VisibleStates(env.visibleStates).liftUnit
    case Get.Snapshot => Response.Snapshot(snapshot, Calendar.getInstance().getTimeInMillis).liftUnit
    case a@Act(act) if a.ttag.tpe <:< typeOf[Action] =>
      affect(act.asInstanceOf[Action]).execute
      Response.ActionApplied(act).liftUnit
  }

  protected implicit def executionContext: ExecutionContext
  protected def scheduler: Scheduler

  def defaultBlockingTimeout: Int
  def defaultFutureTimeout: Int

  /*
   * todo: refactor this reflection using type filtering madness
   */
  trait BaseEnvironmentRef extends EnvironmentRef[Coordinate, State, Global, Action, Env]
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
      def visibleStates: Map[Coordinate, State] = overseer.env.visibleStates
      def snapshot: Env with EnvironmentSnapshot[Coordinate, State, Global, Action, Env] =
        overseer.snapshot.asInstanceOf[Env with EnvironmentSnapshot[Coordinate, State, Global, Action, Env]] // todo: casting
      def stateOf(c: Coordinate): Option[State] = overseer.env.stateOf(c)
      def globalState: Global = overseer.env.globalState
      def affect(act: Action): SideEffect[Env#Ref] = overseer.affect(act).more(overseer.ref).flatExec
    }

    lazy val async: AsyncApi = new AsyncApi{
        def withTimeout[R](t: Int)(r: => R): R = futureTimeoutScope.doWith(t)(r)

        implicit def timeout = Timeout(futureTimeoutScope.get)

        def globalState: Future[Global] = (overseer.actorRef ? Get.GlobalState)
          .mapTo[Response.GlobalState[_]].withFilter(_.ttag.head.tpe <:< typeOf[Global]).map(_.global.asInstanceOf[Global])

        def stateOf(c: Coordinate) = (overseer.actorRef ? Get.StateOf(c))
          .mapTo[Response.StateOf[_, _]]
          .withFilter(_.c == c)
          .withFilter(_.ttag match { case _ :: tt2 :: Nil => tt2.tpe <:< typeOf[State] })
          .map(_.stateOpt.asInstanceOf[Option[State]])

        def affect(act: Action) = (overseer.actorRef ? Act(act))
          .mapTo[Response.ActionApplied[_]]
          .withFilter(_.a == Act)
          .map(_ => SideEffect(envRef))

        def visibleStates = (overseer.actorRef ? Get.VisibleStates)
          .mapTo[Response.VisibleStates[_, _]]
          .withFilter(_.ttag match { case tt0 :: tt1 :: tt2 :: Nil =>
            tt1.tpe =:= typeOf[Coordinate] &&
            tt2.tpe <:< typeOf[State]
          })
          .map(_.states.asInstanceOf[Map[Coordinate, State]])

        def snapshot = (overseer.actorRef ? Get.Snapshot)
          .mapTo[Response.Snapshot[_, _, _, _, _]]
          .withFilter(_.ttag match { case _ :: _ :: _ :: _ :: envTag :: Nil => envTag.tpe =:= typeOf[Env] })
          .map(_.snapshot.asInstanceOf[Env with EnvironmentSnapshot[Coordinate, State, Global, Action, Env]])
      }
  }
}

trait EnvironmentOverseerImplementation[Coordinate, State, Global, Action <: AbstractAction, Env <: Environment[Coordinate, State, Global, Action, Env]]{
  self: EnvironmentOverseer[Coordinate, State, Global, Action, Env] =>
}

trait MutableEnvironmentOverseer[Coordinate, State, Global, Action <: AbstractAction,
                                 Env <: Environment[Coordinate, State, Global, Action, Env]
                                   with MutableEnvironment[Coordinate, State, Global, Action, Env]]
  extends EnvironmentOverseerImplementation[Coordinate, State, Global, Action, Env]
{
  self: EnvironmentOverseer[Coordinate, State, Global, Action, Env] =>

  override val currentEnvironment: Env

  protected[agent] def updateEnvironment(f: (Env) => Env): SideEffect[Env] = SideEffect(f(env))
}

trait ImmutableEnvironmentOverseer[Coordinate, State, Global, Action <: AbstractAction,
                                   Env <: Environment[Coordinate, State, Global, Action, Env]
                                     with ImmutableEnvironment[Coordinate, State, Global, Action, Env]]
  extends EnvironmentOverseerImplementation[Coordinate, State, Global, Action, Env]
{
  self: EnvironmentOverseer[Coordinate, State, Global, Action, Env] =>

  def initialEnvironment: Env
  protected var environment = initialEnvironment
  def currentEnvironment: Env = environment
  protected[agent] def updateEnvironment(f: (Env) => Env): SideEffect[Env] = SideEffect{
    val e = f(env)
    environment = e
    e
  }
}

trait PredictingEnvironmentOverseer[Coordinate, State, Global, Action <: AbstractAction,
                                    Env <: Environment[Coordinate, State, Global, Action, Env]
                                      with PredictableEnvironment[Coordinate, State, Global, Action, Env]]{
  self: EnvironmentOverseer[Coordinate, State, Global, Action, Env] =>

  def predict(a: Action): Env#Prediction

}

trait PredictingEnvironmentOverseerWithActor[Coordinate, State, Global, Action <: AbstractAction,
                                         Env <: Environment[Coordinate, State, Global, Action, Env]
                                           with PredictableEnvironment[Coordinate, State, Global, Action, Env]]
  extends PredictingEnvironmentOverseer[Coordinate, State, Global, Action, Env]
{
  overseer: EnvironmentOverseerWithActor[Coordinate, State, Global, Action, Env] =>

  case class Predict(a: Action) extends UUIDed
  case class Prediction(uuid: UUID, p: Env#Prediction) extends HasUUID

  def predictMaxDelay: FiniteDuration

  protected def predictingActorResponses: PartialFunction[Any, () => Unit] = {
    case msg@Predict(a) => Prediction(msg.uuid, predict(a)).liftUnit
  }

  trait PredictableEnvironmentRefImpl extends PredictableEnvironmentRef[Coordinate, State, Global, Action, Env]{
    def predict(a: Action): Env#Prediction = overseer predict a

    def asyncPredict(a: Action): Future[Env#Prediction] = Predict(a) |> { msg =>
      (overseer.actorRef ? msg)(predictMaxDelay)
        .mapTo[Prediction].havingSameUUID(msg).map(_.p)
    }
  }
}

trait PredictingMutableEnvironmentOverseer[Coordinate, State, Global, Action <: AbstractAction,
                                           Env <: Environment[Coordinate, State, Global, Action, Env]
                                             with PredictableEnvironment[Coordinate, State, Global, Action, Env]
                                             with MutableEnvironment[Coordinate, State, Global, Action, Env]]
  extends PredictingEnvironmentOverseer[Coordinate, State, Global, Action, Env] with MutableEnvironmentOverseer[Coordinate, State, Global, Action, Env]
{
  self: EnvironmentOverseer[Coordinate, State, Global, Action, Env] =>

  /**
   * a snapshot of mutable environment that have setter functions active and has
   */
  def mutableSnapshot(): CustomisableEnvironmentSnapshot[Coordinate, State, Global, Action, Env] with Env
}

trait PredictingMutableDeterministicEnvironmentOverseer[Coordinate, State, Global, Action <: AbstractAction,
                                                       Env <: Environment[Coordinate, State, Global, Action, Env]
                                                         with PredictableDeterministicEnvironment[Coordinate, State, Global, Action, Env]
                                                         with MutableEnvironment[Coordinate, State, Global, Action, Env]]
  extends PredictingMutableEnvironmentOverseer[Coordinate, State, Global, Action, Env]
{
  self: EnvironmentOverseer[Coordinate, State, Global, Action, Env] =>

  protected def predictOnSnapshot(a: Action, snapshot: CustomisableEnvironmentSnapshot[Coordinate, State, Global, Action, Env] with Env) =
    env.effects(a)(snapshot).asInstanceOf[CustomisableEnvironmentSnapshot[Coordinate, State, Global, Action, Env] with Env]

  def predict(a: Action): Env#Prediction = predictOnSnapshot(a, mutableSnapshot()).snapshot()
}

trait ForeseeingEnvironmentOverseer[Coordinate, State, Global, Action <: AbstractAction,
                                   Env <: Environment[Coordinate, State, Global, Action, Env]
                                     with ForeseeableEnvironment[Coordinate, State, Global, Action, Env]]{
  self: PredictingEnvironmentOverseer[Coordinate, State, Global, Action, Env] =>

  def foresee(depth: Int, possibleActions: EnvironmentSnapshot[Coordinate, State, Global, Action, Env] => Set[Action]): Map[Seq[Action], Env#Prediction]
}

trait ForeseeingEnvironmentOverseerWithActor[Coordinate, State, Global, Action <: AbstractAction,
                                             Env <: Environment[Coordinate, State, Global, Action, Env]
                                               with ForeseeableEnvironment[Coordinate, State, Global, Action, Env]]
  extends ForeseeingEnvironmentOverseer[Coordinate, State, Global, Action, Env] with EnvironmentOverseerWithActor[Coordinate, State, Global, Action, Env]
{
  overseer: PredictingEnvironmentOverseer[Coordinate, State, Global, Action, Env] =>

  case class Foresee(possibleActions: EnvironmentSnapshot[Coordinate, State, Global, Action, Env] => Set[Action], depth: Int) extends UUIDed
  case class Foresight(uuid: UUID, predictions: Map[Seq[Action], Env#Prediction]) extends HasUUID

  def foreseeMaxDelay: FiniteDuration

  protected def foreseeingActorResponses: PartialFunction[Any, () => Unit] = {
    case msg@Foresee(possibleActions, depth) => Foresight(msg.uuid, overseer.foresee(depth, possibleActions)).liftUnit
  }

  trait ForeseeableEnvironmentRefImpl extends ForeseeableEnvironmentRef[Coordinate, State, Global, Action, Env]{
    def foresee(depth: Int, possibleActions: EnvironmentSnapshot[Coordinate, State, Global, Action, Env] => Set[Action]): Map[Seq[Action], Env#Prediction] =
      overseer.foresee(depth, possibleActions)
    def asyncForesee(depth: Int, possibleActions: EnvironmentSnapshot[Coordinate, State, Global, Action, Env] => Set[Action]): Future[Map[Seq[Action], Env#Prediction]] =
      Foresee(possibleActions, depth) |> { msg =>
        (overseer.actorRef ? msg)(foreseeMaxDelay)
          .mapTo[Foresight].havingSameUUID(msg).map(_.predictions)
      }
  }
}

trait ForeseeingMutableDeterministicEnvironmentOverseer[Coordinate, State, Global, Action <: AbstractAction,
                                                        Env <: Environment[Coordinate, State, Global, Action, Env]
                                                          with PredictableDeterministicEnvironment[Coordinate, State, Global, Action, Env]
                                                          with ForeseeableEnvironment[Coordinate, State, Global, Action, Env]
                                                          with MutableEnvironment[Coordinate, State, Global, Action, Env]]
  extends ForeseeingEnvironmentOverseer[Coordinate, State, Global, Action, Env]
{
  self: PredictingMutableDeterministicEnvironmentOverseer[Coordinate, State, Global, Action, Env] =>

  def foresee(maxDepth: Int, possibleActions: EnvironmentSnapshot[Coordinate, State, Global, Action, Env] => Set[Action]): Map[Seq[Action], Env#Prediction] = {

    def getPrediction(act: Action, snapshot: CustomisableEnvironmentSnapshot[Coordinate, State, Global, Action, Env] with Env) =
      predictOnSnapshot(act, snapshot).copy()

    def getPossActions(snapshot: CustomisableEnvironmentSnapshot[Coordinate, State, Global, Action, Env] with Env)
      = possibleActions(snapshot.snapshot())

    def rec(depth: Int,
            snapshot: CustomisableEnvironmentSnapshot[Coordinate, State, Global, Action, Env] with Env,
            previousActs: Seq[Action],
            currentAction: Action): Set[(Seq[Action], Env#Prediction)] =
      if(depth == maxDepth) Set((previousActs :+ currentAction) -> getPrediction(currentAction, snapshot).snapshot())
      else getPossActions(snapshot).flatMap(rec(depth + 1, getPrediction(currentAction, snapshot), previousActs :+ currentAction, _))

    val initSnapshot = mutableSnapshot()
    val poss = getPossActions(initSnapshot)
    val res = poss.flatMap(rec(1, initSnapshot, Nil, _)).toMap
    res
  }

}