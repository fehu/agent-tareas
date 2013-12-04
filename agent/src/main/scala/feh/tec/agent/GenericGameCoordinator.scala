package feh.tec.agent

import feh.tec.util.SideEffect
import akka.actor.{Scheduler, ActorSystem}
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.ExecutionContext

abstract class GenericGameCoordinator[Game <: GenericGame, Env <: GenericGameEnvironment[Game, Env]]
  extends GameCoordinator[Game, Env]
{

}

object GenericAbstractGameCoordinator{
  import AbstractGenericGame._
  import AbstractGenericGameEnvironment._

  abstract class Coordinator2[Game <: Game2, Env <: Environment2[Game, Env]] extends GenericGameCoordinator[Game, Env]
}

trait GenericMutableGameCoordinator[Game <: GenericGame, Env <: GenericMutableGameEnvironment[Game, Env]]
  extends GenericGameCoordinator[Game, Env] with MutableGameCoordinator[Game, Env]{

}

trait GenericActorGameCoordinator[Game <: GenericGame, Env <: GenericGameEnvironment[Game, Env]]
  extends GenericGameCoordinator[Game, Env] with GameCoordinatorWithActor[Game, Env]

object GenericMutableActorGameCoordinator{
  import AbstractGenericGame._
  import AbstractGenericGameEnvironment._

  case class Config(actorSystem: ActorSystem,
                    executionContext: ExecutionContext,
                    scheduler: Scheduler,
                    defaultBlockingTimeout: Int,
                    defaultFutureTimeout: Int,
                    awaitEndOfTurnTimeout: FiniteDuration)

  abstract class GenericMutableActorGameCoordinator[Game <: GenericGame, Env <: GenericMutableGameEnvironment[Game, Env]]
    extends GenericMutableGameCoordinator[Game, Env] with GenericActorGameCoordinator[Game, Env]
  {
    protected def config: Config

    protected implicit def executionContext = config.executionContext
    protected def scheduler = config.scheduler
    def defaultBlockingTimeout = config.defaultBlockingTimeout
    def defaultFutureTimeout = config.defaultFutureTimeout
    def awaitEndOfTurnTimeout = config.awaitEndOfTurnTimeout
    def actorSystem = config.actorSystem
  }

  class Coordinator2[Game <: Game2, Env <: Environment2[Game, Env] with GenericMutableGameEnvironment[Game, Env]](
      override val currentEnvironment: Env
    )(implicit protected val config: Config)
    extends GenericMutableActorGameCoordinator[Game, Env]
  {
    def ref: Env#Ref = new GameRefBaseImpl{}
  }
}