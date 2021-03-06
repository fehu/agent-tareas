package feh.tec.agent.game

import feh.tec.agent.conf.AppConfig
import feh.tec.util.SideEffect

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

trait GenericActorGameCoordinator[Game <: GenericGame,
                                  Env <: GenericGameEnvironment[Game, Env]]
  extends GenericGameCoordinator[Game, Env] with GameCoordinatorWithActor[Game, Env]
{
  override def affect(act: Env#Action): SideEffect[Env] = SideEffect(env) // do nothing or register choice, depending on "lifetimeCycle" implementation
}

abstract class GenericMutableActorGameCoordinator[Game <: GenericGame,
                                                  Env <: GenericMutableGameEnvironment[Game, Env]]
  extends GenericMutableGameCoordinator[Game, Env] with GenericActorGameCoordinator[Game, Env]
{
  protected def config: AppConfig

  protected implicit def executionContext = config.executionContext
  protected def scheduler = config.scheduler
  def defaultBlockingTimeout = config.defaultBlockingTimeout
  def defaultFutureTimeout = config.defaultFutureTimeout
  def awaitEndOfTurnTimeout = config.awaitEndOfTurnTimeout
  def actorSystem = config.actorSystem
}


object GenericMutableActorGameCoordinator{
  import AbstractGenericGame._
  import AbstractGenericGameEnvironment._

  class Coordinator2[Game <: Game2, Env <: Environment2[Game, Env] with GenericMutableGameEnvironment[Game, Env]](
      override val currentEnvironment: Env
    )(implicit protected val config: AppConfig)
    extends GenericMutableActorGameCoordinator[Game, Env]
  {
    def ref: Env#Ref = new GameRefBaseImpl{}
  }
}