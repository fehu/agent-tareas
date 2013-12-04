package feh.tec.agent

import feh.tec.util.SideEffect

abstract class GenericGameEnvironment[Game <: GenericGame, Env <: GenericGameEnvironment[Game, Env]]
  extends GameEnvironment[Game, Env]
{
  self: Env =>

}

object AbstractGenericGameEnvironment{
  import AbstractGenericGame._
  
  
  abstract class Environment2[Game <: Game2, Env <: Environment2[Game, Env]] extends GenericGameEnvironment[Game, Env]{
    self: Env =>
    type Ref = GameRef[Game, Env]
  }
}

trait GenericMutableGameEnvironment[Game <: GenericGame,
  Env <: GenericGameEnvironment[Game, Env] with MutableGameEnvironmentImpl[Game, Env]]
    extends GenericGameEnvironment[Game, Env] with MutableGameEnvironmentImpl[Game, Env]
{
  self: Env =>
}

trait GenericDeterministicMutableGameEnvironment[Game <: GenericDeterministicGame,
    Env <: GenericMutableGameEnvironment[Game, Env] with DeterministicGameEnvironment[Game, Env]]
  extends GenericMutableGameEnvironment[Game, Env] with DeterministicGameEnvironment[Game, Env]
{
  self: Env =>
  override def affected(act: GameAction): SideEffect[Env] = super[GenericMutableGameEnvironment].affected(act)
}

object DeterministicMutableGenericGameEnvironment{
  import AbstractGenericGameEnvironment._
  import GenericDeterministicGame._

  case class Env2(game: Game2) extends Environment2[Game2, Env2] with GenericDeterministicMutableGameEnvironment[Game2, Env2]
}

