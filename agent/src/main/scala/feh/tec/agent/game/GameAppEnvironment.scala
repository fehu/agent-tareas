package feh.tec.agent.game

import scala.xml.NodeSeq
import feh.tec.agent.game.GenericMutableActorGameCoordinator.Coordinator2
import feh.tec.agent.game.GenericDeterministicGame.Game2
import scala.collection.mutable
import feh.tec.agent.game.GenericPlayer._
import feh.tec.agent.game.DeterministicMutableGenericGameEnvironment.Env2
import feh.tec.agent.conf.AppConfig
import feh.tec.visual.api.{StopNotifications, AppBasicControlApi}

trait GameAppEnvironment{
  type App <: AppBasicControlApi with StopNotifications
  type Game <: GenericGame
  
  implicit def config: AppConfig
  def game: Game
  def description: NodeSeq
  def app: App
}

trait DeterministicMutableGameAppEnvironment2 extends GameAppEnvironment{
  type Game = GenericDeterministicGame.Game2
  type Env = DeterministicMutableGenericGameEnvironment.Env2[Game]
  type Coord = GenericMutableActorGameCoordinator.Coordinator2[Game, Env]
  type Agent = PlayerAgent[Game, Env] with PlayerAgent.Resettable[Game, Env] with PlayerAgent.RandomBehaviour[Game, Env]
  type Exec = GenericExecutor[Game, Env]

  /**
   * player should be created by this app environment
   */
  def createPlayer(f: (this.type => Agent)*) = {
    f.map(_(this).ensuring(playersMap contains _))
    this
  }

  /**
   * for players created outside of this environment
   */
  def attachPlayer(ag: Agent) = {
    register(ag)
    this
  }

  lazy val environment: Env = new Env2(game)
  lazy val coordinator: Coord = new Coordinator2[Game2, Env2[Game2]](environment)
  lazy val executor: Exec = new GenericExecutor[Game2, Env2[Game2]](config.awaitEndOfTurnTimeout)(config.executionContext)

  protected val playersMap = mutable.Map.empty[Agent, Game#Player]

  def dummyPlayer(sel: Game2 => Game2#Player): Agent = {
    val p = sel(game)
    val pl = new GenericPlayer[Game2, Env2[Game2]](p, executor, coordinator.ref)
      with DummyBestStrategyChooser2[Game2, Env2[Game2]]
      with RandomBehaviour[Game2, Env2[Game2]]
    register(pl)
  }

  def statisticEnforcedPlayer(sel: Game2 => Game2#Player): Agent = {
    val p = sel(game)
    val pl = new GenericPlayer[Game2, Env2[Game2]](p, executor, coordinator.ref)
      with DummyBestStrategyChooser2[Game2, Env2[Game2]]
      with SimpleExpectedUtility2[Game2, Env2[Game2]]
      with RandomBehaviour[Game2, Env2[Game2]]
    register(pl)
  }

  protected def register(ag: Agent) = {
    assert(!playersMap.contains(ag), s"Agent for player ${ag.player} has already been registered")
    playersMap += ag -> ag.player
    ag
  }
}
