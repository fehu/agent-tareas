package feh.tec.visual

import feh.tec.agent.game.DeterministicMutableGameAppEnvironment2
import scala.xml.NodeSeq
import feh.tec.agent.conf.AppConfig
import feh.tec.visual.GenericGameSwingFrame.App2

object DeterministicMutableGameAppEnvironment {
  object AppEnv2{
    def apply(_game: AppEnv2#Game,
              _description: NodeSeq = <html>No description provided</html>)
             (implicit _config: AppConfig) = new AppEnv2 {
      def game = _game
      implicit def config = _config
      def description = _description
    }
  }

  trait AppEnv2 extends DeterministicMutableGameAppEnvironment2{
    type App = App2[Game, Env, Coord, Agent, Exec]
    lazy val app = new App2[Game, Env, Coord, Agent, Exec](game, environment, coordinator, executor, playersMap.toMap, description)
  }
}
