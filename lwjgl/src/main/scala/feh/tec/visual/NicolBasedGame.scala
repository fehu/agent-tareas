package feh.tec.visual

import nicol.{Game => NGame, _}
import feh.tec.visual.api.{GameBasicControlApi, Game}
import feh.tec.util._
import nicol.input.Key._

trait NicolBasedGame {
  def game: NGame
}

trait NicolBasedGameBasicControl extends Game with NicolBasedGame with GameBasicControlApi{
  def prepareDrawEnvironment(ops: DrawSettings): Unit = ???

  protected def pauseEndApi: PauseEndGameInnerApi

  protected def newGame(scene: => Scene/*, pauseScene: Scene, endScene: Scene*/): NGame

  def gameExecutionFinished(): Boolean

  lazy val game: NGame = newGame(initScene >> baseScene)

  protected def initScene: Scene
  protected def render(): Unit = render(gameLayout)
  protected def baseScene: Scene

  def start(): Unit = game.start
  def stop(): Unit = game.stop
}

class NicolLikeBasicScene(render: () => Unit, finishedScene: Lifted[Scene], terminatedScene: Lifted[Scene], terminated_? : () => Boolean, pauseScene: Scene => Scene)
                          (implicit easel: NicolLike2DEasel)
  extends LoopScene with SyncableScene with ShowFPS
{
  def update: Option[Scene] = {
    sync
    showFPS

    render()

    if(terminated_?()) terminatedScene()
    else keyEvent {
      e =>
        e released {
          case _ =>
        }
        e pressed {
          case "escape" => finishedScene()
          case "space" => pauseScene(this)
        }
    }
  }
}