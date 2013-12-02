package feh.tec.visual

import nicol.{Game => NGame, _}
import feh.tec.visual.api.{AppBasicControlApi, AgentApp}
import feh.tec.util._
import nicol.input.Key._
import org.lwjgl.opengl.Display

trait NicolBasedApp {
  def game: NGame
}

trait NicolBasedAgentAppBasicControl extends AgentApp with NicolBasedApp with AppBasicControlApi{
  def prepareDrawEnvironment(ops: DrawSettings): Unit = ???

  protected def pauseEndApi: PauseEndAppInnerApi

  protected def newGame(scene: => Scene/*, pauseScene: Scene, endScene: Scene*/): NGame

  def appExecutionFinished(): Boolean

  lazy val game: NGame = newGame(initScene >> baseScene)

  def initScene: Scene
  def baseScene: Scene
  protected def render(): Unit = render(layout)

  def start(): Unit = {
    isRunning = true
    game.start
  }
  def stop(): Unit = {
    isRunning = false
    game.stop
  }
  var isRunning = false
}

class NicolLikeBasicScene(render: () => Unit, exitScene: Lifted[Scene], terminatedScene: Lifted[Scene], terminated_? : () => Boolean, pauseScene: Scene => Scene)
                          (implicit easel: NicolLike2DEasel)
  extends LoopScene with SyncableScene with ShowFPS
{
  def update: Option[Scene] = {
    sync
    showFPS

    render()

    if(terminated_?()) terminatedScene()
    else if(Display.isCloseRequested) exitScene()
    else keyEvent {
      e =>
        e released {
          case _ =>
        }
        e pressed {
          case "escape" => exitScene()
          case "space" => pauseScene(this)
        }
    }
  }
}