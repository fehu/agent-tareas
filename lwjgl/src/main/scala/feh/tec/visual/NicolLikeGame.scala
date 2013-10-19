package feh.tec.visual

import nicol._
import feh.tec.visual.api.TileGame
import feh.tec.util._
import scala.Some
import scala.Some
import nicol.input.Key._

trait NicolLikeGame {
  def update(): Option[Scene]
}

trait PauseEndGameInnerApi{
  def pauseScene(resume: Scene): Scene
  def endScene: Scene
}

trait NicolLikeTileGame extends NicolLikeGame with TileGame{
  def update(): Option[Scene] = ???

  def prepareDrawEnvironment(ops: DrawSettings): Unit = ???

  protected def pauseEndApi: PauseEndGameInnerApi

//  private var _superRun: () => Unit = _
  protected def superRun() = super.run() //Option(_superRun).get

  protected def newGame(scene: => Scene/*, pauseScene: Scene, endScene: Scene*/): Game

  def gameExecutionFinished(): Boolean

  protected lazy val game: Game = newGame(initScene >> baseScene)

  protected def initScene: Scene
  protected def render(): Unit = render(gameLayout)
  protected def baseScene: Scene

  override def run(): Unit = {
    game.start
  }
}

class NicolLikeBasicScene(render: () => Unit, finishedScene: Lifted[Scene], finished_? : () => Boolean, pauseScene: Scene => Scene)
                          (implicit easel: NicolLike2DEasel) 
  extends LoopScene with SyncableScene with ShowFPS
{
  def update: Option[Scene] = {
    sync
    showFPS

    render()

    if(finished_?()) finishedScene()
    else keyEvent {
      e =>
        e released {
          case _ =>
        }
        e pressed {
          case "escape" => finishedScene()
          case "space" => pauseScene(this)
//            Tarea1LastSceneKeeper.scene = this
//            prePauseScene(this)
//            pauseScene()
            /*Tarea1PauseSceneKeeper.sceneOpt.getOrElse{
              val p = new Tarea1PauseScene(renderMap)
              Tarea1PauseSceneKeeper.scene = p
              p
            }*/
        }
    }
  }
}