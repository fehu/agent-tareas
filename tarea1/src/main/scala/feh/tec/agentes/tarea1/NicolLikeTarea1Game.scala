package feh.tec.agentes.tarea1

import feh.tec.visual._
import feh.tec.visual.api._
import nicol.{Init, Game, Scene}
import feh.tec.util.{ImplicitLift, LiftWrapper}
import ImplicitLift._
import nicol.Init
import feh.tec.visual.api.Layout
import nicol.Init
import feh.tec.visual.api.LayoutElem
import feh.tec.visual.api.BasicDrawEnvironmentSettings
import feh.tec.agentes.tarea1.{Map => TMap}
import feh.tec.agentes.tarea1.Tarea1.Agents.MyDummyAgent
import feh.tec.agentes.tarea1.Agent.Measure

trait Tarea1Types extends NicolLikeTileGame{
  import Agent._

  type TCoord = Position
  type Tile = SqTile
  type Map = TMap
  type EaselTpe = NicolLike2DEasel
  type DrawSettings = BasicDrawEnvironmentSettings
}

case class AgentRef(ag: MyDummyAgent[Nothing])

class NicolLikeTarea1Game(env: Environment, agRef: AgentRef) extends NicolLikeTileGame with Tarea1Types{
  protected val pauseEndApi: PauseEndGameInnerApi = new PauseEndGameInnerApi{
    lazy val pauseScene = new Tarea1PauseScene(render())

    def pauseScene(resume: Scene): Scene = {
      Tarea1LastSceneKeeper.scene = resume
      pauseScene
    }

    def endScene = Tarea1EndScene
  }

  protected def newGame(scene: => Scene): Game = new Tarea1Game(render(), new FinishedScene(render()))

  def gameExecutionFinished(): Boolean = Tarea1App.isFinished

  protected def initScene: Scene = Init("Tarea1 v. 0.01", 800, 600)
  protected def baseScene: Scene = new NicolLikeBasicScene(render().lifted, pauseEndApi.endScene.lifted, gameExecutionFinished().lifted, pauseEndApi.pauseScene)

  implicit def mapRenderer: Renderer[Map, NicolLike2DEasel] = Lwjgl.createMapRenderer

  def gameLayout: Layout[NicolLike2DEasel] = Layout(
    LayoutElem[Map, NicolLike2DEasel](env, (0, 0F)) :: Nil
  )

  implicit def easelCoordinateOps = NicoleLike2dEaselCoordinateOps

  @deprecated("render layout with map")
  def renderMap(ops: EaselTpe#MDrawOptions)(implicit easel: EaselTpe): Unit = ???

  def mapDrawOps: EaselTpe#MDrawOptions = Lwjgl.mapDrawOps

  def drawEnvSettings: DrawSettings = ??? //BasicDrawEnvironmentSettings("Tarea1", 600, 800, false)

  implicit lazy val easel: EaselTpe = new NicolLike2DEasel

  def render(l: Layout[EaselTpe])(implicit easel: EaselTpe): Unit = ???

}


class Tarea1CriteriaValueRenderer extends CriteriaValueRenderer[Agent.Position, Agent.EnvState, Agent.EnvGlobal, Agent.Action, Agent.Env, Agent.Measure, Agent.Easel]{

  override def render(t: Measure#CriteriaValue)(implicit easel: Agent.Easel): Unit = {
    easel.drawRect(0F -> -300F, 300F -> 0F)
    super.render(t)(easel)

  }
  def render(t: Measure#CriterionValue)(implicit easel: Agent.Easel): Unit = ???
}