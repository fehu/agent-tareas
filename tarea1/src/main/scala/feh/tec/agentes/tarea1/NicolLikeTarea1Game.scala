package feh.tec.agentes.tarea1

import feh.tec.visual._
import feh.tec.visual.api._
import nicol.{Game, Scene, Init}
import feh.tec.util.{ImplicitLift, LiftWrapper}
import ImplicitLift._
import feh.tec.agentes.tarea1.Tarea1.Agents.MyDummyAgent
import feh.tec.visual.api.BasicDrawEnvironmentSettings
import feh.tec.visual.api.Layout
import feh.tec.visual.api.LayoutElem
import feh.tec.visual.render.CriteriaReasonedDecisionRenderer
import java.awt.Color

trait Tarea1Types extends NicolLikeTileGame{
  type EaselTpe = NicolLike2DEasel
  type DrawSettings = BasicDrawEnvironmentSettings
}

case class AgentRef(ag: MyDummyAgent[Nothing])
object AgentRef{
  def create(ag: MyDummyAgent[_]): AgentRef = new AgentRef(ag.asInstanceOf[MyDummyAgent[Nothing]])
  implicit def AgentRefToRef(ref: AgentRef) = ref.ag
}

class NicolLikeTarea1Game(val env: Environment, val agRef: AgentRef) extends NicolLikeTileGame with Tarea1Types{
  protected val pauseEndApi: PauseEndGameInnerApi = new PauseEndGameInnerApi{
    lazy val pauseScene = new Tarea1PauseScene(render())

    def pauseScene(resume: Scene): Scene = {
      Tarea1LastSceneKeeper.scene = resume
      pauseScene
    }

    def endScene = Tarea1EndScene
  }

  protected def newGame(scene: => Scene): Game = new Game(scene) {}

  def gameExecutionFinished(): Boolean = Tarea1App.isFinished

  def finishedScene = new FinishedScene(render)

  protected def initScene: Scene = Init("Tarea1 v. 0.01", 900, 600)
  protected def baseScene: Scene = new NicolLikeBasicScene(render().lifted, pauseEndApi.endScene.lifted, finishedScene.lifted, gameExecutionFinished().lifted, pauseEndApi.pauseScene)

  val criteriaRendererScheme = CriteriaReasonedDecisionRenderer.Scheme(
    criterionFont = "arial", criterionColor = Color.white,
    beforeCriteriaFont = "arial", beforeCriteriaColor = Color.gray,
    afterCriteriaFont = "arial", afterCriteriaColor = Color.green,
    frameColor = Some(Color.blue), sepLineColor = Some(Color.white)
  )

  implicit def mapRenderer: Renderer[Map, NicolLike2DEasel] = Lwjgl.createMapRenderer
  implicit def criteriaValueRenderer: Renderer[agRef.ag.ActionExplanation, NicolLike2DEasel] = new CriteriaReasonedDecisionRenderer(criteriaRendererScheme)

  val gameLayout: Layout[NicolLike2DEasel] = Layout(List[LiftedOptionLayoutElem[_, NicolLike2DEasel]](
    LayoutElem[Map, NicolLike2DEasel](env, (0, 0)),
    LiftedOptionLayoutElem[agRef.ag.ActionExplanation, NicolLike2DEasel](
      () => agRef.currentDecisionExplanation,
      (530, 0)
    )
  )
  )

  implicit def easelCoordinateOps = NicoleLike2dEaselCoordinateOps
  implicit lazy val easel: EaselTpe = new NicolLike2DEasel
  def render(l: Layout[EaselTpe])(implicit easel: EaselTpe): Unit = l.render
}
