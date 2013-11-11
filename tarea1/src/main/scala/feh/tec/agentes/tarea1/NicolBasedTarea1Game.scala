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
import feh.tec.visual.render.{LwjglSquareMapRoutesRenderer, LwjglSquareMapOnMouseHighlightRenderer, LwjglSquareMapRenderer, CriteriaReasonedDecisionRenderer}
import java.awt.Color
import LwjglSquareMapRenderer.BuildTDrawOpsParams
import feh.tec.agent.AgentId
import feh.tec.map.visual.WorldVisualisationCalls

case class AgentRef(ag: MyDummyAgent[Nothing])
object AgentRef{
  def create(ag: MyDummyAgent[_]): AgentRef = new AgentRef(ag.asInstanceOf[MyDummyAgent[Nothing]])
  implicit def AgentRefToRef(ref: AgentRef) = ref.ag
}

trait AgentResolver{
  def byId(id: AgentId): Option[AgentRef]
}

class NicolBasedTarea1Game(val env: Environment, val agentId: AgentId)(implicit agResolver: AgentResolver) extends NicolBasedGameBasicControl{
  type EaselTpe = NicolLike2DEasel
  type DrawSettings = BasicDrawEnvironmentSettings

  protected val pauseEndApi: PauseEndGameInnerApi = new PauseEndGameInnerApi{
    lazy val pauseScene = new Tarea1PauseScene(render().lifted)

    def pauseScene(resume: Scene): Scene = {
      Tarea1App.ag.executionLoop.pause()
      Tarea1LastSceneKeeper.scene = resume
      pauseScene
    }

    def endScene = Tarea1EndScene
  }

  protected def newGame(scene: => Scene): Game = new Game(scene) {}

  def gameExecutionFinished(): Boolean = Tarea1App.isFinished

  def finishedScene = new FinishedScene(render)

  protected def initScene: Scene = Init("Agent Closing Hole With Plugs \t v. 0.2", 900, 600)
  protected def baseScene: Scene = new NicolLikeBasicScene(render().lifted, pauseEndApi.endScene.lifted, finishedScene.lifted, gameExecutionFinished().lifted, pauseEndApi.pauseScene)

  val criteriaRendererScheme = CriteriaReasonedDecisionRenderer.Scheme(
    criterionFont = "arial", criterionColor = Color.white,
    beforeCriteriaFont = "arial", beforeCriteriaColor = Color.gray,
    afterCriteriaFont = "arial", afterCriteriaColor = Color.green,
    frameColor = Some(Color.blue), sepLineColor = Some(Color.white)
  )

  lazy val agRef = agResolver.byId(agentId).get

  implicit val mapRenderer: Renderer[Map, NicolLike2DEasel] with WorldVisualisationCalls[SqTile, (Int, Int)] = NicolBasedTarea1Game.mapRenderer()

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

object NicolBasedTarea1Game{
  def mapRenderer(mapDrawConfig: NicolLike2DEasel#MDrawOptions = Tarea1App.visual.mapDrawConfig) =
    new LwjglSquareMapRenderer[Map, SqTile, NicolLike2DEasel](
      LwjglTileRenderer.create,
      mapDrawConfig
    ) with LwjglSquareMapOnMouseHighlightRenderer[Map, SqTile, NicolLike2DEasel]
            with LwjglSquareMapRoutesRenderer[Map, SqTile, NicolLike2DEasel]
    {
      def buildTDrawOps = ops => BasicSquareTileDrawOptions[NicolLike2DEasel](mapDrawConfig.tileSideSize, selectColor(ops), None, delayRendering(ops))
    }

  private def delayRendering(ops: BuildTDrawOpsParams[Map, SqTile, NicolLike2DEasel]) = ops.highlightedX && ops.highlightedY

  private def selectColor(ops: BuildTDrawOpsParams[Map, SqTile, NicolLike2DEasel]) = PartialFunction.condOpt(ops){
    case BuildTDrawOpsParams(_, _, true, true)    => Color.red
    case BuildTDrawOpsParams(_, _, true, false)   => Color.green
    case BuildTDrawOpsParams(_, _, false, true)   => Color.blue
  }
}