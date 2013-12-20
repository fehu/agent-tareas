package feh.tec.agentes.tarea1

import feh.tec.visual._
import feh.tec.visual.api._
import nicol.{Game, Scene, Init}
import feh.tec.util.{ImplicitLift, LiftWrapper}
import ImplicitLift._
import feh.tec.agentes.tarea1.Tarea1.Agents.MySimpleAgent
import feh.tec.visual.api.BasicDrawEnvironmentSettings
import feh.tec.visual.api.Layout
import feh.tec.visual.api.LayoutElem
import feh.tec.visual.render.{LwjglSquareMapRoutesRenderer, LwjglSquareMapOnMouseHighlightRenderer, LwjglSquareMapRenderer, CriteriaReasonedDecisionRenderer}
import java.awt.Color
import LwjglSquareMapRenderer.BuildTDrawOpsParams
import feh.tec.agent.AgentId
import feh.tec.visual.api.WorldVisualisationCalls

case class AgentRef(ag: MySimpleAgent[Nothing])
object AgentRef{
  def create(ag: MySimpleAgent[_]): AgentRef = new AgentRef(ag.asInstanceOf[MySimpleAgent[Nothing]])
  implicit def AgentRefToRef(ref: AgentRef) = ref.ag
}

trait AgentResolver{
  def byId(id: AgentId): Option[AgentRef]
}

class NicolBasedTarea1AgentApp(val env: Environment, val agentId: AgentId, xSize: Int = 900, ySize: Int = 600)
                              (implicit agResolver: AgentResolver, tarea1: Tarea1App) extends NicolBasedAgentAppBasicControl{
  type EaselTpe = NicolLike2DEasel
  type DrawSettings = BasicDrawEnvironmentSettings

  protected val pauseEndApi: PauseEndAppInnerApi = new PauseEndAppInnerApi{
    lazy val pauseScene = new Tarea1PauseScene(tarea1, render().lifted)

    def pauseScene(resume: Scene): Scene = {
      tarea1.ag.executionLoop.pause()
      Tarea1LastSceneKeeper.scene = resume
      pauseScene
    }

    def endScene = Tarea1EndScene(tarea1)
  }

  protected def newGame(scene: => Scene): Game = new Game(scene) {
    override protected def onGameStop(): Unit = {}
  }

  def appExecutionFinished(): Boolean = tarea1.isFinished

  def finishedScene = new FinishedScene(tarea1, render)

  protected def defaultInitScene = new Init("Agent Closing Hole With Plugs \t v. 0.2", xSize, ySize)
  private var _initScene: Scene = defaultInitScene
  def overrideInitScene(scene: Scene => Scene){
    _initScene = scene(initScene)
  }
  def initScene = _initScene

  protected def defaultBaseScene = new NicolLikeBasicScene(render().lifted, pauseEndApi.endScene.lifted, finishedScene.lifted, appExecutionFinished().lifted, pauseEndApi.pauseScene)
  private var _baseScene: Scene = defaultBaseScene
  def overrideBaseScene(scene: Scene => Scene){
    _baseScene = scene(baseScene)
  }
  def baseScene = _baseScene

  val criteriaRendererScheme = CriteriaReasonedDecisionRenderer.Scheme(
    criterionFont = "arial", criterionColor = Color.white,
    beforeCriteriaFont = "arial", beforeCriteriaColor = Color.gray,
    afterCriteriaFont = "arial", afterCriteriaColor = Color.green,
    frameColor = Some(Color.blue), sepLineColor = Some(Color.white)
  )

  lazy val agRef = agResolver.byId(agentId).get

  implicit val mapRenderer: Renderer[Map, NicolLike2DEasel] with WorldVisualisationCalls[SqTile, (Int, Int)] = NicolBasedTarea1AgentApp.mapRenderer()

  implicit def criteriaValueRenderer: Renderer[agRef.ag.ActionExplanation, NicolLike2DEasel] = new CriteriaReasonedDecisionRenderer(criteriaRendererScheme)

  val layout: Layout[NicolLike2DEasel] = Layout(List[LiftedOptionLayoutElem[_, NicolLike2DEasel]](
    LayoutElem[Map, NicolLike2DEasel](env, (0, 0)),
    LiftedOptionLayoutElem[agRef.ag.ActionExplanation, NicolLike2DEasel](
      () => Option(agRef).flatMap(_.currentDecisionExplanation),
      (530, 0)
    )
  )
  )

  implicit def easelCoordinateOps = NicoleLike2dEaselCoordinateOps
  implicit lazy val easel: EaselTpe = new NicolLike2DEasel
  def render(l: Layout[EaselTpe])(implicit easel: EaselTpe): Unit = l.render
}

object NicolBasedTarea1AgentApp{
  def mapRenderer(mapDrawConfig: NicolLike2DEasel#MDrawOptions = visual.mapDrawConfig) =
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