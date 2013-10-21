package feh.tec.agentes.tarea1

import feh.tec.visual._
import feh.tec.visual.api._
import nicol.{End, Game, Scene, Init}
import feh.tec.util.{ImplicitLift, LiftWrapper}
import ImplicitLift._
import feh.tec.agentes.tarea1.{Map => TMap}
import feh.tec.agentes.tarea1.Tarea1.Agents.MyDummyAgent
import feh.tec.agentes.tarea1.Agent.Measure
import java.awt.Color
import feh.tec.visual.api.BasicStringDrawOps
import feh.tec.visual.api.BasicDrawEnvironmentSettings
import feh.tec.visual.api.Layout
import feh.tec.visual.api.LayoutElem
import feh.tec.visual.api.StringAlignment.Center

trait Tarea1Types extends NicolLikeTileGame{
  import Agent._

  type TCoord = Position
  type Tile = SqTile
  type Map = TMap
  type EaselTpe = NicolLike2DEasel
  type DrawSettings = BasicDrawEnvironmentSettings
}

case class AgentRef(ag: MyDummyAgent[Nothing])
object AgentRef{
  def create(ag: MyDummyAgent[_]): AgentRef = new AgentRef(ag.asInstanceOf[MyDummyAgent[Nothing]])
  implicit def AgentRefToRef(ref: AgentRef) = ref.ag
}

class NicolLikeTarea1Game(env: Environment, agRef: AgentRef) extends NicolLikeTileGame with Tarea1Types{
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

  protected def initScene: Scene = Init("Tarea1 v. 0.01", 800, 600)
  protected def baseScene: Scene = new NicolLikeBasicScene(render().lifted, pauseEndApi.endScene.lifted, finishedScene.lifted, gameExecutionFinished().lifted, pauseEndApi.pauseScene)

  implicit def mapRenderer: Renderer[Map, NicolLike2DEasel] = Lwjgl.createMapRenderer
  implicit def criteriaValueRenderer: Renderer[Measure#CriteriaValue, NicolLike2DEasel] = new Tarea1CriteriaValueRenderer

  def gameLayout: Layout[NicolLike2DEasel] = Layout(List[LiftedLayoutElem[_, NicolLike2DEasel]](
    LayoutElem[Map, NicolLike2DEasel](env, (0, 0)),
    new LiftedLayoutElem[agRef.ag.measure.CriteriaValue, NicolLike2DEasel](
      () => agRef.currentDecisionExplanation.get.criteria.toSeq,
      (530, 0)
    )
  )
  )

  implicit def easelCoordinateOps = NicoleLike2dEaselCoordinateOps

  @deprecated("render layout with map")
  def renderMap(ops: EaselTpe#MDrawOptions)(implicit easel: EaselTpe): Unit = ???

  def mapDrawOps: EaselTpe#MDrawOptions = Lwjgl.mapDrawOps

  def drawEnvSettings: DrawSettings = ??? //BasicDrawEnvironmentSettings("Tarea1", 600, 800, false)

  implicit lazy val easel: EaselTpe = new NicolLike2DEasel

  def render(l: Layout[EaselTpe])(implicit easel: EaselTpe): Unit = l.render
}


class Tarea1CriteriaValueRenderer extends CriteriaValueRenderer[Agent.Position, Agent.EnvState, Agent.EnvGlobal, Agent.Action, Agent.Env, Agent.Measure, Agent.Easel]{

  override def render(t: Measure#CriteriaValue)(implicit easel: Agent.Easel): Unit = {
    import easel.WithoutTextures
//    easel.drawRect(0F -> 300F, 300F -> 0F).withoutTextures
    t.zipWithIndex foreach {
      case (cval , i) => easel.withAffineTransform(easel.Offset(10, 50+30*i)){
        render(cval)
      }
    }

    easel.drawLine(0F -> 320F, 300F -> 320F).withoutTextures
    easel.drawString("Criteria value: " + t.map(_.value).sum, 20F -> 350F, BasicStringDrawOps[NicolLike2DEasel](StringAlignment.Left, Color.green, "Arial", 0F, 12F))
  }
  def render(t: Measure#CriterionValue)(implicit easel: Agent.Easel): Unit =
    easel.drawString(s"${t.name}: ${t.value}", (0, 0), BasicStringDrawOps[NicolLike2DEasel](StringAlignment.Left, Color.white, "Arial", 0, 12))
}