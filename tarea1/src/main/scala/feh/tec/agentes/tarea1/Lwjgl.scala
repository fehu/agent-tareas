package feh.tec.agentes.tarea1

import LwjglTileRenderer._
import feh.tec.visual._
import java.awt.Color
import scala.util.Random
import feh.tec.visual.api._
import nicol._
import feh.tec.agent.AgentId
import feh.tec.agentes.tarea1.DummyMapGenerator.DummyMapGeneratorRandomPositionSelectHelper
import feh.tec.visual.render.LwjglSquare2DMapRenderer
import LwjglSquare2DMapRenderer.BuildTDrawOpsParams

object Lwjgl{
  object Settings{
    def tileSideSize = 50
    def showLabels = false
    def howToDrawTheMap = new SquareMapDrawOptions[NicolLike2DEasel]{
      def tileSideSize: NicolLike2DEasel#CoordinateUnit = Settings.tileSideSize
      def showLabels: Boolean = Settings.showLabels
    }
  }
  import Settings._

  def createEasel = new NicolLike2DEasel

  implicit def createMapRenderer = new LwjglSquare2DMapRenderer[Map, SqTile, NicolLike2DEasel](
    LwjglTileRenderer.create,
    howToDrawTheMap,
    ops => BasicSquareTileDrawOptions[NicolLike2DEasel](tileSideSize, selectColor(ops), None)
  )



  private def selectColor(ops: BuildTDrawOpsParams[Map, SqTile, NicolLike2DEasel]) = ops match {
    case BuildTDrawOpsParams(_, _, true, true)    => Color.red
    case BuildTDrawOpsParams(_, _, true, false)   => Color.green
    case BuildTDrawOpsParams(_, _, false, true)   => Color.blue
    case _ => Color.white
  }

  def genMap(ag: Option[AgentId]) = {
    import DummyMapGenerator._

    withHelpers[DummyMapGeneratorRandomPositionSelectHelper].apply(0 until 10, 0 until 10)(mapBuildingFunc(ag))
  }

  def mapBuildingFunc(ag: Option[AgentId]): DummyMapGeneratorRandomPositionSelectHelper => (Int, Int) => Option[MapObj] = {
    h => (x, y) =>
      if (ag.isDefined && x -> y == h.uniqueRandomPosition) Some(AgentAvatar(ag.get))
      else {
        val r = Random.nextDouble()
        if(r < 0.2) Some(Hole())
        else if (r > 0.8) Some(Plug())
        else None
      }
  }

  def mapDrawOps(implicit easel: NicolLike2DEasel) = BasicSquareMapDrawOptions[NicolLike2DEasel](50, showLabels = false)

  def game(ag: Option[AgentId]) = {
    implicit val easel = createEasel

    new LwjglTileGame[TCoord, SqTile, Map, NicolLike2DEasel](
      map = genMap(ag),
      mapRenderer = createMapRenderer,
      mapDrawOps = mapDrawOps,
      drawEnvSettings = BasicDrawEnvironmentSettings("test", 600, 800, false),
      glSettings = new CopiedFromNicolExampleGL11Settings,
      preCreateOpt = Some(
        () => Pretransformed(
          easel.drawString("TEST", (100.toFloat, 100.toFloat), BasicStringDrawOps[NicolLike2DEasel](StringAlignment.Center, Color.red, "Arial", 12, 3))
        )
      )
    )

  }
}