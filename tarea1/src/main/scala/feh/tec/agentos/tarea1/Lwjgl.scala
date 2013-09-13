package feh.tec.agentos.tarea1

import LwjglTileRenderer._
import feh.tec.visual._
import java.awt.Color
import scala.util.Random
import feh.tec.visual.api._
import nicol._
import feh.tec.agent.AgentId
import feh.tec.agentos.tarea1.DummyMapGenerator.DummyMapGeneratorRandomPositionSelectHelper

object LwjglTest{
  def createEasel = new NicolLike2DEasel

  def createMapRenderer = new LwjglSquare2DMapRenderer[Map, SqTile, NicolLike2DEasel](LwjglTileRenderer.create, (tile, ops) =>
    BasicSquareTileDrawOptions[NicolLike2DEasel](50, Color.white, None)
  )

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

  def mapDrawOps(implicit easel: NicolLike2DEasel) = BasicSquareMapDrawOptions[NicolLike2DEasel](50)

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
          easel.drawString("TEST", (100.toFloat, 100.toFloat), BasicStringDrawOps[NicolLike2DEasel](StringAlignment.Center, Color.red, "Arial", 0, 12))
        )
      )
    )

  }
}
// doesn't work, sue Nicol
//object LwjglTestApp extends TileGameRunner(LwjglTest.game(LwjglTestAppAgentHolder.get))
