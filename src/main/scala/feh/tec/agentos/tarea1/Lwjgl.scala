package feh.tec.agentos.tarea1

import LwjglTileRenderer._
import feh.tec.visual._
import java.awt.Color
import scala.util.Random
import feh.tec.visual.api._
import nicol._

object LwjglTest{
  def createEasel = new NicolLike2DEasel

  def createMapRenderer = new LwjglSquare2DMapRenderer[Map, SqTile, NicolLike2DEasel](LwjglTileRenderer.create, (tile, ops) =>
    BasicSquareTileDrawOptions[NicolLike2DEasel](50, Color.white, None)
  )

  def genMap = DummyMapGenerator(0 until 10, 0 until 10){
    (x, y) =>
      val r = Random.nextDouble()

      if(r < 0.2) Some(Hole())
      else if (r > 0.8) Some(Plug())
      else None
  }

  def mapDrawOps(implicit easel: NicolLike2DEasel) = BasicSquareMapDrawOptions[NicolLike2DEasel](50)

  def game = {
    implicit val easel = createEasel

    new LwjglTileGame[TCoord, SqTile, Map, NicolLike2DEasel](
      map = genMap,
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

object LwjglTestApp extends TileGameRunner(LwjglTest.game)
