package feh.tec.agentos.tarea1

import LwjglTileRenderer._
import feh.tec.visual.{NicolLike2DEasel, LwjglSquare2DMapRenderer}
import feh.tec.visual.api.BasicSquareTileDrawOptions
import java.awt.Color

object LwjglMapRenderer {
  def create = new LwjglSquare2DMapRenderer[Map, SqTile](LwjglTileRenderer.create, (tile, ops) =>
    BasicSquareTileDrawOptions[NicolLike2DEasel](50, Color.black, None)
  )

  def createEasel = new NicolLike2DEasel
}
