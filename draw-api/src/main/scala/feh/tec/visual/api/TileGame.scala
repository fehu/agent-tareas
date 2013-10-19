package feh.tec.visual.api

import feh.tec.map.tile.AbstractTile
import feh.tec.map.AbstractMap

trait TileGame {
  type TCoord
  type Tile <: AbstractTile[Tile, TCoord]
  type Map <: AbstractMap[Tile, TCoord]
  type EaselTpe <: Easel
  type DrawSettings <: DrawEnvironmentSettings

//  def map: Map

  def gameLayout: Layout[EaselTpe]

  implicit def easelCoordinateOps: EaselCoordinateOps[EaselTpe]

  def prepareDrawEnvironment(ops: DrawSettings)
  def render(l: Layout[EaselTpe])(implicit easel: EaselTpe)

    @deprecated("render layout with map")
  def renderMap(ops: EaselTpe#MDrawOptions)(implicit easel: EaselTpe)

  def mapDrawOps: EaselTpe#MDrawOptions
  def drawEnvSettings: DrawSettings

  implicit def easel: EaselTpe

  def run(){ // todo
    prepareDrawEnvironment(drawEnvSettings)
    render(gameLayout)
  }
}

trait DrawEnvironmentSettings{
  def title: String
  def width: Int
  def height: Int
  def fullscreen: Boolean
}

case class BasicDrawEnvironmentSettings(title: String,
                                        width: Int ,
                                        height: Int,
                                        fullscreen: Boolean) extends DrawEnvironmentSettings