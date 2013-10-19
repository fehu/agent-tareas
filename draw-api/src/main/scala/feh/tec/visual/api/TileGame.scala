package feh.tec.visual.api

import feh.tec.map.tile.AbstractTile
import feh.tec.map.AbstractMap

@deprecated("use nicol")
trait TileGame {
  type TCoord
  type Tile <: AbstractTile[Tile, TCoord]
  type Map <: AbstractMap[Tile, TCoord]
  type EaselTpe <: Easel
  type DrawSettings <: DrawEnvironmentSettings

  def map: Map

  implicit def easelCoordinateOps(easel: EaselTpe): EaselCoordinateOps[EaselTpe]

  def prepareDrawEnvironment(ops: DrawSettings)
  def preCreate()(implicit easel: EaselTpe)
  def render(l: Layout[EaselTpe])(implicit easel: EaselTpe)

    @deprecated("render layout with map")
  def renderMap(ops: EaselTpe#MDrawOptions)(implicit easel: EaselTpe)

  def mapDrawOps: EaselTpe#MDrawOptions
  def drawEnvSettings: DrawSettings

  implicit def easel: EaselTpe

  def run(){ // todo
    prepareDrawEnvironment(drawEnvSettings)
    preCreate()
    renderMap(mapDrawOps)
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