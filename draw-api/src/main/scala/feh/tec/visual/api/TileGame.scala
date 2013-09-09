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

  def prepareDrawEnvironment(ops: DrawSettings)
  def preCreate()(implicit easel: EaselTpe)
  def createMap(ops: EaselTpe#MDrawOptions)(implicit easel: EaselTpe)

  def mapDrawOps: EaselTpe#MDrawOptions
  def drawEnvSettings: DrawSettings

  implicit def easel: EaselTpe

  def run(){ // todo
    prepareDrawEnvironment(drawEnvSettings)
    preCreate()
    createMap(mapDrawOps)
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