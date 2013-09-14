package feh.tec.visiual

import feh.tec.util.SideEffect
import feh.tec.visual.api.{Easel, MapRenderer}
import feh.tec.map._
import feh.tec.map.tile.AbstractTile
import feh.tec.agent.EnvironmentOverseer
import scala.concurrent.duration.Duration

//todo: move from api module
@deprecated("it should have nicol-game init like structure in the drawing thread in order to have openGL context; for a while rely on Nicol Game and Init ")
trait AsyncMapDrawingEnvironmentOverseer[Map <: AbstractMap[Tile, Coordinate],
                                         Tile <: AbstractTile[Tile, Coordinate],
                                         Coordinate,
                                         State <: MapState[Coordinate, Tile, Map],
                                         Global <: MapGlobalState[Coordinate, Tile, Map],
                                         Action <: MapAction[Coordinate, Tile, Map],
                                         Env <: MapEnvironment[Map, Tile, Coordinate, State, Global, Action, Env] with Map,
                                         Ea <: Easel]
extends EnvironmentOverseer[Coordinate, State, Global, Action, Env]
{
  self: MapEnvironmentOverseerWithActor[Map, Tile, Coordinate, State, Global, Action, Env] =>

  def mapRenderer: MapRenderer[Map, Tile, Coordinate, Ea]

  def mapDrawConfig: Ea#MDrawOptions

  implicit def easel: Ea

//  protected abstract override def affect(a: Action): SideEffect[Env] = super.affect(a)
//    .foreach(env => scheduler.scheduleOnce(Duration.Zero)(mapRenderer.render(env: Map, mapDrawConfig)))
}
