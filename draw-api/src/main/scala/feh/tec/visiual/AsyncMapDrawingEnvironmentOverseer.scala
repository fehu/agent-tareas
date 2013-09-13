package feh.tec.visiual

import feh.tec.util.SideEffect
import feh.tec.visual.api.{Easel, MapRenderer}
import feh.tec.map._
import feh.tec.map.tile.AbstractTile
import feh.tec.agent.EnvironmentOverseer
import scala.concurrent.duration.Duration

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
  self: MapEnvironmentOverseerActor[Map, Tile, Coordinate, State, Global, Action, Env] =>

  def mapRenderer: MapRenderer[Map, Tile, Coordinate, Ea]

  def mapDrawConfig: Ea#MDrawOptions

  implicit def easel: Ea

  private implicit def executionContext = externalExecutionContext

  protected abstract override def affect(a: Action): SideEffect[Env] = super.affect(a)
    .foreach(env => scheduler.scheduleOnce(Duration.Zero)(mapRenderer.render(env: Map, mapDrawConfig)))
}
