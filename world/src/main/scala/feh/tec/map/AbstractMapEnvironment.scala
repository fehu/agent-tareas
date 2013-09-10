package feh.tec.map

import feh.tec.agent.{AbstractAction, Environment}
import feh.tec.map.tile.AbstractTile
import feh.tec.util.SideEffect

trait AbstractMapEnvironment[Map <: AbstractMap[Tile, Coordinate],
                             Tile <: AbstractTile[Tile, Coordinate],
                             Coordinate,
                             State <: MapState[Coordinate, Tile, Map],
                             Global <: MapGlobalState[Coordinate, Tile, Map],
                             Action <: MapAction[Coordinate, Tile, Map],
                             Env <: AbstractMapEnvironment[Map, Tile, Coordinate, State, Global, Action, Env]]
  extends Environment[Coordinate, State, Global, Action, Env] with AbstractMap[Tile, Coordinate]
{

}

trait MapAction[Coordinate, Tile <: AbstractTile[Tile, Coordinate], Map <: AbstractMap[Tile, Coordinate]] extends AbstractAction

trait MapGlobalState[Coordinate, Tile <: AbstractTile[Tile, Coordinate], Map <: AbstractMap[Tile, Coordinate]]

trait MapState[Coordinate, Tile <: AbstractTile[Tile, Coordinate], Map <: AbstractMap[Tile, Coordinate]]
trait MapStateBuilder[Coordinate, Tile <: AbstractTile[Tile, Coordinate], Map <: AbstractMap[Tile, Coordinate],
                      State <: MapState[Coordinate, Tile, Map]]{
  def build: Tile => State
}