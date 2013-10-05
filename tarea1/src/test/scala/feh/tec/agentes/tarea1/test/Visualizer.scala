package feh.tec.agentes.tarea1.test

import feh.tec.visual.api.{BasicSquareMapDrawOptions, SquareMapDrawOptions, Easel}
import feh.tec.visual.NicolLike2DEasel
import feh.tec.agentes.tarea1.{LwjglTest, Map}
import nicol._
import nicol.Init
import nicol.input.Key._
import nicol.Init

trait Visualizer[E <: Easel, V] {
  def easel: Easel
  def visualisation: V

  def show()
  def close()

}

class NicolLikeMapVisualizer(val visualisation: Map)
  extends Visualizer[NicolLike2DEasel, Map]
{
  implicit val easel = new NicolLike2DEasel
  val howToDraw: NicolLike2DEasel#MDrawOptions = BasicSquareMapDrawOptions(50)

  val mapRenderer = LwjglTest.createMapRenderer
  val game = new Game(Init("Tests", 800, 600, vSync = false) >> new LoopScene with SyncableScene{
    def update: Option[Scene] = {
      sync

      renderMap()

      keyEvent {
        e =>
          e released {
            case _ =>
          }
          e pressed {
            case "escape" => End
          }
      }
    }
  }){}

  protected def renderMap() = mapRenderer.render(visualisation, howToDraw)

  def show() { game.start }
  def close() { game.stop }
}