package feh.tec.agentes.tarea1

import feh.tec.visual.NicolLike2DEasel
import feh.tec.visual.api.BasicSquareMapDrawOptions
import nicol._
import nicol.input.Key._
import feh.tec.agent.AgentId

class NicolTestMain(val map: Map, buildMapDrawOps: NicolLike2DEasel => BasicSquareMapDrawOptions[NicolLike2DEasel]) extends LoopScene with SyncableScene with ShowFPS{

  implicit val easel = LwjglTest.createEasel
  val mapRenderer = LwjglTest.createMapRenderer

  val mapDrawOps = buildMapDrawOps(easel)

  def update: Option[Scene] = {
    //    Pretransformed {
    //      easel.drawString("Test", (300, 300), BasicStringDrawOps[NicolLike2DEasel](StringAlignment.Center, Color.red, "Arial", 0, 12))
    //    }

    //    easel.drawString("Test 2", (400, 400), BasicStringDrawOps[NicolLike2DEasel](StringAlignment.Center, Color.green, "Arial", 0, 12))


    mapRenderer.render(map, mapDrawOps)

    sync
    showFPS

    keyEvent {
      e =>
        e released {
          case _ =>
          //            draw("Releasd %s".format(e.name), position = (350, 100), rgb = (1, 0, 0))
        }
        e pressed {
          case "escape" =>
            NicolTestApp.stop
            End
        }
    }
  }
}

object NicolTestAppAgentHolder extends ThreadLocal[Option[AgentId]]{
  override def initialValue(): Option[AgentId] = Some(AgentId())
}

object NicolTestApp extends Game(Init("Test", 800, 600) >> new NicolTestMain(LwjglTest.genMap(NicolTestAppAgentHolder.get()), implicit easel => LwjglTest.mapDrawOps))