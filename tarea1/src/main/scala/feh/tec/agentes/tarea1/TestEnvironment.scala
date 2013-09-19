package feh.tec.agentes.tarea1

import feh.tec.agent.AgentId


object TestEnvironment {
  def test1(ag: Option[AgentId]) =
    Tarea1.environment(ag, DummyMapGenerator.buildTilesMap(Tarea1.Environment.xRange, Tarea1.Environment.yRange){
      case (3, 7) => ag map AgentAvatar
      case (3, 8) => Some(Plug())
      case (3, 9) => Some(Hole())
      case _ => None
    })

}
