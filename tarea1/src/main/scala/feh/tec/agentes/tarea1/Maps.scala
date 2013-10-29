package feh.tec.agentes.tarea1

import feh.tec.util._
import feh.tec.agentes.tarea1.DummyMapGenerator.DummyMapGeneratorRandomPositionSelectHelper
import scala.util.Random
import feh.tec.agent.AgentId


object Maps {
  def failExample1Map(agentId: AgentId) =
    Seq(0 -> 0, 1 -> 1, 2 -> 3, 2 -> 4, 2 -> 6, 5 -> 2) -> Plug().lifted ::
    Seq(1 -> 5, 1 -> 7, 2 -> 7) -> Hole().lifted ::
    Seq(1 -> 6) -> AgentAvatar(agentId).lifted :: Nil

  def failExample1(agentId: AgentId) = Map.build(0 until 10, 0 until 10)(map => SqTile(map, _, None), map => buildExampleMap(map, failExample1Map(agentId)))

  def buildExampleMap(map: Map, tilesDef: List[(Seq[(Int, Int)], Lifted[MapObj])]) =
    tilesDef.flatMap{
      case (coords, obj) => coords.map(c => c -> SqTile(map, c, Option(obj())))
    }.toMap


  def randomMap(xRange: Range, yRange: Range, agentIdOpt: Option[AgentId]) = DummyMapGenerator.withHelpers[DummyMapGeneratorRandomPositionSelectHelper]
    .apply(xRange, yRange){
    h => (x, y) =>
      if (agentIdOpt.isDefined && x -> y == h.uniqueRandomPosition) Some(AgentAvatar(agentIdOpt.get))
      else {
        val r = Random.nextDouble()
        if(r < 0.2) Some(Hole())
        else if (r > 0.8) Some(Plug())
        else None
      }
  }

}
