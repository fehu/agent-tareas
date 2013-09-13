package feh.tec.agentos.tarea1

import feh.tec.agentos.tarea1.DummyMapGenerator.DummyMapGeneratorRandomPositionSelectHelper

object AgentApp extends App{
  val ag = new AbstractAgent() {}

  val environment = new Environment(
    buildTilesMap = DummyMapGenerator.withHelpers[DummyMapGeneratorRandomPositionSelectHelper]
      .buildTilesMap(0 until 10, 0 until 10)(LwjglTest.mapBuildingFunc(Option(ag)))
  )
}
