package feh.tec.agent.run

import feh.tec.visual.build.SwingAppsRunner
import feh.tec.agentes.tarea1.Tarea1App
import feh.tec.tarea3.{GenericGameTest, PrisonerDilemmaTest}
import feh.tec.visual.api.StopNotifications

//import feh.tec.tarea3.PrisonerDilemmaApp
import feh.tec.util.build.LwjglAppsRunner

object AgentApps extends SwingAppsRunner(                                                           "Apps Runner #0.3",
  "Hole-Plug"                       -> (() => new Tarea1App with StopNotifications),
  "Prisoner Dilemma"                -> (() => (new PrisonerDilemmaTest).app),
  "Paper-Scissors-Stone"            -> (() => (new GenericGameTest).app)
)
  with LwjglAppsRunner