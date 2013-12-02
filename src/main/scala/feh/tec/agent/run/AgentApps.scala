package feh.tec.agent.run

import feh.tec.visual.build.SwingAppsRunner
import feh.tec.agentes.tarea1.Tarea1App
import feh.tec.tarea3.PrisonerDilemmaApp

object AgentApps extends SwingAppsRunner(                                                           "Apps Runner #0.3",
  "Hole-Plug"                       -> (() => new Tarea1App),
  "Prisoner Dilemma"                -> (() => new PrisonerDilemmaApp())
)
