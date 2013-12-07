package feh.tec.agent.run

import feh.tec.visual.build.SwingAppsRunner
import feh.tec.agentes.tarea1.Tarea1App
import feh.tec.tarea3.{ExampleGamesApps, PaperScissorsStone, PrisonerDilemmaCreator}
import feh.tec.visual.api.StopNotifications
import feh.tec.util.{LiftWrapper, Lifted}
import feh.tec.agent.game.GameAppEnvironment
import feh.tec.util.build.LwjglAppsRunner

object AgentAppsHelpers{
  implicit def gameAppEnvironmentToStringLiftedAppPairWrapper(env: GameAppEnvironment): (String, Lifted[SwingAppsRunner.Control]) =
    env.game.name -> env.app.lifted
}

import AgentAppsHelpers._

object AgentApps extends SwingAppsRunner(                                                           "Apps Runner #0.3.2",
  "Hole-Plug"                       -> (() => new Tarea1App with StopNotifications),
  "Prisoner Dilemma"                -> (() => (new PrisonerDilemmaCreator).app),
  "Paper-Scissors-Stone"            -> (() => (new PaperScissorsStone).app),
  ExampleGamesApps.battleOFSexes,
  ExampleGamesApps.penaltyKicks
)
  with LwjglAppsRunner