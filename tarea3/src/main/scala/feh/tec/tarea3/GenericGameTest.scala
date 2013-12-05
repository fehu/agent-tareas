package feh.tec.tarea3

import feh.tec.agent.GenericDeterministicGame.Game2
import feh.tec.agent.DeterministicMutableGenericGameEnvironment.Env2
import feh.tec.agent.GenericMutableActorGameCoordinator.Coordinator2
import feh.tec.agent.conf.DefaultAppConfig
import feh.tec.visual.GenericGameSwingFrame.App2
import feh.tec.agent.{GenericPlayer, GenericExecutor}
import feh.tec.agent.GenericPlayer.DummyBestStrategyChooser2

trait GenericGameTest{
  implicit val config = DefaultAppConfig.create("GenericGameTest")

  val strategiesNames = Set("Paper", "Scissors", "Stone")

  val game = new Game2("Paper-Scissors-Stone", _.Max, "A", "B", strategiesNames, strategiesNames)(
    // draw
    ("Paper" -> "Paper") -> (0, 0),
    ("Scissors" -> "Scissors") -> (0, 0),
    ("Stone" -> "Stone") -> (0, 0),
    // A wins
    ("Paper" -> "Stone") -> (1, -1),
    ("Scissors" -> "Paper") -> (1, -1),
    ("Stone" -> "Scissors") -> (1, -1),
    // B wins
    ("Paper" -> "Scissors") -> (-1, 1),
    ("Scissors" -> "Stone") -> (-1, 1),
    ("Stone" -> "Paper") -> (-1, 1)
  )

  val environment = new Env2(game)
  val coordinator = new Coordinator2[Game2, Env2[Game2]](environment)
  val executor = new GenericExecutor(config.awaitEndOfTurnTimeout)(config.executionContext)

//  val player1 =

//  val app = new App2(game, environment, coordinator, executor, )
}


object PrisonerDilemmaTest{
  implicit val config = DefaultAppConfig.create("GenericGameTest")

  val strategiesNames = Set("Betray", "Refuse")

  object PrisonerDilemma extends Game2("Prisoner-Dilemma", _.Min, "A", "B", strategiesNames, strategiesNames)(
    ("Betray" -> "Refuse") -> (0, 12),
    ("Betray" -> "Betray") -> (8, 8),
    ("Refuse" -> "Refuse") -> (6, 6),
    ("Refuse" -> "Betray") -> (12, 0)
  )

  type Game = PrisonerDilemma.type
  type Env = Env2[Game]
//  type Agent = GenericPlayer[Game, Env] with DummyBestStrategyChooser2[Game]

  val environment = new Env2(PrisonerDilemma)
  val coordinator = new Coordinator2[Game, Env](environment)
  val executor = new GenericExecutor[Game, Env](config.awaitEndOfTurnTimeout)(config.executionContext)

  val player1 = new GenericPlayer[Game, Env](PrisonerDilemma.A, executor, coordinator.ref) with DummyBestStrategyChooser2[Game]
  val player2 = new GenericPlayer[Game, Env](PrisonerDilemma.B, executor, coordinator.ref) with DummyBestStrategyChooser2[Game]

  val description =
    <html>
      in order to make a decision each agent
      <ol>
        <li>calculates expected utility for each available choice, taking in accont previous opponent's moves</li>
        <li>selects a {grayI("random")} strategy with probability set in the interface</li>
        <li>chooses the {grayI("random")} strategy with preference, set in the interface</li>
      </ol>
    </html>
  def grayI(str: String) = <i><font color="gray">{str}</font></i>

//  val app = new App2/*[Game, Env, Coordinator2[Game, Env], Agent, GenericExecutor[Game, Env]]*/()
  val app = new App2(PrisonerDilemma, environment, coordinator, executor, Map(
    player1 -> PrisonerDilemma.A,
    player2 -> PrisonerDilemma.B
  ))
}

object PrisonerDilemmaTestApplication extends App{
  PrisonerDilemmaTest.app.start()
}