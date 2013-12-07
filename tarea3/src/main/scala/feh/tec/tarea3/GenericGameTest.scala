package feh.tec.tarea3

import feh.tec.agent.GenericDeterministicGame.Game2
import feh.tec.agent.GenericMutableActorGameCoordinator.Coordinator2
import feh.tec.agent.conf.DefaultAppConfig
import feh.tec.agent._
import feh.tec.agent.GenericPlayer._
import scala.collection.mutable
import scala.xml.NodeSeq
import feh.tec.visual.GenericGameSwingFrame.App2
import feh.tec.agent.DeterministicMutableGenericGameEnvironment.Env2
import feh.tec.agent.conf.AppConfig

trait DeterministicMutableGameAppEnvironment2{
  type Game = GenericDeterministicGame.Game2
  type Env = DeterministicMutableGenericGameEnvironment.Env2[Game]
  type Coord = GenericMutableActorGameCoordinator.Coordinator2[Game, Env]
  type Agent = PlayerAgent[Game, Env] with PlayerAgent.Resettable[Game, Env] with PlayerAgent.RandomBehaviour[Game, Env]
  type Exec = GenericExecutor[Game, Env]

  implicit def config: AppConfig
//  type Game <: Game2
  def game: Game
  def description: NodeSeq

  lazy val environment: Env = new Env2(game)
  lazy val coordinator: Coord = new Coordinator2[Game2, Env2[Game2]](environment)
  lazy val executor: Exec = new GenericExecutor[Game2, Env2[Game2]](config.awaitEndOfTurnTimeout)(config.executionContext)

  private val playersMap = mutable.Map.empty[Agent, Game#Player]

  def dummyPlayer(sel: Game2 => Game2#Player): Agent = {
    val p = sel(game)
    val pl = new GenericPlayer[Game2, Env2[Game2]](p, executor, coordinator.ref)
      with DummyBestStrategyChooser2[Game2, Env2[Game2]]
      with RandomBehaviour[Game2, Env2[Game2]]
    playersMap += pl -> p
    pl
  }

  def statisticEnforcedPlayer(sel: Game2 => Game2#Player): Agent = {
    val p = sel(game)
    val pl = new GenericPlayer[Game2, Env2[Game2]](p, executor, coordinator.ref)
      with DummyBestStrategyChooser2[Game2, Env2[Game2]]
      with SimpleStatistics[Game2, Env2[Game2]]
      with SimpleExpectedUtility2[Game2, Env2[Game2]]
      with SimpleStatisticsGathering[Game2, Env2[Game2]]
      with RandomBehaviour[Game2, Env2[Game2]]
    playersMap += pl -> p
    pl
  }

  lazy val app = new App2[Game, Env, Coord, Agent, Exec](game, environment, coordinator, executor, playersMap.toMap, description)
}

class GenericGameTest extends DeterministicMutableGameAppEnvironment2{
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

  def description: NodeSeq = Nil

  val player1 = statisticEnforcedPlayer(_.A)
  val player2 = statisticEnforcedPlayer(_.B)

}


class PrisonerDilemmaTest{
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

  val player1 = new GenericPlayer[Game, Env](PrisonerDilemma.A, executor, coordinator.ref)
    with DummyBestStrategyChooser2[Game, Env] with GenericPlayer.RandomBehaviour[Game, Env]
  val player2 = new GenericPlayer[Game, Env](PrisonerDilemma.B, executor, coordinator.ref)
    with DummyBestStrategyChooser2[Game, Env] with GenericPlayer.RandomBehaviour[Game, Env]

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

  val app = new App2(PrisonerDilemma, environment, coordinator, executor, Map(
    player1 -> PrisonerDilemma.A,
    player2 -> PrisonerDilemma.B
  ), description)
}

object PrisonerDilemmaTestApplication extends App{
  val a = new PrisonerDilemmaTest
  a.app.isMain = true
  a.app.start()
}