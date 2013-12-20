package feh.tec.tarea3

import feh.tec.agent.game._
import GenericDeterministicGame.Game2
import GenericMutableActorGameCoordinator.Coordinator2
import feh.tec.agent.conf.DefaultAppConfig
import GenericPlayer._
import scala.xml.NodeSeq
import feh.tec.visual.GenericGameSwingFrame.App2
import feh.tec.agent.game.DeterministicMutableGenericGameEnvironment.Env2
import feh.tec.visual.DeterministicMutableGameAppEnvironment
import feh.tec.visual.DeterministicMutableGameAppEnvironment.AppEnv2


class PaperScissorsStone extends DeterministicMutableGameAppEnvironment.AppEnv2{
  implicit val config = DefaultAppConfig.create("Paper-Scissors-Stone")

  val strategiesNames = Set("Paper", "Scissors", "Stone")

  val game = new Game2("Paper-Scissors-Stone", _.Max, "A", "B", strategiesNames, strategiesNames)(
    Left(List(
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
  )))

  def description: NodeSeq = Nil

  val player1 = statisticEnforcedPlayer(_.A)
  val player2 = statisticEnforcedPlayer(_.B)

}


class PrisonerDilemmaCreator{
  implicit val config = DefaultAppConfig.create("GenericGameTest")

  val strategiesNames = Set("Betray", "Refuse")

  object PrisonerDilemma extends Game2("Prisoner-Dilemma", _.Min, "A", "B", strategiesNames)(
    ("Betray" -> "Refuse") -> (0, 12),
    ("Betray" -> "Betray") -> (8, 8),
    ("Refuse" -> "Refuse") -> (6, 6),
    ("Refuse" -> "Betray") -> (12, 0)
  )

  type Game = PrisonerDilemma.type
  type Env = Env2[Game]

  val environment = new Env2(PrisonerDilemma)
  val coordinator = new Coordinator2[Game, Env](environment)
  val executor = new GenericExecutor[Game, Env](config.awaitEndOfTurnTimeout)(config.executionContext)

  val player1 = new GenericPlayer[Game, Env](PrisonerDilemma.A, executor, coordinator.ref)
    with SimpleBestStrategyChooser2[Game, Env] with GenericPlayer.SimpleRandomBehaviour[Game, Env]
  val player2 = new GenericPlayer[Game, Env](PrisonerDilemma.B, executor, coordinator.ref)
    with SimpleBestStrategyChooser2[Game, Env] with GenericPlayer.SimpleRandomBehaviour[Game, Env]

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
  val a = new PrisonerDilemmaCreator
  a.app.isMain = true
  a.app.start()
}

object BattleOFSexes extends Game2("Battle of the Sexes", _.Max, "John", "Jane", Set("Opera", "Football"))(
    ("Opera" -> "Football")     -> (0, 0),
    ("Opera" -> "Opera")        -> (1, 2),
    ("Football" -> "Football")  -> (2, 1),
    ("Football" -> "Opera")     -> (0, 0)
  )

object PenaltyKicks extends Game2(_.Max, "Penalty Kicks", "Kicker", "Goalie", Set("Left", "Center", "Right"))({
  case (s1, s2) if s1 == s2 => 0 -> 1
  case _ => 1 -> 0
})


object ExampleGamesApps{
  implicit def createAppConfig = DefaultAppConfig.create()

  def battleOFSexes = AppEnv2(BattleOFSexes)
    .createPlayer(
      _.statisticEnforcedPlayer(_.A),
      _.statisticEnforcedPlayer(_.B)
    )
  def penaltyKicks = AppEnv2(PenaltyKicks)
    .createPlayer(
      _.statisticEnforcedPlayer(_.A),
      _.statisticEnforcedPlayer(_.B)
    )
}