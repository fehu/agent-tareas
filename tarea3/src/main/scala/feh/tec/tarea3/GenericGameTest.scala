package feh.tec.tarea3

import feh.tec.visual.{SwingFrameAppCreation, SwingAppFrame}
import scala.swing.Frame
import feh.tec.agent.GenericDeterministicGame.Game2
import feh.tec.agent.DeterministicMutableGenericGameEnvironment.Env2
import feh.tec.agent.GenericMutableActorGameCoordinator.Coordinator2
import feh.tec.agent.conf.DefaultAppConfig

trait GenericGameTest{
  implicit def config = DefaultAppConfig.create("GenericGameTest")

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
  val coordinator = new Coordinator2[Game2, Env2](environment)
}

class GenericGameTestApp extends Frame with SwingAppFrame with SwingFrameAppCreation.Frame9PositionsLayoutBuilder with GenericGameTest{

  lazy val layout: List[SwingFrameAppCreation.AbstractLayoutSetting] = null

  def start(): Unit = ???
  def stop(): Unit = ???
  def isRunning: Boolean = ???
}
