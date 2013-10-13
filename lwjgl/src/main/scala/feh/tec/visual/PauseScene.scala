package feh.tec.visual

import nicol.{Scene, LoopScene, SyncableScene}
import feh.tec.visual.api.Easel
import nicol.input.Key._

class PauseScene[E <: Easel](onPause: () => Unit, onResume: () => Unit,
                             val endScene: () => Scene, val resumeScene: () => Scene,
                             pausedMessage: (String, E#StrDrawOptions),
                             val resumeKey: String = "space", val quitKey: String = "escape")
                            (implicit easel: E) extends LoopScene with SyncableScene{
  def update: Option[Scene] = {
    onPause()
    easel.drawString(pausedMessage._1, easel.center, pausedMessage._2.asInstanceOf[easel.StrDrawOptions])
    sync
    keyEvent(_.pressed(processPressedKey))
  }
  
  def processPressedKey: PartialFunction[String, Scene] = {
    case `quitKey` => endScene()
    case `resumeKey` => onResume(); resumeScene()
  } 
}