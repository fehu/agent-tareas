package feh.tec.visual

import nicol.Scene

trait PauseEndGameInnerApi{
  def pauseScene(resume: Scene): Scene
  def endScene: Scene
}