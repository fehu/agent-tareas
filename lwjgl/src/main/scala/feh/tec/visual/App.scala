package feh.tec.visual

import nicol.Scene

trait PauseEndAppInnerApi{
  def pauseScene(resume: Scene): Scene
  def endScene: Scene
}