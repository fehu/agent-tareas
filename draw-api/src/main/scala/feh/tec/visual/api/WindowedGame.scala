package feh.tec.visual.api

import java.awt.{Window, Component}


trait WindowedGame {
  def gameWindow: Window
  def drawComponent: Component
  def game: Game
}
