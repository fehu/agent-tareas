package feh.tec.visual

import feh.tec.visual.SwingGameFrame.{ComponentAccess, AbstractLayoutSetting}
import feh.tec.visual.api.Game
import java.awt.Component

trait SwingNicolGame extends SwingGameFrame{
  def drawComponent: Component = ???

  def game: Game = ???

}
