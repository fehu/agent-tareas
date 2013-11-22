package feh.tec.visual

import feh.tec.visual.SwingAppFrame.{ComponentAccess, AbstractLayoutSetting}
import feh.tec.visual.api.AgentApp
import java.awt.Component

trait SwingNicolApp extends SwingAppFrame{
  def drawComponent: Component = ???

  def app: AgentApp = ???

}
