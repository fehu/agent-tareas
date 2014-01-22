package feh.tec.visual

import java.awt.{Component => AComponent, Window => AWindow}
import feh.tec.visual.api.{AwtWindowedApp, AppBasicControlApi, AgentApp}
import scala.swing.{Component, Window}

trait SwingWindowedApp {
  def appWindow: Window
  def drawComponent: Component
  def app: AgentApp with AppBasicControlApi
}

class SwingToAwtWindowedAppAdapter(swingApp: SwingWindowedApp) extends AwtWindowedApp{
  def appWindow: AWindow = swingApp.appWindow.peer
  def drawComponent: AComponent = swingApp.drawComponent.peer
  def app: AgentApp with AppBasicControlApi = swingApp.app
}