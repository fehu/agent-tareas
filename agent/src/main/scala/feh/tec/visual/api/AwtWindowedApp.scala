package feh.tec.visual.api

import java.awt.{Window, Component}

/**
 * Draws `app` in `drawComponent` of `appWindow`
 */
trait AwtWindowedApp {
  def appWindow: Window
  def drawComponent: Component
  def app: AgentApp with AppBasicControlApi
}
