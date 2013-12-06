package feh.tec.util.build

import feh.tec.visual.api.{StopNotifications, AppBasicControlApi}

trait AppsRunner{
  def apps: Map[String, () => AppBasicControlApi with StopNotifications]
  def start()
  def stop()
}
