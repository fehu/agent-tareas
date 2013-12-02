package feh.tec.util.build

import feh.tec.visual.api.AppBasicControlApi

trait AppsRunner{
  def apps: Map[String, () => AppBasicControlApi]
  def start()
  def stop()
}
