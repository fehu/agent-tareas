package feh.tec.visual.api

trait GameBasicControlApi {
  def start()
  def run() = start()
//  def pause()
//  def resume()
  def stop()
}
