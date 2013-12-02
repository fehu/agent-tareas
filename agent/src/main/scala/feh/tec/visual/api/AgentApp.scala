package feh.tec.visual.api

trait AgentApp{
  type EaselTpe <: Easel with EaselAffineTransforms
  type DrawSettings <: DrawEnvironmentSettings

  def layout: Layout[EaselTpe]

  implicit def easelCoordinateOps: EaselCoordinateOps[EaselTpe]

  def prepareDrawEnvironment(ops: DrawSettings)
  def render(l: Layout[EaselTpe])(implicit easel: EaselTpe)

  implicit def easel: EaselTpe
}

trait DrawEnvironmentSettings{
  def title: String
  def width: Int
  def height: Int
  def fullscreen: Boolean
}

case class BasicDrawEnvironmentSettings(title: String,
                                        width: Int ,
                                        height: Int,
                                        fullscreen: Boolean) extends DrawEnvironmentSettings

trait AppBasicControlApi {
  def start()
  def run() = start()
  //  def pause()
  //  def resume()
  def stop()
  def isRunning: Boolean
}
