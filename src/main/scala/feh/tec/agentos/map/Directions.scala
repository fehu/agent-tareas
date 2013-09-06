package feh.tec.agentos.map

trait SimpleDirection

object SimpleDirection{
  object Left extends SimpleDirection
  object Right extends SimpleDirection
  object Up extends SimpleDirection
  object Down extends SimpleDirection

  type Bottom = Down.type
  def Bottom = Down

  type Top = Up.type
  def Top = Up
}

