package feh.tec.world

trait SimpleDirection{
  def opposite: SimpleDirection
}

object Simple2dDirection{
  object Left extends SimpleDirection{
    def opposite: SimpleDirection = Right
    override def toString: String = "Left"
  }
  object Right extends SimpleDirection{
    def opposite: SimpleDirection = Left
    override def toString: String = "Right"
  }
  object Up extends SimpleDirection{
    def opposite: SimpleDirection = Down
    override def toString: String = "Up"
  }
  object Down extends SimpleDirection{
    def opposite: SimpleDirection = Up
    override def toString: String = "Down"
  }

  type Bottom = Down.type
  def Bottom = Down

  type Top = Up.type
  def Top = Up
}

