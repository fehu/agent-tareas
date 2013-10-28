package feh.tec.map

trait SimpleDirection{
  def opposite: SimpleDirection
}

object SimpleDirection{
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

