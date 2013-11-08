package feh.tec.world

trait SimpleDirection{
  def opposite: SimpleDirection
}

object Simple2dDirection{
  trait Simple2dDirection extends SimpleDirection

  object Left extends Simple2dDirection{
    def opposite: Simple2dDirection = Right
    override def toString: String = "Left"
  }
  object Right extends Simple2dDirection{
    def opposite: Simple2dDirection = Left
    override def toString: String = "Right"
  }
  object Up extends Simple2dDirection{
    def opposite: Simple2dDirection = Down
    override def toString: String = "Up"
  }
  object Down extends Simple2dDirection{
    def opposite: Simple2dDirection = Up
    override def toString: String = "Down"
  }

  type Bottom = Down.type
  def Bottom = Down

  type Top = Up.type
  def Top = Up
}

object Simple3dDirection{
  trait Simple3dDirection extends SimpleDirection

  object North extends Simple3dDirection{
    def opposite: Simple3dDirection = South
    override def toString: String = "North"
  }
  object South extends Simple3dDirection{
    def opposite: Simple3dDirection = North
    override def toString: String = "South"
  }
  object West extends Simple3dDirection{
    def opposite: Simple3dDirection = East
    override def toString: String = "West"
  }
  object East extends Simple3dDirection{
    def opposite: Simple3dDirection = West
    override def toString: String = "East"
  }
  object Up extends Simple3dDirection{
    def opposite: Simple3dDirection = Down
    override def toString: String = "Up"
  }
  object Down extends Simple3dDirection{
    def opposite: Simple3dDirection = Up
    override def toString: String = "Down"
  }
}