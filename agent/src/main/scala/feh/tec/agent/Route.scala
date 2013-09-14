package feh.tec.agent

/**
 *
 * @param steps doesn't include `from` nor `to`
 */
case class Route[Coordinate](from: Coordinate, to: Coordinate, steps: List[Coordinate]){
  def length = steps.length + 1

  private def resolveToAndNextFrom(next: Route[Coordinate]) =
    if(to == next.from) to :: Nil
    else to :: next.from :: Nil

  def /(next: Route[Coordinate]): Route[Coordinate] = Route(from, next.to, steps ::: resolveToAndNextFrom(next) ::: next.steps)
  def /(next: Coordinate): Route[Coordinate] = Route(from, next, steps :+ next) // todo: mutable builder !!!
}

object Route{
  def apply[Coordinate](c: Coordinate): Route[Coordinate] = Route(c, c, c :: Nil)
}