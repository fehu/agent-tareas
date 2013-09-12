package feh.tec.agent

/**
 *
 * @param steps doesn't include `from` nor `to`
 */
case class Route[Coordinate](from: Coordinate, to: Coordinate, steps: Seq[Coordinate]){
  def length = steps.length + 1
}
