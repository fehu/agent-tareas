package feh.tec.visual.util

import scala.swing.Swing
import java.awt.{Rectangle, Point, Dimension}


trait AwtUtils {

  implicit def dimensionToPair(dim: Dimension): (Int, Int) = dim.getWidth.toInt -> dim.getHeight.toInt

  implicit def pair2Dimension = Swing.pair2Dimension _
  implicit def pair2Point(p: (Int, Int)): Point = Swing.pair2Point(p)
  implicit def pair2Point(p: (Int, Int, Int, Int)): Rectangle = Swing.pair2Point(p)
}
