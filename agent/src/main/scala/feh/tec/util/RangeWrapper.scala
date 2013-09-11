package feh.tec.util

import scala.util.Random

trait RangeWrapper {
  implicit class TheRangeWrapper(r: Range){
    def randomSelect: Int = {
      val n = Random.nextInt(r.length)
      r.min + r.step*n
    }
  }
}

object RangeWrapper extends RangeWrapper