package feh.tec.util

import scala.collection.SeqLike
import scala.util.Random

trait RandomWrappers {
  implicit class SeqLikeWrapper[+A, +Repr](seq: SeqLike[A, Repr]){
    def randomChoose: A = seq(Random.nextInt(seq.length))
  }
}

object RandomWrappers extends RandomWrappers