package feh.tec.util

import scala.collection.SeqLike
import scala.util.Random

trait RandomWrappers {
  implicit class SeqLikeWrapper[+A, +Repr](seq: SeqLike[A, Repr]){
    def randomChoice: A = seq(Random.nextInt(seq.length))
  }
  implicit class MapLikeWrapper[A, +B, +This <: scala.collection.MapLike[A, B, This] with scala.collection.Map[A, B]](mlike: scala.collection.MapLike[A, B, This]){
    def randomChoose: (A, B) = {
      val k = mlike.keys.toList.randomChoice
      k -> mlike(k)
    }
  }

  implicit class SetLikeWrapper[A, Repr <: collection.SetLike[A, Repr] with Set[A]](set: collection.SetLike[A, Repr]){
    def randomChoose: A = set.toSeq.randomChoice
  }

  implicit class RangeWrapper(r: Range){
    def randomSelect: Int = {
      val n = Random.nextInt(r.length)
      r.min + r.step*n
    }
  }
}

object RandomWrappers extends RandomWrappers