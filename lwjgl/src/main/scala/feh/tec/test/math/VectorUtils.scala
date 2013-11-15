package feh.tec.test.math

import org.lwjgl.util.vector.Vector3f

trait VectorUtils {

  def updateVector(vec: Vector3f, upd: (Float, Float, Float), op: ((Float, Float)) => Float){
    updateVector(vec, upd)(Function.untupled(op))
  }
  def updateVector(vec: Vector3f, upd: (Float, Float, Float))(op: (Float, Float) => Float){
    vec.x = op(vec.x, upd._1)
    vec.y = op(vec.y, upd._2)
    vec.z = op(vec.z, upd._3)
  }
}

object VectorUtils extends VectorUtils
