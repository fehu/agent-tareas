package feh.tec.test.math

import org.lwjgl.util.vector.{Vector2f, Vector4f, Vector3f}

trait VectorUtils {

  def updateVector(vec: Vector3f, upd: (Float, Float, Float), op: ((Float, Float)) => Float){
    updateVector(vec, upd)(Function.untupled(op))
  }
  def updateVector(vec: Vector3f, upd: (Float, Float, Float))(op: (Float, Float) => Float){
    vec.x = op(vec.x, upd._1)
    vec.y = op(vec.y, upd._2)
    vec.z = op(vec.z, upd._3)
  }

  implicit class FloatTuple3Wrapper(wrapped: (Float, Float, Float)){
    def toVector = new Vector3f(wrapped._1, wrapped._2, wrapped._3)
  }

  implicit class FloatTuple4Wrapper(wrapped: (Float, Float, Float, Float)){
    def *(t: (Float, Float, Float, Float)): Float  = wrapped._1*t._1 + wrapped._2*t._2 + wrapped._3*t._3 + wrapped._4*t._4
//    def *(t: (Int, Int, Int, Int)): Float          = wrapped._1*t._1 + wrapped._2*t._2 + wrapped._3*t._3 + wrapped._4*t._4
    def *(t: Vector4f): Float                      = *(t.x, t.y, t.z, t.w)
  }

  implicit class Vector2fWrapper(wrapped: Vector2f){
    def toTuple = (wrapped.x, wrapped.y)
  }

  implicit class Vector3fWrapper(wrapped: Vector3f){
    def to4d(w: Float) = new Vector4f(wrapped.x, wrapped.y, wrapped.z, w)
    def toTuple = (wrapped.x, wrapped.y, wrapped.z)
  }

  implicit class Vector4fWrapper(wrapped: Vector4f){
    def withoutW = new Vector3f(wrapped.x, wrapped.y, wrapped.z)
  }
}

object VectorUtils extends VectorUtils
