package feh.tec.test.math

import org.lwjgl.util.vector.{Vector4f, Matrix4f}
import scala.Array
import feh.tec.util.PipeWrapper
import org.lwjgl.BufferUtils

trait MatrixUtils extends VectorUtils{
  implicit class Matrix4fWrapper(mat: Matrix4f){
    def rows: Array[Array[Float]] = Array(
      Array(mat.m00, mat.m01, mat.m02, mat.m03),
      Array(mat.m10, mat.m11, mat.m12, mat.m13),
      Array(mat.m20, mat.m21, mat.m22, mat.m23),
      Array(mat.m30, mat.m31, mat.m32, mat.m33)
    )

    // by rows
    def asArray = rows.flatten

    // by rows
    def asBuffer = {
      val buff = BufferUtils.createFloatBuffer(16)
      rows foreach buff.put
      buff.flip
      buff
    }

    def multiply(v: Vector4f): Vector4f = mat.rows.map{
      case Array(x, y, z, w) => (x, y, z, w) * v
    } |>  {
      case Array(x, y, z, w) => new Vector4f(x, y, z, w)
    }
    def * = multiply _
  }
}


object MatrixUtils extends MatrixUtils