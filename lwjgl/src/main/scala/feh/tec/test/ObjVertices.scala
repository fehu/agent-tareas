package feh.tec.test

import org.lwjgl.util.vector.Vector3f
import feh.tec.test.BindBufferTarget.GL_ARRAY_BUFFER
import feh.tec.test.DataStoreUsagePattern.GL_STATIC_DRAW

object ObjVertices {
  object Pyramid {
    def vertices = Seq(
      // Front face
      vec(+0.0f, +0.5f, +0.0f),
      vec(-0.5f, -0.5f, +0.5f),
      vec(+0.5f, -0.5f, +0.5f),

      // Right face
      vec(+0.0f, +0.5f, +0.0f),
      vec(+0.5f, -0.5f, +0.5f),
      vec(+0.5f, -0.5f, -0.5f),

      // Back face
      vec(+0.0f, +0.5f, +0.0f),
      vec(+0.5f, -0.5f, -0.5f),
      vec(-0.5f, -0.5f, -0.5f),

      // Left face
      vec(+0.0f, +0.5f, +0.0f),
      vec(-0.5f, -0.5f, -0.5f),
      vec(-0.5f, -0.5f, +0.5f)
    )

    def colors = Seq(
      // Front face
      vec(1, 0, 0),
      vec(0, 1, 0),
      vec(0, 0, 1),

      // Right face
      vec(1, 0, 0),
      vec(0, 1, 0),
      vec(0, 0, 1),

      // Back face
      vec(1, 0, 0),
      vec(0, 1, 0),
      vec(0, 0, 1),

      // Left face
      vec(1, 0, 0),
      vec(0, 1, 0),
      vec(0, 0, 1)
    )

    def vbo = Vbo(vertices, colors)
  }

  object Cube{
    def vertices = Seq(
      // Front face
      vec(-0.5f, +0.5f, +0.5f),
      vec(+0.5f, +0.5f, +0.5f),
      vec(-0.5f, -0.5f, +0.5f),
      vec(+0.5f, -0.5f, +0.5f),

      // Right face
      vec(+0.5f, +0.5f, +0.5f),
      vec(+0.5f, +0.5f, -0.5f),
      vec(+0.5f, -0.5f, +0.5f),
      vec(+0.5f, -0.5f, -0.5f),

      // Back face
      vec(+0.5f, +0.5f, -0.5f),
      vec(-0.5f, +0.5f, -0.5f),
      vec(+0.5f, -0.5f, -0.5f),
      vec(-0.5f, -0.5f, -0.5f),

      // Left face
      vec(-0.5f, +0.5f, -0.5f),
      vec(-0.5f, +0.5f, +0.5f),
      vec(-0.5f, -0.5f, -0.5f),
      vec(-0.5f, -0.5f, +0.5f),

      // Top face
      vec(-0.5f, +0.5f, +0.5f),
      vec(+0.5f, +0.5f, +0.5f),
      vec(-0.5f, +0.5f, -0.5f),
      vec(+0.5f, +0.5f, -0.5f),

      // Bottom face
      vec(-0.5f, -0.5f, +0.5f),
      vec(+0.5f, -0.5f, +0.5f),
      vec(-0.5f, -0.5f, -0.5f),
      vec(+0.5f, -0.5f, -0.5f)
    )

    def colors = Seq(
      // Front face
      vec(1, 0, 0),
      vec(0, 1, 0),
      vec(0, 0, 1),
      vec(1, 0, 0),

      // Right face
      vec(0, 1, 0),
      vec(0, 0, 1),
      vec(1, 0, 0),
      vec(0, 1, 0),

      // Back face
      vec(0, 0, 1),
      vec(1, 0, 0),
      vec(0, 1, 0),
      vec(0, 0, 1),

      // Left face
      vec(1, 0, 0),
      vec(0, 1, 0),
      vec(0, 0, 1),
      vec(1, 0, 0),

      // Top face
      vec(0, 1, 0),
      vec(0, 0, 1),
      vec(1, 0, 0),
      vec(0, 1, 0),

      // Bottom face
      vec(0, 0, 1),
      vec(1, 0, 0),
      vec(0, 1, 0),
      vec(0, 0, 1)
    )

    def vbo = Vbo(vertices, colors)
  }

  private def Vbo = VBO(GL_ARRAY_BUFFER, GL_STATIC_DRAW) _
  private def vec(c: (Float, Float, Float)) = new Vector3f(c._1, c._2, c._3)

}
