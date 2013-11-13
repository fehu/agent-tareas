package feh.tec.test

import scala.io.Source
import org.lwjgl.opengl.{GL11, GL20}
import org.lwjgl.util.vector.Matrix4f
import feh.tec.test.MatrixUtil.MatrixToFloatBufferWrapper

class ShaderProgram(vert: Source, frag: Source) {
  val programId = GL20.glCreateProgram

  // Create the shader and set the source
  val vertexShaderId = GL20.glCreateShader(GL20.GL_VERTEX_SHADER)
  val fragmentShaderId = GL20.glCreateShader(GL20.GL_FRAGMENT_SHADER)

  init(vertexShaderId, vert.getLines().mkString("\n"))
  init(fragmentShaderId, frag.getLines().mkString("\n"))

  protected def init(shaderId: Int, source: String){
    GL20.glShaderSource(shaderId, source)

    // Compile the shader
    GL20.glCompileShader(shaderId)

    // Check for errors
    if(GL20.glGetShaderi(shaderId, GL20.GL_COMPILE_STATUS) == GL11.GL_FALSE) {
      dispose()
      sys.error(s"vertex shader compilation failed, $source")
    }

    GL20.glAttachShader(programId, shaderId)
  }

  def linked() = {
    link()
    this
  }

  def link(){
    // Link this program
    GL20.glLinkProgram(programId);

    // Check for linking errors
    val res = GL20.glGetProgrami(programId, GL20.GL_LINK_STATUS)
    println(s"linking result: $res")
    if (res == GL11.GL_FALSE){
      dispose()
      sys.error(s"failed to link shader program, $vert")
    }
  }

  def bind(){
    GL20.glUseProgram(programId)
  }

  def unbind(){
    GL20.glUseProgram(0)
  }

  def dispose(){
    // Unbind the program
    unbind()

    // Detach the shaders
    GL20.glDetachShader(programId, vertexShaderId)
    GL20.glDetachShader(programId, fragmentShaderId)

    // Delete the shaders
    GL20.glDeleteShader(vertexShaderId)
    GL20.glDeleteShader(fragmentShaderId)

    // Delete the program
    GL20.glDeleteProgram(programId)
  }

  def setUniform(name: String, value: Matrix4f){
    GL20.glUniformMatrix4(GL20.glGetUniformLocation(programId, name), false, value.toFloatBuffer)
  }
}
