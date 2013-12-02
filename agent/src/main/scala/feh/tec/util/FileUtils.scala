package feh.tec.util

import java.io.{FileOutputStream, FileInputStream, File => JFile}
import scala.util.Try

object FileUtils extends FileUtils
trait FileUtils {
  implicit class ByteArrayToFileWrapper(arr: Array[Byte]){
    def toFile(file: String): JFile = toFile(new JFile(file))
    def toFile(file: JFile): JFile = file.withOutputStream(_.write(arr)).file
  }

  implicit class FileWrapper(file: JFile){
    case class FileResult[R](file: JFile, result: R, previous: Option[FileResult[_]] = None)

    protected implicit def resultToContainer[R](res: R) = FileResult(file, res)

    def withOutputStream[R](f: FileOutputStream => R): FileResult[R] = {
      val stream = new FileOutputStream(file)
      try f(stream)
      finally stream.close()
    }
    def withInputStream[R](f: FileInputStream => R): FileResult[R] = {
      val stream = new FileInputStream(file)
      try f(stream)
      finally stream.close()
    }
  }

  type JFile = java.io.File

  class File(path: String){
    lazy val peer = new JFile(path)

    def mv(path: Path) = Try{
      peer.renameTo(new JFile(path.toString))
    }
    def withOutputStream[R](f: FileOutputStream => R) = peer.withOutputStream(f)
    def withInputStream[R](f: FileInputStream => R) = peer.withInputStream(f)
  }

  lazy val separator = JFile.separator
  lazy val separatorChar = JFile.separatorChar

  def file(path: Path): File = new File(path.toString)
  def dropExt(filename: String) = {
    val i = filename.lastIndexOf(".")
    if(i < 0) filename else filename.substring(0, i)
  }

  sealed trait Path{
    def reversedPath: List[String]
    def absolute: Boolean
    protected def cpy(path: List[String]): Path

    protected def path = reversedPath.reverse
    def /(next: String) = cpy(next :: reversedPath)
    def back = cpy(reversedPath.tail)

    override def toString: String = (if(absolute) separator else "") + path.mkString(separator)
  }

  object RelativePath{
    def raw(seq: Seq[String]) = RelativePath(seq.mkString(" ").split(separatorChar))

    def apply(path: Seq[String]): RelativePath = new RelativePath(path.reverse.toList)
  }
  case class RelativePath protected[FileUtils](reversedPath: List[String]) extends Path{
    def absolute = false
    protected def cpy(path: List[String]) = copy(reversedPath = path)
  }

  implicit def stringToRelativePath(filename: String): RelativePath = RelativePath(filename :: Nil)
}
