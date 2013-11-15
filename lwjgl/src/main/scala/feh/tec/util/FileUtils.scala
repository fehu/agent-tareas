package feh.tec.util

import java.io.{FileOutputStream, FileInputStream, File}

object FileUtils {
  implicit class ByteArrayToFileWrapper(arr: Array[Byte]){
    def toFile(file: String): File = toFile(new File(file))
    def toFile(file: File): File = file.withOutputStream(_.write(arr)).file
  }

  implicit class FileWrapper(file: File){
    case class FileResult[R](file: File, result: R, previous: Option[FileResult[_]] = None)

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

  def workingDir = LazyFile(".")
}

object LazyFile{
  protected [LazyFile] def filterPath(path: String) = path.split(File.separatorChar).filter(_.nonEmpty)

  def apply(path: String): LazyFile =
    new LazyFile(filterPath(path), path.head == File.separatorChar)

  implicit def lazyFileToFile(f: LazyFile): File = f.file

}

class LazyFile(val path: Seq[String], absolute: Boolean){
  private def separator = File.separator

  lazy val stringPath = (if(absolute) separator else "") + path.mkString(separator)

  lazy val file = new File(stringPath)

  def append(next: String*) = new LazyFile(path ++ next.flatMap(LazyFile.filterPath), absolute)
  def + = append _
}