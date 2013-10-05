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
}
