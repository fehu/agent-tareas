package feh.tec.util

import java.io.{File => JFile, InputStream, FileOutputStream, FileInputStream}
import scala.util.{Success, Try}
import scala.collection.mutable
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.io.Source

object FileUtils extends FileUtils
trait FileUtils {
  implicit class ByteArrayToFileWrapper(arr: Array[Byte]){
    def toFile(file: String): JFile = toFile(new JFile(file))
    def toFile(file: JFile): JFile = file $$ (_.withOutputStream(_.write(arr)))
  }

  implicit class StringToFileWrapper(str: String){
    def toFile: JFile = new JFile(str)
  }

  implicit class FileWrapper(file: JFile){
    def withOutputStream[R](f: FileOutputStream => R): Try[R] = {
      val stream = new FileOutputStream(file)
      Try(f(stream)) $$ { _ =>
        stream.close()
      }
    }
    def withInputStream[R](f: FileInputStream => R): Try[R] = {
      val stream = new FileInputStream(file)
      Try(f(stream)) $$ { _ =>
        stream.close()
      }
    }
    def mv(path: Path, `override`: Boolean = false) = Try{
      val dest = new JFile(path.toString)
      if(dest.exists()) if(`override`) dest.delete() else sys.error(s"destination file $dest exists")
      file.renameTo(dest)
      dest
    }

    def createIfNotExists() = Try{
      if(!file.exists()) File.createFile(Path.absolute(file))
      file
    }

    def copy(file: JFile, `override`: Boolean = false): Try[JFile] = cp(Path.absolute(file), `override`)
    def cp(path: Path, `override`: Boolean = false): Try[JFile] = Try{
      val dest = path.file
      if(dest.exists()) if(`override`) dest.delete() else sys.error(s"destination file $dest exists")
      dest.withOutputStream(File.write(file)).get
      dest
    }

    def affect(f: JFile => Unit) = Try{
      f(file)
      file
    }

    def existing[R](f: JFile => R): Option[R] = if(exists) Some(f(file)) else None

    def name = file.getName
    def ls(filter: JFile => Boolean = null) = file.listFiles().toList |>> (Option(filter), _.filter)
    def dir_? = file.isDirectory
    def isDir = file.isDirectory
    def exists = file.exists()
    def mkDir() = Try{
      file.mkdirs()
      file
    }

    def path = file.getAbsolutePath
  }

  trait Reader[T]{
    def read: InputStream => T
  }
  implicit object ByteArrayReader$ extends Reader[Array[Byte]]{
    def read: InputStream => Array[Byte] = File.readBytesFromStream(_)
  }
  implicit object StringReader$ extends Reader[String]{
    def read: InputStream => String = is => new String(File.readBytesFromStream(is))
  }

  import Path._

  object Resource{
    private def streamOpt(path: Path) = Option(getClass.getResourceAsStream(path.toString))

    def source(path: Path) = Option(getClass.getResource(path.toString)).map(Source.fromURL)
    def apply[R](path: Path)(f: InputStream => R): Try[R] = {
      val s = streamOpt(path)
      Try{ s.getOrElse(sys.error(s"no resource found: $path ")) |> f } $$ {s.foreach(_.close())}
    }
  }

  object File{
    @tailrec
    final def readBytesFromStream(instream: InputStream, acc: ListBuffer[Byte] = ListBuffer.empty): Array[Byte] =
      if(instream.available > 0) readBytesFromStream(instream, acc += instream.read.toByte) else acc.toArray

    def read[R: Reader](is: InputStream): R = implicitly[Reader[R]].read(is)
    def write(file: JFile): FileOutputStream => Unit = {
      stream =>
        file.withInputStream(read[Array[Byte]]).map{
          src => write(src)
        }
    }
    def write(bytes: Array[Byte]): FileOutputStream => Unit = {
      stream =>
        stream.write(bytes)
        stream.flush()
    }

    implicit def fileBuilderToFileWrapper(b: FileBuilder) = b.dir
    case class FileBuilder(dirPath: Path, isTemporary: Boolean)(val dir: JFile = dirPath.file){
      assert(dir.exists() && dir.isDirectory && dir.canWrite)
      if(isTemporary) dir.deleteOnExit()

      def createFile(path: Path, `override`: Boolean = isTemporary): Try[JFile] = File.createFile(path append dirPath)
      def createDir(path: Path) = path.file.affect(_.mkdirs().ensuring(x => x, s"couldn't create dir $path"))
    }

    protected[util] def crFile(path: Path) = path.file //.log("creating file " + _).file
      .affect(_.createNewFile().ensuring(x => x, "failed to create file"))

    def createFile(path: Path, `override`: Boolean = false): Try[JFile] = {
      def inner(path: Path, first: Boolean = false): Try[JFile] = path match {
        case Exists(Dir(dir)) =>
          Success(dir.file)
        case Exists(file) =>
          if(`override`) file.delete() flatMap (_ => inner(file))
          else sys.error(s"file $file exists")
        case / / file if first =>
          val f = file
          crFile(path)
        case rest / file if first =>
          val f = file
          inner(rest).flatMap{
            _ => crFile(path)
          }
        case rest / dir =>
          path.file.mkDir()
      }
      inner(path, first = true)
    }

    def temporaryDir(name: String, deleteOnExit: Boolean = true): FileBuilder = {
      assert(!name.contains(separatorChar))
      val dir = temporary(name, deleteOnExit)
      if(dir.exists()) assert(dir.delete(), "couldn't remove old temp directory")
      dir.mkdir()
      dir.mv(name)
      FileBuilder(Path.absolute(dir), deleteOnExit)(dir)
    }

    def temporary(path: Path, deleteOnExit: Boolean = true): JFile = path match {
      case Path(EmptyPath, name, ext) => JFile.createTempFile(name, ext)
      case / / dir / file =>
        val b = temporaryDir(dir, deleteOnExit)
        b.createFile(file, `override` = true).get
      case s => sys.error(s"path larger then 2 isn't supported $s")
    }
  }

  lazy val separator = JFile.separator
  lazy val separatorChar = JFile.separatorChar

  def file(path: Path): JFile = new JFile(path.toString)
  def dropExt(filename: String) = {
    val i = filename.lastIndexOf(".")
    if(i < 0) filename else filename.substring(0, i)
  }

  /**
   * extract unexisting part of path
   */
  object Unexisting{
    def unapply(path: Path): Option[Path] = {
      val p = path.tails.takeWhile(p => !p.file.exists()).toSeq
      val q = RelativePath(p.reverse.map(_.reversed.head))
      q.backOpt
    }

//    def apply(path: Path)
  }

  /**
   * extract unexisting part of path
   */
  object Existing{
    def unapply(path: Path): Option[Path] = path.tails.takeWhile(p => p.file.exists()).toSeq.lastOption
  }

  object Exists{
    def unapply(path: Path): Option[Path] = if(path.file.exists()) Some(path) else None
  }

  object Dir{
    def unapply(path: Path): Option[Path] = if(path.file.dir_?) Some(path) else None
  }


  object Path{
    implicit def toString(path: Path) = path.toString

    def absolute(file: JFile): AbsolutePath = AbsolutePath(file.getAbsolutePath)
    def absolute(file: String): AbsolutePath = AbsolutePath(file)

    def relative(file: JFile): RelativePath = RelativePath(file.getPath)
    def relative(file: String): RelativePath = RelativePath(file)

    def apply(list: List[String], absolute: Boolean): Path = if(absolute) AbsolutePath(list) else RelativePath(list)
    def apply(str: String): Path = {
      assert(str.nonEmpty && str != null, "path is empty")
      val (s, abs) = if(str(0) == separatorChar) str.drop(1) -> true else str -> false
      if(abs) AbsolutePath(s.split(separatorChar))
      else if(s(0) == '.' && s.substring(1).headOption == Some(separatorChar)) RelativePath(s.drop(2).split(separatorChar))
      else RelativePath(s.split(separatorChar))
    }
    // path, file, suffix
    def unapply(path: Path): Option[(Path, String, String)] =
      Some(path.back, path.splittedName._1, path.splittedName._2)

    /**
     * Use only for pattern matching
     */
    sealed trait EmptyPath extends Path{
      def emptyPathException(call: String) = sys.error(s"calling $call on EmptyPath")

      override def isEmpty = true
      def reversed = Nil
      def absolute: Boolean = emptyPathException("absolute")
      protected def cpy(path: List[String]): Path = emptyPathException("cpy")
      override def length = 0
      override def backOpt = None
      override def back = emptyPathException("back")
      override def file = emptyPathException("file")
      override def name = ""
      override def ext = ""
      override def splittedName = "" -> ""
      override def head = emptyPathException("head")
      override def tail = emptyPathException("tail")
      override def tails = Iterator.empty

      override def equals(obj: Any) = obj match{
          case p: Path if p.isEmpty => true
          case _ => false
        }
    }
    object EmptyPath extends EmptyPath{
      object Absolute extends AbsolutePath(Nil) with Absolute

      trait Absolute extends AbsolutePath with EmptyPath {
        override val reversed = Nil
        override val absolute = true
        override protected def cpy(path: List[String]) = AbsolutePath(path)
        override def /(next: RelativePath) = AbsolutePath(next)
        override def /(next: String) = AbsolutePath(next)
      }
      object Relative extends RelativePath(Nil) with Relative
      trait Relative extends RelativePath with EmptyPath {
        override val reversed = Nil
        override val absolute = false
        override protected def cpy(path: List[String]) = RelativePath(path)
        override def /(next: RelativePath) = RelativePath(next)
        override def /(next: String) = RelativePath(next)
      }

      def toAbsolute = Absolute
      def toRelative = Relative
    }

  }

  object / extends EmptyPath{

    def unapply(path: Path): Option[(Path, String)] = {
      path.backOpt map (_ -> path.name)
    }

    override def equals(obj: Any): Boolean = EmptyPath == obj

    override def / (p: String) = AbsolutePath(p)

    def toAbsolute = EmptyPath.Absolute
    def toRelative = EmptyPath.Relative
  }

  sealed trait Path{
    def reversed: List[String]
    def absolute: Boolean
    protected def cpy(path: List[String]): Path
    def isEmpty = reversed.isEmpty
    def length = reversed.length

    lazy val path = reversed.reverse
    def /(next: String): Path = cpy(next :: reversed)
    def /(next: RelativePath): Path = prepend(next)
    def back =
      if(length != 1) cpy(reversed.tail)
      else EmptyPath
    def backOpt = {
      if(length > 0) Some(back) else None
    }
    def prepend(next: Path) = cpy(next.reversed ::: this.reversed)
    def append(next: Path) = cpy(this.reversed ::: next.reversed)

    def file = new JFile(toString)
    def delete() = Try{file.delete()}
    def name = reversed.head
    def ext = splittedName._2
    def splittedName = {
      val split = name.split('.')
      if (split.length == 1) split.head -> "" 
      else split.dropRight(1).mkString(".") -> split.last
    }

    def intersects(that: Path) =  ???

    def head = name
    def tail = back

    def rHead = reversed.head
    def rTail = reversed.tail
    def tails: Iterator[Path] =
      if(length == 1) Iterator(this)
      else reversed.tails.map(p => Path(p.reverse, absolute))

    override def equals(obj: scala.Any): Boolean = PartialFunction.cond(obj) {
      case p: Path if p.absolute == this.absolute || p.reversed == this.reversed => true
    }
    def toAbsolute: AbsolutePath
    def toRelative: RelativePath
  }

  object `.` {
    def / (p: String) = RelativePath(p)
  }

  trait PathBuildHelper{
    protected def build(path: Seq[String]) = path.flatMap(_.split(separatorChar)).map(_.trim).filter(_.nonEmpty)
  }

  object RelativePath extends PathBuildHelper{
    def raw(seq: Seq[String]) = RelativePath(seq.mkString(" ").split(separatorChar))

    def apply(path: Seq[String]): RelativePath = build(path) match {
      case Nil => EmptyPath.Relative
      case p => new RelativePath(p.toList.reverse)
    }
    def apply(path: String): RelativePath = apply(path :: Nil)
  }
  class RelativePath protected[FileUtils](val reversed: List[String]) extends Path{
    def absolute = false
    protected def cpy(path: List[String]) = new RelativePath(reversed = path)

    def relToCurrentDir = "." + separator + toString

    override def toString: String = path.mkString(separator)

    def toAbsolute = new AbsolutePath(reversed)
    def toRelative = this
  }

  implicit def stringToRelativePath(filename: String): RelativePath = RelativePath(filename :: Nil)

  object AbsolutePath extends PathBuildHelper{
    def raw(seq: Seq[String]) = AbsolutePath(seq.mkString(" ").split(separatorChar))

    def apply(path: Seq[String]): AbsolutePath = build(path) match {
      case Nil => EmptyPath.Absolute
      case p => new AbsolutePath(p.toList.reverse)
    }
    def apply(path: String): AbsolutePath = this.apply(path :: Nil)
  }

  class AbsolutePath protected[FileUtils](val reversed: List[String]) extends Path{
    def absolute = true
    protected def cpy(path: List[String]) = new AbsolutePath(reversed = path)

    override def toString: String = path.mkString(separator, separator, "")

    def toAbsolute = this
    def toRelative = new RelativePath(reversed)
  }
}
