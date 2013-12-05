package feh.tec.util.build

import feh.tec.util.FileUtils._
import spray.json._
import feh.tec.util.{OptionWrapper, TrySeqWrapper, JsonImplicits}

object LwjglResourcesPathResolver{
  /**
   * as in LWJGLPlugin
   * https://github.com/philcali/sbt-lwjgl-plugin/blob/master/src/main/scala/LWJGLPlugin.scala
   */
  def resolveLwjglResourcesPrefix = sys.props("os.name").toLowerCase.take(3) match{
    case "lin" => "linux"
    case "mac" | "dar" => "osx" // fixed
    case "win" => "windows"
    case "sun" => ???;  "solaris" // resources not copied
    case _ => throw new RuntimeException("couldn't determine os to prepare lwjgl resources")
  }
}


import LwjglResourcesPathResolver._

trait LwjglResourcesPathResolver {
  val resourceMapper = JsonResourceMapper

  protected def resolveLwjglResourcesPathBase = / / "lwjgl-resources"

  val libPathProp = "org.lwjgl.librarypath"

  def lwjglResourcesPath = resolveLwjglResourcesPathBase / resolveLwjglResourcesPrefix
  def prepareLwjglResources(path: Path, desiredTempDir: String) {
    val resources = resourceMapper.read()
    println("resources: " + resources)
    val tmpDir = File.temporaryDir(desiredTempDir)
    resources.map{
      case (filename, bytes) =>
        val file = tmpDir.createFile(Path.absolute(filename), `override` = true)
        val f = file.flatMap{
          _.withOutputStream(File.write(bytes))
        }
        println(s"created $filename in $tmpDir - $f, $file")
        f
    }.flat

    sys.props(libPathProp) = tmpDir.dir.toString
    println("copied lwjgl resources to " + tmpDir.dir.toString)
  }
}


trait LwjglAppsRunner extends AppsRunner with LwjglResourcesPathResolver{
  sys.props.getOrElse(libPathProp, prepareLwjglResources(lwjglResourcesPath, "lwjgl-resources-"))
}

trait ResourceMapper{
  def write(resourcesBase: Path, lwjgl: RelativePath, resourceMap: RelativePath)
  def read(resourceMap: Path): Seq[(String, Array[Byte])]
}

object JsonResourceMapper extends ResourceMapper with JsonImplicits{
  lazy val uuid = "3ca3d3ed-ecbc-449f-9c32-1ef7b09c0b39"

  def write(resourcesBase: Path, lwjgl: RelativePath, resourceMap: RelativePath){
    val lwjglBase = (resourcesBase / lwjgl).file
    val listed =
      for{
        platform <- lwjglBase.ls()
        if platform.dir_?
        file <- platform.ls()
      } yield platform.name -> file.name

    val json = genJson(lwjgl, listed.groupBy(_._1).mapValues(_.map(_._2)))
    val mapFile = (resourcesBase / resourceMap).file.createIfNotExists()
    mapFile.map(json.prettyPrint.getBytes.toFile)
    println(s"created lwjgl resource map in ${Path.relative(mapFile.get)}")
  }


  def read(_resourceMap: Path = mapPath): Seq[(String, Array[Byte])] = {
    val resourceMap = _resourceMap.toAbsolute
    val read = Resource(resourceMap)(File.read[String]).orElse{
      resourceMap.file.existing(_.withInputStream(File.read[String])).getOrThrow(s"file $resourceMap doesn't exist")
    }.get
    val json = read.asJson
    val (base, map) = parseJson(json)
    val prefix = resolveLwjglResourcesPrefix
    val Some(platform) = map.keySet.find(_.contains(prefix))
    println("resourcesBasePrefix = " + resourcesBasePrefix)
    val origPaths = map(platform)
    val paths = origPaths.map(resourcesBasePrefix / base / platform /)
    val (names, fileTries) = paths.zipMap(Resource(_)(File.read[Array[Byte]]))
      .mapZipIn2(origPaths)((k, tr, orig) => orig -> tr.orElse(k.file.withInputStream(File.read[Array[Byte]])))
      .unzip
    val files = fileTries.flat.get
    names zip files
  }

  def genJson(base: Path, listed: Map[String, Seq[String]]) = JsObject(
    "uuid" -> uuid,
    "base" -> base.toString,
    "platforms" -> JsObject(listed.toSeq.map{
      case (platform, files) => platform -> files : JsField
    }: _*)
  )

  def parseJson(json: JsValue) = json match{
    case JsObject(fields) =>
      assert(fields("uuid") == JsString(uuid), "uuid not valid")
      val JsString(base) = fields("base")
      val files =  fields("platforms") match {
        case JsObject(platforms) => platforms.map{
          case (platform, JsArray(files)) => platform -> files.map{
            case JsString(file) => file
          }
        }
      }
      base -> files
  }

  lazy val resourcesBasePrefix = Path.absolute(sys.props.get("feh.lwjgl.resources-base-prefix").getOrElse(""))
  lazy val mapPath = Path.relative(sys.props.get("feh.lwjgl.resources-map").getOrElse("feh/lwjgl/resources-map"))
}

object JsonResourceMapperExecutable extends App{
  args match {
    case Array(base, lwjgl) => JsonResourceMapper.write(Path(base), lwjgl, JsonResourceMapper.mapPath)
  }

}