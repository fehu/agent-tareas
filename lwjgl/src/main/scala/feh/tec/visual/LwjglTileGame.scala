package feh.tec.visual

import feh.tec.visual.api._
import feh.tec.map.tile.AbstractTile
import feh.tec.map.AbstractMap
import org.lwjgl.opengl.{GL11, DisplayMode, Display}
import feh.tec.visual.api.BasicDrawEnvironmentSettings
import nicol.{Pretransformed, View}
import feh.tec.visual.render.Lwjgl2DMapRenderer

/**
 * some parts are taken from Tom Streller's (scan) Nicol-0.1.2 project (https://github.com/scan/Nicol)
 * due to a lack of scala 2.10 compatible version
 */
class LwjglTileGame[Coord, T <: AbstractTile[T, Coord], M <: AbstractMap[T, Coord], E <: NicolLike2DEasel]
  (val map: M,
   val mapRenderer: Lwjgl2DMapRenderer[M, T, Coord, E],
   val mapDrawOps: E#MDrawOptions,
   val drawEnvSettings: BasicDrawEnvironmentSettings,
   val glSettings: BasicDrawEnvironmentGL11Settings,
   preCreateOpt: Option[() => Unit] = None
)(implicit val easel: E)
  extends TileGame
{
  sys.error("not used")

  def stop(): Unit = ???
  def start(): Unit = ???

  type TCoord = Coord
  type Tile = T
  type Map = M
  type EaselTpe = E
  type DrawSettings = BasicDrawEnvironmentSettings

  def gameLayout: Layout[LwjglTileGame[Coord, T, M, E]#EaselTpe] = ???

  implicit def easelCoordinateOps: EaselCoordinateOps[LwjglTileGame[Coord, T, M, E]#EaselTpe] = ???

  lazy val camera = new View

  implicit def easelCoordinateOps(easel: LwjglTileGame[Coord, T, M, E]#EaselTpe): EaselCoordinateOps[LwjglTileGame[Coord, T, M, E]#EaselTpe] = ???

  def render(l: Layout[LwjglTileGame[Coord, T, M, E]#EaselTpe])(implicit easel: LwjglTileGame[Coord, T, M, E]#EaselTpe): Unit = ???

  def prepareDrawEnvironment(ops: DrawSettings) {
    import Display._
    import ops._

    setTitle(title)
    setVSyncEnabled(true)

    val target = getAvailableDisplayModes.find { d =>
      d.getWidth == width && d.getHeight == height
    } getOrElse new DisplayMode(width, height)

    setDisplayMode(target)

    // Try fullscreen
    setFullscreen(fullscreen)

    create()

    glSettings.setSettings(ops)
  }

  def renderMap(ops: EaselTpe#MDrawOptions)(implicit easel: E) {
    render(Layout[EaselTpe](
      LayoutElem[Map, EaselTpe](map, easel.zeroCoordinate)(null) :: Nil
    ))

  }

  def preCreate()(implicit easel: EaselTpe) { preCreateOpt.foreach(_()) }
}

trait GL11Settings{
  type DrawSettings <: DrawEnvironmentSettings

  def setSettings(ops: DrawSettings)
}

trait BasicDrawEnvironmentGL11Settings extends GL11Settings{
  type DrawSettings = BasicDrawEnvironmentSettings
}

class CopiedFromNicolExampleGL11Settings extends BasicDrawEnvironmentGL11Settings{
  def setSettings(ops: DrawSettings) {
    import GL11._
    import ops._

    glEnable(GL_TEXTURE_2D)

    glDisable(GL_LIGHTING)
    glDisable(GL_DEPTH_TEST)

    glShadeModel(GL_SMOOTH)
    glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_FASTEST)

    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)

    glViewport(0, 0, width, height)

    glMatrixMode(GL_PROJECTION)
    glLoadIdentity()
    glOrtho(0, width, height, 0, 1, -1)
    glMatrixMode(GL_MODELVIEW)
    glLoadIdentity()
  }
}