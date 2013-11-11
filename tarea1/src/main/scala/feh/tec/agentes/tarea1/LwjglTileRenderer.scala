package feh.tec.agentes.tarea1

import feh.tec.visual._
import feh.tec.visual.api._
import java.awt.Color
import feh.tec.visual.render.{WorldObjectLwjglAtomDrawer, Generic2DLwjglContainerAtomDrawer, BasicLwjglSquareTileDrawer, LwjglCompositeAtom2DIntRenderer}
import feh.tec.world.WorldObject

object LwjglTileRenderer {
  type Tile = SqTile
  type TCoord = Map#Coordinate
  type Easel = Easel2DFloat with OpenGLEasel
  type MObj = MapObj

  def create = new LwjglCompositeAtom2DIntRenderer[Tile](renderers)

  def renderers =
    new Generic2DLwjglContainerAtomDrawer[Tile, TCoord, Easel, MObj](mapObjectDrawers) ::
    new BasicLwjglSquareTileDrawer[Tile, TCoord, Easel]  :: Nil

  def mapObjectDrawers = new MapObjDrawer :: Nil

  class MapObjDrawer extends WorldObjectLwjglAtomDrawer[Tile, TCoord, MObj]{
    def draw(obj: WorldObject, where: Easel2D#Coordinate, how: Easel2D#TDrawOptions)(implicit easel: Easel2D) = {
      def drawIt(txt: String, c: Color) = drawStringInTileWithOffset(txt, where, how, stringDrawOps(c))

      obj match{
        case AgentAvatar(_) =>      drawIt("A", Color.green)
        case Plug() =>              drawIt("P", Color.blue)
        case Hole() =>              drawIt("O", Color.red)
      }
    }

    def stringDrawOps(color: Color)(implicit easel: Easel2D): Easel2D#StrDrawOptions =
      BasicStringDrawOps(
        StringAlignment.Center,
        color,
        font = "Arial",
        size = easel.unitNumeric.fromInt(12),
        vSpacing = easel.unitNumeric.fromInt(3)
      ).asInstanceOf[Easel2D#StrDrawOptions] // todo ??


    def drawStringInTileWithOffset(str: String,
                                   where: Easel2D#Coordinate,
                                   how: Easel2D#TDrawOptions,
                                   strDrawOps: Easel2D#StrDrawOptions)(implicit easel: Easel2D) = {
      import easel._

      implicit def num = easel.unitNumeric .asInstanceOf[Numeric[Easel2D#CoordinateUnit]]

      implicit def coordinateWrapper: Easel2D#Coordinate => easel.Coordinate = _.asInstanceOf[easel.Coordinate]

      val newCoord: Coordinate = how match {
        case sq: SquareTileDrawOptions[Easel2D] => num match {
          case integral: Integral[Easel2D#CoordinateUnit] =>
            val halfSide = integral.quot(sq.sideSize, integral.fromInt(2))
            where.ops plusNum halfSide
          case fract: Fractional[Easel2D#CoordinateUnit] =>
            val halfSide = fract.div(sq.sideSize, fract.fromInt(2))
            where.ops plusNum halfSide
        }
        case _ => where.asInstanceOf[Coordinate]
      }

     easel.drawString(str, newCoord, strDrawOps.asInstanceOf[StrDrawOptions])
    }
  }
}
