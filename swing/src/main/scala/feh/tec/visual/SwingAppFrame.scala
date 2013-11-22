package feh.tec.visual

import feh.tec.visual.api.{AppBasicControlApi, AwtWindowedApp}
import scala.swing._
import feh.tec.visual.SwingAppFrame.{ComponentAccess, LayoutDSL}
import scala.swing.ScrollPane.BarPolicy
import java.awt.Color

trait SwingAppFrame extends Frame with AwtWindowedApp with AppBasicControlApi with LayoutDSL{
  def appWindow = peer

  def componentAccess: ComponentAccess
}

object SwingAppFrame extends FormCreation{
  trait LayoutDSL extends FormCreationDSL{
    def layout: List[AbstractLayoutSetting]

    protected def componentAccess: ComponentAccess
    // upper lever settings
    protected def split(orientation: scala.swing.Orientation.Value)
                       (leftOrDown: List[LayoutSetting])(rightOrUp: List[LayoutSetting]) = SplitLayout(orientation, leftOrDown.toList, rightOrUp.toList)
    protected def set(s: UpperLevelLayoutGlobalSetting) = s

    // normal settings
    protected def place[C <% Component](what: C, id: String) = DSLPlacing(what, id)
    protected def make(s: LayoutGlobalSetting) = s

    //

    val tst1_t = new TextField("a")

    def tst1: List[AbstractLayoutSetting] =
      set(Title("title")) and
      split(Orientation.Vertical)(
        make(Scrollable()) and
        (place(tst1_t, "text-field-1") at LeftTopCorner) and
        (place(monitorFor(A.x).label.affect(_.foreground = Color.red), "label-1") to theNorth of "label-1") and
        (place(monitorFor(A.x).label, "label-2") to theSouth of tst1_t)
      )(
        place(new TextField("b"), "text-field-2").at(theCenter) :: Nil // todo setting to list wrapper
      )

    protected case class DSLPlacing(what: Component, id: String){
      def to(pos: DSLAbsolutePosition): LayoutElem = LayoutElem(what, id, pos)
      def to(rel: DSLRelativePositionDirection): DSLRelativelyOf = DSLRelativelyOf(what, id, rel)
      def at(pos: DSLAbsolutePosition): LayoutElem = to(pos)
      def at(rel: DSLRelativePositionDirection): DSLRelativelyOf = to(rel)
      def in(pos: DSLAbsolutePosition): LayoutElem = to(pos)
      def in(rel: DSLRelativePositionDirection): DSLRelativelyOf = to(rel)
    }

    protected case class DSLRelativelyOf(what: Component, id: String, dir: DSLRelativePositionDirection){
      def of(rel: String): LayoutElem = LayoutElem(what, id, DSLRelativePosition(dir, rel))
      def of[C <% Component](c: C) = LayoutElem(what, id, DSLRelativePosition(dir, componentAccess.id(c)))
    }

    protected implicit class AbstractLayoutSettingWrapper(list: List[AbstractLayoutSetting]){
      def and(other: AbstractLayoutSetting) = list :+ other
    }

    protected implicit class LayoutSettingListWrapper(list: List[LayoutSetting]){
      def and(other: LayoutSetting) = list :+ other
    }
  }

  sealed trait AbstractLayoutSetting
  trait UpperLevelLayoutSetting extends AbstractLayoutSetting{
    def and(that: UpperLevelLayoutSetting) = this :: that :: Nil
  }
  trait UpperLevelLayoutGlobalSetting extends UpperLevelLayoutSetting

  trait LayoutSetting extends AbstractLayoutSetting{
    def and(that: LayoutSetting) = this :: that :: Nil
    def and(that: List[LayoutSetting]) = this :: that
  }
  trait LayoutGlobalSetting extends LayoutSetting

  // // //// // //// // //// // //// // //  Upper Level Layout Settings  // // //// // //// // //// // //// // //

  case class SplitLayout(orientation: scala.swing.Orientation.Value,
                        leftOrDown: List[LayoutSetting],
                        rightOrUp: List[LayoutSetting]) extends UpperLevelLayoutSetting

  case class Title(title: String) extends UpperLevelLayoutGlobalSetting

  // // //// // //// // //// // //// // //  Layout Settings  // // //// // //// // //// // //// // //

  case class LayoutElem(what: Component, id: String, pos: DSLAbsolutePosition) extends LayoutSetting

  protected object LayoutElem{
    def apply(what: Component, id: String, pos: DSLPosition): LayoutElem = ???
  }

  case class Scrollable(vert: BarPolicy.Value = BarPolicy.AsNeeded,
                        hor: BarPolicy.Value = BarPolicy.AsNeeded) extends LayoutGlobalSetting

  // // //// // //// // //// // //// // //  Positions  // // //// // //// // //// // //// // //

  sealed trait DSLPosition
  trait DSLAbsolutePosition extends DSLPosition
  case class DSLRelativePosition(dir: DSLRelativePositionDirection, relTo: String) extends DSLPosition
  trait DSLRelativePositionDirection

  case object Center extends DSLAbsolutePosition
  def theCenter = Center
  case object LeftTopCorner extends DSLAbsolutePosition
  def theLeftTopCorner = LeftTopCorner 


  case object West extends DSLRelativePositionDirection
  def theWest = West
  case object East extends DSLRelativePositionDirection
  def theEast = East
  case object North extends DSLRelativePositionDirection
  def theNorth = North
  case object South extends DSLRelativePositionDirection
  def theSouth = South

  trait ComponentAccess{
    def apply(id: String): Component
    def get(id: String): Option[Component]

    def layoutOf(id: String): LayoutElem
    def getLayoutOf(id: String): LayoutElem

    def id[C <% Component](c: C): String
    def getId[C <% Component](c: C): Option[String]
  }

}