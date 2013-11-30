package feh.tec.visual

import feh.tec.visual.api.{AppBasicControlApi, AwtWindowedApp}
import scala.swing._
import scala.swing.ScrollPane.BarPolicy
import java.awt.{Graphics, Color}
import feh.tec.visual.util.AwtUtils
import javax.swing.{JPanel, JComponent}
import scala.collection.mutable
import java.util.UUID
import feh.tec.util._


trait SwingAppFrame extends Frame with AppBasicControlApi with SwingFrameAppCreation{
  self: SwingFrameAppCreation#LayoutDSL with SwingFrameAppCreation#LayoutBuilder =>
}

object SwingFrameAppCreation extends SwingFrameAppCreation
trait SwingFrameAppCreation extends FormCreation{
  trait LayoutBuilder{
    dsl: LayoutDSL =>
    
    /**
     * builds layout within itself
     */
    def buildLayout()
  }
  
  trait LayoutDSL extends FormCreationDSL{
    val layout: List[AbstractLayoutSetting]

    val componentAccess = new RegistringComponentAccess

    // upper lever settings
    protected def split(orientation: scala.swing.Orientation.Value)(leftOrDown: List[LayoutSetting])(rightOrUp: List[LayoutSetting]) =
      SplitLayout(orientation, leftOrDown.toList, rightOrUp.toList)
    protected def set(s: UpperLevelLayoutGlobalSetting) = s
    protected def panel = new PanelChooser

    // normal settings
    protected def place[C <% Component](what: C, id: String): DSLPlacing = DSLPlacing(what, id)
    protected def place[T](builder: DSLFormBuilder[T], id: String): DSLPlacing = DSLPlacing(DSLFormBuilderWrapper(builder).build, id)
    protected def place(meta: PanelMeta): DSLPlacing = DSLPlacing(meta.panel, meta.id)
    protected def make(s: LayoutGlobalSetting) = s

    protected case class DSLPlacing(what: Component, id: String){
      def to(pos: DSLAbsolutePosition): LayoutElem = LayoutElem(what, id, pos).register
      def to(rel: DSLRelativePositionDirection): DSLRelativelyOf = DSLRelativelyOf(what, id, rel)
      def at(pos: DSLAbsolutePosition): LayoutElem = to(pos)
      def at(rel: DSLRelativePositionDirection): DSLRelativelyOf = to(rel)
      def in(pos: DSLAbsolutePosition): LayoutElem = to(pos)
      def in(rel: DSLRelativePositionDirection): DSLRelativelyOf = to(rel)
      def on(rel: DSLRelativePositionDirection): DSLRelativelyOf = to(rel)
    }

    protected case class DSLRelativelyOf(what: Component, id: String, dir: DSLRelativePositionDirection){
      def of(rel: String): LayoutElem = LayoutElem(what, id, DSLRelativePosition(dir, rel)).register
      def of[C <% Component](c: C) = LayoutElem(what, id, DSLRelativePosition(dir, componentAccess.id(c))).register
    }

    protected implicit class AbstractLayoutSettingWrapper(list: List[AbstractLayoutSetting]){
      def and(other: AbstractLayoutSetting) = list :+ other
    }

    protected implicit class LayoutSettingListWrapper(list: List[LayoutSetting]){
      def and(other: LayoutSetting) = list :+ other
    }
    protected implicit def layoutSettingToListWrapper(s: AbstractLayoutSetting): List[AbstractLayoutSetting] = s :: Nil

    protected[SwingFrameAppCreation] implicit class LayoutElemRegister(elem: LayoutElem){
      def register = {
        componentAccess.register(elem)
        elem
      }
    }

    protected implicit def componentIdPairToUnplacedLayoutElem[C <% Component](p: (C, String)): UnplacedLayoutElem = {
      val el = LayoutElem.unplaced(p._1, p._2)
      el.register
      el
    }

    protected class PanelChooser{
      def flow[E <% UnplacedLayoutElem](alignment: FlowPanel.Alignment.type => FlowPanel.Alignment.Value)(elems: E*) =
        FlowPanelMeta(alignment(FlowPanel.Alignment), elems.toList.map(x => x: UnplacedLayoutElem))
      def box[E <% UnplacedLayoutElem](alignment: Orientation.type => Orientation.Value)(elems: E*) =
        BoxPanelMeta(alignment(Orientation), elems.toList.map(x => x: UnplacedLayoutElem))
    }

    class RegistringComponentAccess extends ComponentAccess{
      private val componentsMap = mutable.HashMap.empty[String, LayoutElem]
      protected[LayoutDSL] def register(elem: LayoutElem){
        componentsMap += elem.id -> elem
      }

      def get(id: String) = componentsMap.get(id).map(_.component)
      def getLayoutOf(id: String) = componentsMap.get(id)
      def getId[C <% Component](c: C) = {
        val comp = c: Component
        componentsMap.find(_._2.component == comp).map(_._1)
      }
      def all: Seq[LayoutElem] = componentsMap.values.toSeq
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
  case class Size(size: (Int, Int)) extends UpperLevelLayoutGlobalSetting

  // // //// // //// // //// // //// // //  Layout Settings  // // //// // //// // //// // //// // //

  case class LayoutElem(component: Component, id: String, pos: DSLPosition) extends LayoutSetting
  object LayoutElem{
    def unplaced(c: Component, id: String): UnplacedLayoutElem = new LayoutElem(c, id, null) with UnplacedLayoutElem 
  } 
  
  trait UnplacedLayoutElem extends LayoutElem{
    override val pos = Undefined  
  }

  /*  protected object LayoutElem{
      def apply(what: Component, id: String, pos: DSLPosition): LayoutElem = ???
    }*/

  case class Scrollable(vert: BarPolicy.Value = BarPolicy.AsNeeded,
                        hor: BarPolicy.Value = BarPolicy.AsNeeded) extends LayoutGlobalSetting


  trait PanelMeta extends LayoutSetting{
    type Panel <: scala.swing.Panel
    def panelName: String
    def elems: List[LayoutSetting]
    def panel: Panel

    def id: String = s"$panelName: ${UUID.randomUUID().toString}"

    def affect(effects: (Panel => Unit)*): PanelMeta
  }

  case class FlowPanelMeta(alignment: FlowPanel.Alignment.Value,
                           elems: List[LayoutElem],
                           panelName: String = "FlowPanel",
                           protected val effects: List[FlowPanelMeta#Panel => Unit] = Nil) extends PanelMeta
  {

    type Panel = FlowPanel
    def panel = {
      val p = new FlowPanel(alignment)(elems.map(_.component): _*)
      effects.foreach(_(p))
      p
    }
    def affect(effects: (FlowPanelMeta#Panel => Unit) *): PanelMeta = copy(effects = this.effects ++ effects)
  }
  case class BoxPanelMeta(alignment: Orientation.Value,
                          elems: List[LayoutElem],
                          indent: Option[Int] = Some(10),
                          panelName: String = "BoxPanel",
                          protected val effects: List[BoxPanelMeta#Panel => Unit] = Nil) extends PanelMeta
  {
    type Panel = BoxPanel

    def indent(i: Option[Int]): BoxPanelMeta = copy(indent = i)

    def elem(el: LayoutElem*) = copy(elems = elems ++ el)
    def glue = copy(elems = elems :+ LayoutElem.unplaced(createGlue, null))
    def strut(i: Int) = copy(elems = elems :+ LayoutElem.unplaced(createStrut(i), null))

    def panel = {
      val p = new BoxPanel(alignment)
      val contents = Y[List[Component], List[Component]](
        rec => {
          case Nil => Nil
          case x :: Nil => List(x)
          case x :: y :: tail => x :: createGlue :: y :: rec(tail)
        }
      )(elems.map(_.component))

      p.contents ++= indent.map{
        i => createStrut(i) :: contents ::: List(createStrut(i))
      } getOrElse contents

      effects.foreach(_(p))
      p
    }
    def affect(effects: (BoxPanelMeta#Panel => Unit) *): PanelMeta = copy(effects = this.effects ++ effects)

    protected def createGlue = alignment match {
      case Orientation.Horizontal => Swing.HGlue
      case Orientation.Vertical => Swing.VGlue
    }
    protected def createStrut(i: Int) = alignment match{
      case Orientation.Horizontal => Swing.HStrut(i)
      case Orientation.Vertical => Swing.VStrut(i)
    }
  }

  // // //// // //// // //// // //// // //  Positions  // // //// // //// // //// // //// // //

  sealed trait DSLPosition{
    def isRelative: Boolean
    final def isAbsolute = !isRelative

    def isDefined: Boolean
  }
  case object Undefined extends DSLPosition{ def isRelative = false; final def isDefined = false }
  trait DSLAbsolutePosition extends DSLPosition{ final def isRelative = false ; final def isDefined = true}
  case class DSLRelativePosition(dir: DSLRelativePositionDirection, relTo: String) extends DSLPosition{
    final def isRelative = true ; final def isDefined = true
  }
  trait DSLRelativePositionDirection

  trait ComponentAccess{
    def apply(id: String): Component = get(id).get
    def get(id: String): Option[Component]

    def layoutOf(id: String): LayoutElem = getLayoutOf(id).get
    def getLayoutOf(id: String): Option[LayoutElem]

    def id[C <% Component](c: C): String = getId(c).get
    def getId[C <% Component](c: C): Option[String]
    
    def all: Seq[LayoutElem]

    def collect[R](f: PartialFunction[LayoutElem, R]): Seq[R] = all.collect(f)
  }

  trait ComponentAccessBuilder{
    def build(layout: List[AbstractLayoutSetting]):ComponentAccess
  }

  // // //// // //// // //// // //// // //  Impl  // // //// // //// // //// // //// // //

  trait Layout9PositionsDSL extends LayoutDSL{

    case object Center extends DSLAbsolutePosition
    case object North extends DSLAbsolutePosition
    case object South extends DSLAbsolutePosition
    case object West extends DSLAbsolutePosition
    case object East extends DSLAbsolutePosition
    case object NorthWest extends DSLAbsolutePosition
    case object NorthEast extends DSLAbsolutePosition
    case object SouthWest extends DSLAbsolutePosition
    case object SouthEast extends DSLAbsolutePosition
    def theCenter = Center
    def theNorth = North
    def theSouth = South
    def theWest = West
    def theEast = East
    def theNorthWest = NorthWest
    def theNorthEast = NorthEast
    def theSouthWest = SouthWest
    def theSouthEast = SouthEast

    // todo: diagonals
    case object Left extends DSLRelativePositionDirection
    case object Right extends DSLRelativePositionDirection
    case object Up extends DSLRelativePositionDirection
    case object Down extends DSLRelativePositionDirection
    def theLeft = Left
    def theRight = Right
    def Top = Up
    def theTop = Up
    def Bottom = Down
    def theBottom = Down


    val tst1_t = new TextField("a")

    def tst1: List[AbstractLayoutSetting] =
      set(Title("title")) and
        split(Orientation.Vertical)(
          make(Scrollable()) and
            (place(tst1_t, "text-field-1") at NorthWest) and
            (place(monitorFor(A.x).label.affect(_.foreground = Color.red), "label-1") to Top of "label-1") and
            (place(monitorFor(A.x).label, "label-2") to Bottom of tst1_t)
        )(
          place(new TextField("b"), "text-field-2").at(theCenter) :: Nil // todo setting to list wrapper
        )
  }


  protected def collectDsl[R](l: List[AbstractLayoutSetting], func: PartialFunction[AbstractLayoutSetting, R]): List[R] = l match{
    case Nil => Nil
    case SplitLayout(_, leftOrDown, rightOrUp) :: tail => collectDsl(leftOrDown, func) ::: collectDsl(rightOrUp, func) ::: collectDsl(tail, func)
    case setting :: tail => func.lift(setting).map(_ :: collectDsl(tail, func)) getOrElse collectDsl(tail, func)
  }

  protected def collectFirstDsl[R](l: List[AbstractLayoutSetting], func: PartialFunction[AbstractLayoutSetting, R]): Option[R] = l match{
    case Nil => None
    case SplitLayout(_, leftOrDown, rightOrUp) :: tail => collectFirstDsl(tail, func) orElse collectFirstDsl(leftOrDown, func) orElse collectFirstDsl(rightOrUp, func)
    case setting :: tail => func.lift(setting) orElse collectFirstDsl(tail, func)
  }

  trait LayoutDSLDefaultImpl extends LayoutDSL{
/*
    lazy val componentAccessBuilder: ComponentAccessBuilder = new ComponentAccessBuilder {
      def build(layout: List[AbstractLayoutSetting]): ComponentAccess = new ComponentAccess{
        val componentsMap = collectDsl(layout, {
          case elem@LayoutElem(_, id, _) => id -> elem
        }).toMap

        def get(id: String) = componentsMap.get(id).map(_.component)
        def getLayoutOf(id: String) = componentsMap.get(id)
        def getId[C <% Component](c: C) = componentsMap.find(_._2.component == c).map(_._1)
      }
    }
*/

    protected lazy val updatingComponents = componentAccess.collect{
      case LayoutElem(c, _, _) if c.isInstanceOf[UpdateInterface] => c.asInstanceOf[UpdateInterface]
    }

    def updateForms(): Unit = updatingComponents.foreach(_.updateForm())
  }
  
  trait Frame9PositionsLayoutBuilder extends LayoutBuilder with AwtUtils with Layout9PositionsDSL with LayoutDSLDefaultImpl{
    frame: Frame =>

    /**
     * builds layout within `frame`
     */
    def buildLayout(): Unit = {
      collectFirstDsl(layout, { case Title(title) => frame.title = title })
      collectFirstDsl(layout, { case Size(size) => frame.size = size })
      
      def panel(elems: List[LayoutElem], scroll: Option[Scrollable]) = { // l: List[LayoutSetting]
        val center = elems.collect{ case e@LayoutElem(_, _, Center) => e} -> Orientation.Horizontal

        val north = elems.collect{ case e@LayoutElem(_, _, North) => e}   -> Orientation.Horizontal
        val south = elems.collect{ case e@LayoutElem(_, _, South) => e}   -> Orientation.Horizontal
        val west = elems.collect{ case e@LayoutElem(_, _, West) => e}     -> Orientation.Vertical
        val east = elems.collect{ case e@LayoutElem(_, _, East) => e}     -> Orientation.Vertical

        val northWest = elems.collect{ case e@LayoutElem(_, _, NorthWest) => e} -> Orientation.Horizontal
        val northEast = elems.collect{ case e@LayoutElem(_, _, NorthEast) => e} -> Orientation.Vertical
        val southWest = elems.collect{ case e@LayoutElem(_, _, SouthWest) => e} -> Orientation.Vertical
        val southEast = elems.collect{ case e@LayoutElem(_, _, SouthEast) => e} -> Orientation.Horizontal

        def n(list: List[List[LayoutElem]]) = if((false /: list)((acc, e) => acc || e.nonEmpty)) 1 else 0

        val x1 = west._1 :: northWest._1 :: southWest._1 :: Nil
        val x2 = center._1 :: north._1 :: south._1 :: Nil
        val x3 = east._1 :: northEast._1 :: southEast._1 :: Nil
        val (nx1, nx2, nx3) = (n(x1), n(x2), n(x3))
        val nx = nx1 + nx2 + nx3

        val y1 = north._1 :: northWest._1 :: northEast._1 :: Nil
        val y2 = center._1 :: west._1 :: east._1 :: Nil
        val y3 = south._1 :: southWest._1 :: southEast._1 :: Nil
        val (ny1, ny2, ny3) = (n(y1), n(y2), n(y3))
        val ny = ny1 + ny2 + ny3


        def all = northWest :: north :: northEast :: west :: center :: east :: southWest :: south :: southEast :: Nil

        val p = new GridPanel(ny, nx){
          panel =>

          def putContents(c: (Seq[LayoutElem], Orientation.Value)) = {
            val (elems, orientation) = c
            if(elems.size == 0) {} // do nothing
            else if(elems.size == 1) panel.contents += elems.head.component
            else panel.contents += new BoxPanel(orientation){ contents ++= elems.map(_.component) }
          }

          if(nx1 == 1 && ny1 == 1) putContents(northWest)
          if(nx2 == 1 && ny1 == 1) putContents(north)
          if(nx3 == 1 && ny1 == 1) putContents(northEast)
          if(nx1 == 1 && ny2 == 1) putContents(west)
          if(nx2 == 1 && ny2 == 1) putContents(center)
          if(nx3 == 1 && ny2 == 1) putContents(east)
          if(nx1 == 1 && ny3 == 1) putContents(southWest)
          if(nx2 == 1 && ny3 == 1) putContents(south)
          if(nx3 == 1 && ny3 == 1) putContents(southEast)
/*          for{
            (elems, flow) <- all
            if elems.nonEmpty
          } contents :+ (
            if(elems.length == 1) elems.head.component
            else new BoxPanel(flow){contents :+ elems.map(_.component)}
          )*/
        }

        scroll.map{
          case Scrollable(v, h) => new ScrollPane(p){
            verticalScrollBarPolicy = v
            horizontalScrollBarPolicy = h
          }
        } getOrElse p
      }

      val scrollable = layout.collectFirst{ // search scrollable on top level
        case sc: Scrollable => sc
      }
      val (absolute, relative) = layout.collect{ // search elements on top level
        case el: LayoutElem => el
      }.partition(_.pos.isAbsolute)
      val groupedRelative = relative.groupBy(_.pos.asInstanceOf[DSLRelativePosition].relTo)
      val absoluteIds = absolute.map(_.id)
      val outOfScope = groupedRelative.keys.filterNot(absoluteIds.contains)
      assert(outOfScope.isEmpty, s"relative position refers to an element out of outer container: $outOfScope")

      def relativeToAbsolute: DSLPosition => DSLAbsolutePosition = {
        case DSLRelativePosition(dir, _) => dir match {
          case Left => West
          case Right => East
          case Up => North
          case Down => South
        }
      }

      val absoluteMap = absolute.map(le => le.id -> le).toMap
      def center(id: String) = LayoutElem(absoluteMap(id).component, id, Center)

      val panelsMap = groupedRelative.map{
        case (centerId, elems) => centerId -> panel(center(centerId) :: elems.map(e => e.copy(pos = relativeToAbsolute(e.pos))), None) // todo
      }.toMap
      val unReferenced = (absoluteMap -- panelsMap.keys).map(_._2)
      val finalElems =
        panelsMap.map{
          case (id, p) => LayoutElem(p, id, absoluteMap(id).pos)
        }.toSeq ++ unReferenced

      val finalPanel =
        if(finalElems.length == 1) finalElems.head.component
        else panel(finalElems.toList, scrollable)

      frame.contents = finalPanel
      frame.pack()
    }
  }
}

object SwingSurroundingFrameAppCreation extends SwingSurroundingFrameAppCreation
trait SwingSurroundingFrameAppCreation extends SwingFrameAppCreation{

  trait SurroundingLayoutBuilder extends LayoutBuilder{
    dsl: SurroundingLayoutDSL =>
  }

  trait SurroundingLayoutDSL extends LayoutDSL with FormCreationDSL{

    private val awtToSwingComponentCache = mutable.HashMap.empty[java.awt.Component, Component]
    implicit def awtToSwingComponent(c: java.awt.Component): Component =
      awtToSwingComponentCache.getOrElse(c, {
        val p = new Panel { // let's see, let's see ...
          peer.add(new JPanel{
            add(c)
            /*
                          override def paintComponent(g: Graphics) {
                            c.paint(g)
                            super.paintComponent(g)
                          }
            */
          })
        }
        awtToSwingComponentCache += c -> p
        p
      })
  }
}

trait AppSurroundingSwingFrame extends SwingAppFrame with AwtWindowedApp
  with SwingSurroundingFrameAppCreation
{
  self: SwingSurroundingFrameAppCreation#SurroundingLayoutDSL with SwingSurroundingFrameAppCreation#SurroundingLayoutBuilder =>

  def appWindow = peer
}