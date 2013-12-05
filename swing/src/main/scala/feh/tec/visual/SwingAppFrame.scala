package feh.tec.visual

import feh.tec.visual.api.{AppBasicControlApi, AwtWindowedApp}
import scala.swing._
import scala.swing.ScrollPane.BarPolicy
import feh.tec.visual.util.AwtUtils
import javax.swing.JPanel
import scala.collection.mutable
import java.util.UUID
import feh.tec.util._
import feh.tec.visual.FormCreation.Constraints
import scala.xml.NodeSeq
import javax.swing.Box.Filler


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
    @deprecated protected def set(s: UpperLevelLayoutGlobalSetting): AbstractLayoutSetting = s
    protected def panel = new PanelChooser
    protected def scrollable[M <% BuildMeta](vert: BarPolicy.Value = BarPolicy.AsNeeded,
                                             hor: BarPolicy.Value = BarPolicy.AsNeeded)
                                            (content: M, id: String) = ScrollPaneBuilder(vert, hor, content, id)
    protected def boxed[M <% BuildMeta](content: M, id: String) = BoxedBuilder(content, id)

    // normal settings
    protected def place[M <% BuildMeta](what: M, id: String): DSLPlacing = DSLPlacing(what, id)
    protected def place[T](builder: DSLFormBuilder[T], id: String): DSLPlacing = DSLPlacing(builder.formMeta, id)
    protected def place(builder: PanelBuilder): DSLPlacing = DSLPlacing(builder.meta, builder.id)
    protected def place(builder: ScrollPaneBuilder): DSLPlacing = DSLPlacing(builder.meta, builder.id)
    protected def place(builder: BoxedBuilder): DSLPlacing = DSLPlacing(builder.meta, builder.id)
    protected def place(meta: GridBagMeta, id: String): DSLPlacing = DSLPlacing(meta, id)
    protected def make(s: LayoutGlobalSetting) = s

    protected def noId: String = null

    protected def html[T](t: => T)(build: T => NodeSeq): DSLLabelBuilder[T] = label(t)
      .stringExtractor(t => surroundHtml(<html>{build(t)}</html>).toString)
    protected def html(html: NodeSeq): DSLLabelBuilder[NodeSeq] = label(surroundHtml(html))
    protected def label[T](text: => T): DSLLabelBuilder[T] = new DSLLabelBuilder[T](() => text)

    protected case class DSLPlacing(what: BuildMeta, id: String){
      def at(pos: DSLAbsolutePosition): LayoutElem = LayoutElem(what, id, pos).register
      def in(pos: DSLAbsolutePosition): LayoutElem = at(pos)
      def to(rel: DSLRelativePositionDirection): DSLRelativelyOf = DSLRelativelyOf(what, id, rel)
      def on(rel: DSLRelativePositionDirection): DSLRelativelyOf = to(rel)
    }

    protected case class DSLRelativelyOf(what: BuildMeta, id: String, dir: DSLRelativePositionDirection){
      def of(rel: String): LayoutElem = LayoutElem(what, id, DSLRelativePosition(dir, rel)).register
      def of[C <% Component](c: C) = LayoutElem(what, id, DSLRelativePosition(dir, componentAccess.id(c))).register
      def from(rel: String): LayoutElem = of(rel)
      def from[C <% Component](c: C) = of(c)
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
//        println(s"$elem is registered!")
        componentAccess.register(elem)
        elem
      }
    }

    protected implicit def componentIdPairToUnplacedLayoutElem[C <% Component](p: (C, String)): UnplacedLayoutElem = {
      val el = LayoutElem.unplaced(BuildMeta(p._1), p._2)
      el.register
      el
    }

    protected implicit class ComponentToUnplacedLayoutElemWithoutRegisteringWrapper[C <% Component](c: C){
      def withoutRegistering = LayoutElem.unplaced(BuildMeta(c), null)
    }

    implicit class ExplicitToComponentWrapper[C <% Component](c: C){
      def toComponent: Component = c
    }
    
    implicit class WithoutIdWrapper[C <% Component](c: C){
      def withoutId: UnplacedLayoutElem = c -> noId
    }
    
    implicit class WithoutIdSeqWrapper[C <% Component](c: Seq[C]){
      def withoutIds: Seq[UnplacedLayoutElem] = c.map(k => (k, noId): UnplacedLayoutElem)
    }
    
    protected class PanelChooser{
      def flow[E <% UnplacedLayoutElem](alignment: FlowPanel.Alignment.type => FlowPanel.Alignment.Value)(elems: E*) =
        FlowPanelBuilder(alignment(FlowPanel.Alignment), elems.toList.map(x => x: UnplacedLayoutElem))
      def box[E <% UnplacedLayoutElem](alignment: Orientation.type => Orientation.Value)(elems: E*) =
        BoxPanelBuilder(alignment(Orientation), elems.toList.map(x => x: UnplacedLayoutElem))
      def gridBag(settings: LayoutSetting*) = GridBagMeta(settings.toList)
    }

    class RegistringComponentAccess extends ComponentAccess{
      private val componentsMap = mutable.HashMap.empty[String, LayoutElem]
      private val delayedMap = mutable.HashMap.empty[String, LayoutElem]

      protected[LayoutDSL] def register(elem: LayoutElem){
        elem.meta match{
          case gb: GridBagMeta => delayedMap += elem.id -> elem // should be called again on panel creation
          case _ => componentsMap += elem.id -> elem

        }
      }

      def get(id: String) = componentsMap.get(id).map(_.meta.component)
      def getLayoutOf(id: String) = componentsMap orElse delayedMap lift id
      def getId[C <% Component](c: C) = {
        val comp = c: Component
        componentsMap.find(_._2.meta.component == comp).map(_._1)
      }
      def all: Seq[LayoutElem] = componentsMap.values.toSeq
    }
    private def surroundHtml(in: NodeSeq) = in match{
      case <html>{_*}</html> => in
      case _ => <html>{in}</html>
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

  class LayoutElem private (val meta: BuildMeta, val id: String, val pos: DSLPosition) extends LayoutSetting{
    override def toString: String = s"LayoutElem($id, $pos, $meta)"
    def copy(meta: BuildMeta = this.meta, id: String = this.id, pos: DSLPosition = this.pos) = LayoutElem(meta, id, pos)
  }
  object LayoutElem{
    def apply(meta: BuildMeta, id: String, pos: DSLPosition): LayoutElem =
      new LayoutElem(meta, Option(id) getOrElse randomId(meta), pos)
    def unplaced(c: BuildMeta, id: String): UnplacedLayoutElem = new LayoutElem(c, id, null) with UnplacedLayoutElem
    def unapply(el: LayoutElem): Option[(BuildMeta, String, DSLPosition)] = Some(el.meta, el.id, el.pos)

    def randomId = (_: BuildMeta) match{
      case _: GridBagMeta => "#GridBag-" + UUID.randomUUID()
      case meta => "#" + meta.component.hashCode()
    }
  }
  
  trait UnplacedLayoutElem extends LayoutElem{
    override val pos = Undefined  
  }

  case class Scrollable(vert: BarPolicy.Value = BarPolicy.AsNeeded,
                        hor: BarPolicy.Value = BarPolicy.AsNeeded) extends LayoutGlobalSetting

  case class ScrollPaneBuilder(vert: BarPolicy.Value,
                            hor: BarPolicy.Value,
                            protected val content: BuildMeta,
                            id: String)
    extends LayoutSetting
  {
    def component = {
      val c =
        if(content.component.isInstanceOf[UpdateInterface]) new ScrollPane(content.component) with UpdateInterface{
          def updateForm(): Unit = content.component.asInstanceOf[UpdateInterface].updateForm()
        }
        else new ScrollPane(content.component)
      c.horizontalScrollBarPolicy = hor
      c.verticalScrollBarPolicy = vert
      c
    }

    def meta = BuildMeta(component, content.layout: _*)
  }

  case class BoxedBuilder(protected val content: BuildMeta,
                       id: String)
    extends LayoutSetting
  {
    def component =
      if(content.component.isInstanceOf[UpdateInterface]) new BoxPanel(Orientation.NoOrientation) with UpdateInterface{
        contents += content.component
        def updateForm(): Unit = content.component.asInstanceOf[UpdateInterface].updateForm()
      }
      else new BoxPanel(Orientation.NoOrientation){}

    def meta = BuildMeta(component, content.layout: _*)
  }

  case class GridBagMeta(settings: List[LayoutSetting], layout: List[(Constraints) => Unit] = Nil)
    extends BuildMeta with AbstractDSLBuilder
  {
    def component: Component = null
    def layout(effects: (Constraints => Unit)*): GridBagMeta = copy(layout = layout ++ effects)

    override def toString: String = s"GridBagMeta($settings, $layout)"

    type Comp = Null
    def affect(effects: (GridBagMeta#Comp => Unit) *): AbstractDSLBuilder = ???
  }

  implicit class GridBagMetaOps(meta: GridBagMeta) extends DSLFormBuilderOps[GridBagMeta](meta)
  implicit class FlowPanelBuilderOps(builder: FlowPanelBuilder) extends DSLFormBuilderOps[FlowPanelBuilder](builder)
  implicit class BoxPanelBuilderOps(builder: BoxPanelBuilder) extends DSLFormBuilderOps[BoxPanelBuilder](builder)

  implicit def buildBoxPanelMeta: BoxPanelBuilder => BuildMeta = _.meta

  trait PanelBuilder extends LayoutSetting with AbstractDSLBuilder{
    type Panel <: scala.swing.Panel
    type Comp = Panel
    def panelName: String
    def elems: List[LayoutSetting]
    def panel: Panel
    def meta = BuildMeta(panel, layout: _*)
    def component = panel

    def id: String = s"$panelName: ${UUID.randomUUID().toString}"

    def affect(effects: (Panel => Unit)*): PanelBuilder
    def layout(effects: (Constraints => Unit)*): PanelBuilder
    protected def layout: List[Constraints => Unit]
  }

  case class FlowPanelBuilder(alignment: FlowPanel.Alignment.Value,
                             elems: List[LayoutElem],
                             panelName: String = "FlowPanel",
                             protected val effects: List[FlowPanelBuilder#Panel => Unit] = Nil,
                             protected val layout: List[Constraints => Unit] = Nil) extends PanelBuilder
  {

    type Panel = FlowPanel
    def panel = {
      val p = new FlowPanel(alignment)(elems.map(_.meta.component): _*)
      effects.foreach(_(p))
      p
    }
    def affect(effects: (FlowPanelBuilder#Panel => Unit) *): PanelBuilder = copy(effects = this.effects ++ effects)
    def layout(effects: (Constraints => Unit) *) = copy(layout = layout ++ effects)
  }

  implicit def boxPanelMetaToComponent: BoxPanelBuilder => Component = _.panel

  case class BoxPanelBuilder(alignment: Orientation.Value,
                            elems: List[LayoutElem],
                            indent: Option[Int] = Some(10),
                            panelName: String = "BoxPanel",
                            protected val effects: List[BoxPanelBuilder#Panel => Unit] = Nil,
                            protected val layout: List[Constraints => Unit] = Nil) extends PanelBuilder
  {
    type Panel = BoxPanel

    def indent(i: Option[Int]): BoxPanelBuilder = copy(indent = i)

    def prepend(el: LayoutElem*) = copy(elems = elems ++ el)
    def append(el: LayoutElem*) = copy(elems = el.toList ++ elems)
    def glue = copy(elems = elems :+ LayoutElem.unplaced(BuildMeta(createGlue), null))
    def strut(i: Int) = copy(elems = elems :+ LayoutElem.unplaced(BuildMeta(createStrut(i)), null))

    def panel = {
      val p = new BoxPanel(alignment)
      val contents = Y[List[Component], List[Component]](
        rec => {
          case Nil => Nil
          case x :: Nil => List(x)
          case x :: y :: tail if y.peer.isInstanceOf[Filler] => x :: y :: rec(tail)
          case x :: y :: tail => x :: createGlue :: y :: rec(y :: tail)
        }
      )(elems.map(_.meta.component))

      p.contents ++= indent.map{
        i => createStrut(i) :: contents ::: List(createStrut(i))
      } getOrElse contents

      effects.foreach(_(p))
      p
    }
    def affect(effects: (BoxPanelBuilder#Panel => Unit) *): PanelBuilder = copy(effects = this.effects ++ effects)
    def layout(effects: (Constraints => Unit) *) = copy(layout = layout ++ effects)

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

    def updatable = collect{
      case LayoutElem(BuildMeta(c, _), _, _) if c.isInstanceOf[UpdateInterface] => c.asInstanceOf[UpdateInterface]
    }
  }

  trait ComponentAccessBuilder{
    def build(layout: List[AbstractLayoutSetting]):ComponentAccess
  }

  // // //// // //// // //// // //// // //  Impl  // // //// // //// // //// // //// // //

  trait Layout9PositionsDSL extends LayoutDSL{

    case object Center extends DSLAbsolutePosition
    case object North extends DSLAbsolutePosition with DSLRelativePositionDirection
    case object South extends DSLAbsolutePosition with DSLRelativePositionDirection
    case object West extends DSLAbsolutePosition with DSLRelativePositionDirection
    case object East extends DSLAbsolutePosition with DSLRelativePositionDirection
    case object NorthWest extends DSLAbsolutePosition with DSLRelativePositionDirection
    case object NorthEast extends DSLAbsolutePosition with DSLRelativePositionDirection
    case object SouthWest extends DSLAbsolutePosition with DSLRelativePositionDirection
    case object SouthEast extends DSLAbsolutePosition with DSLRelativePositionDirection
    def theCenter = Center
    def theNorth = North
    def theSouth = South
    def theWest = West
    def theEast = East
    def theNorthWest = NorthWest
    def theNorthEast = NorthEast
    def theSouthWest = SouthWest
    def theSouthEast = SouthEast

    def Left  = West
    def Right = East
    def Up    = North
    def Down  = South
    def theLeft = Left
    def theRight = Right
    def Top = Up
    def theTop = Up
    def Bottom = Down
    def theBottom = Down
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
    protected lazy val updatingComponents = componentAccess.updatable

    def updateForms(): Unit = updatingComponents.foreach(_.updateForm())
  }

  trait Frame9PositionsLayoutBuilder extends LayoutBuilder with GridBagLayoutBuilder{
    frame: Frame =>

    /**
     * builds layout within `frame`
     */
    def buildLayout(): Unit = {
      collectFirstDsl(layout, { case Title(title) => frame.title = title })
      collectFirstDsl(layout, { case Size(size) => frame.size = size })

      frame.contents = gridBag(layout)
      frame.pack()
    }


  }
  trait GridBagLayoutBuilder extends LayoutBuilder with AwtUtils with Layout9PositionsDSL with LayoutDSLDefaultImpl{

    def gridBag(settings: List[AbstractLayoutSetting]): Component = {
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

        val p = new GridBagPanel{
          panel =>

          def createContent(c: (Seq[LayoutElem], Orientation.Value)) = {
            val (elems, orientation) = c
            if(elems.size == 0) None // do nothing
            else if(elems.size == 1) elems.head.meta match{
              case GridBagMeta(s, l) =>
                val meta = BuildMeta(gridBag(s), l: _*)
                LayoutElem(meta, elems.head.id, elems.head.pos).register
                Some(meta)
              case m => Some(m)
            }
            else Some(BuildMeta(new BoxPanel(orientation){ contents ++= elems.map(_.meta.component) }/*, todo: take meta from one of components ?? */))
          }

          def applyLayout(constraints: Constraints, buildConstraints: Seq[Constraints => Unit]) = {
            buildConstraints.foreach(_(constraints))
            constraints
          }

          def putContents(c: (Seq[LayoutElem], Orientation.Value))(x: Int, y: Int, buildConstraints: (Constraints => Unit)*) {
            putContents(c, applyLayout(x -> y, buildConstraints))
          }
          def putContents(c: (Seq[LayoutElem], Orientation.Value), constraints: Constraints) {
            val metaOpt = createContent(c)
            metaOpt.foreach{ meta => panel.layout += meta.component -> applyLayout(constraints, meta.layout) }
          }

          if(nx1 == 1 && ny1 == 1) putContents(northWest)   (0, 0)
          if(nx2 == 1 && ny1 == 1) putContents(north)       (1, 0)
          if(nx3 == 1 && ny1 == 1) putContents(northEast)   (2, 0)
          if(nx1 == 1 && ny2 == 1) putContents(west)        (0, 1)
          if(nx2 == 1 && ny2 == 1) putContents(center)      (1, 1)
          if(nx3 == 1 && ny2 == 1) putContents(east)        (2, 1)
          if(nx1 == 1 && ny3 == 1) putContents(southWest)   (0, 2)
          if(nx2 == 1 && ny3 == 1) putContents(south)       (1, 2)
          if(nx3 == 1 && ny3 == 1) putContents(southEast)   (2, 2)
        }

        scroll.map{
          case Scrollable(v, h) => new ScrollPane(p){
            verticalScrollBarPolicy = v
            horizontalScrollBarPolicy = h
          }
        } getOrElse p
      }

      val scrollable = settings.collectFirst{ // search scrollable on top level
        case sc: Scrollable => sc
      }
      val (absolute, tmp) = settings.collect{ // search elements on top level
        case el: LayoutElem => el
      }.partition(_.pos.isAbsolute)
      val (relative, undef) = tmp.partition(_.pos.isRelative)
      val groupedRelative = relative.groupBy(_.pos.asInstanceOf[DSLRelativePosition].relTo)

      def relativeToAbsolute: DSLPosition => DSLAbsolutePosition = {
        case DSLRelativePosition(dir, _) => dir match {
          case abs: DSLAbsolutePosition => abs
        }
      }

      val absoluteMap = absolute.map(le => le.id -> le).toMap
      def center(id: String) = LayoutElem(componentAccess.layoutOf(id).meta, id, Center)

      val panelsMap = groupedRelative.map{
        case (centerId, elems) => centerId -> panel(center(centerId) :: elems.map(e => e.copy(pos = relativeToAbsolute(e.pos))), None) // todo
      }.toMap
      val unReferenced = (absoluteMap -- panelsMap.keys).map(_._2)
      val finalElems =
        panelsMap.map{
          case (id, p) => LayoutElem(p, id, componentAccess.layoutOf(id).pos)
        }.toSeq ++ unReferenced

      if(finalElems.length == 1) finalElems.head.meta.component
      else panel(finalElems.toList, scrollable)
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