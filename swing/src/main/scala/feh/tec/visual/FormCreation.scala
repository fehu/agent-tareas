package feh.tec.visual

import scala.swing._
import scala.swing.event.{ValueChanged, ButtonClicked}
import javax.swing.JSpinner
import feh.tec.util.LiftWrapper
import scala.swing.Label
import java.awt.Color
import concurrent.duration.FiniteDuration
import scala.collection.mutable
import scala.collection.immutable.NumericRange
import feh.tec.util._
import scala.swing.GridBagPanel.Fill


object FormCreation{
  type Constraints = GridBagPanel#Constraints
}
import FormCreation._

trait FormCreation {

  protected trait FormCreationDSL{
    protected def monitorFor[K, V](get: => Map[K, V])(implicit chooser:  (=> Map[K, V]) => MapMonitorComponentChooser[K, V]) = chooser(get)
    protected def monitorFor[T](get: => T)(implicit chooser:  (=> T) => MonitorComponentChooser[T]) = chooser(get)
    protected def controlFor[T](get: => T)(set: T => Unit)(implicit chooser: (=> T, T => Unit) => ControlComponentChooser[T]) = chooser(get, set)
    protected def numericControlFor[N](get: => N)(set: N => Unit)
                                      (implicit chooser: (=> N, N => Unit) => NumericControlComponentChooser[N], num: Numeric[N]) = chooser(get, set)
    protected def triggerFor(action: => Unit)(implicit chooser: (=> Unit) => TriggerComponentChooser) = chooser(action)

    object A { var x = 0 }
    def test = monitorFor(A.x).text
      .affect(_.border = ???, _.background = ???)

//    def test2 = numericControlFor(A.x){case x if x > 0 => None case _ => Some(new Exception(""))}.slider

    def updateForms()
  }

  implicit def monitorComponentChooser[T](get: => T): MonitorComponentChooser[T] = new MonitorComponentChooser(get)
  implicit def mapMonitorComponentChooser[K, V](get: => Map[K, V]): MapMonitorComponentChooser[K, V] = new MapMonitorComponentChooser(get)
  implicit def controlComponentChooser[T](get: => T, set: T => Unit) = new ControlComponentChooser(get, set)
  implicit def numericControlComponentChooser[N: Numeric](get: => N, set: N => Unit): NumericControlComponentChooser[N] = new NumericControlComponentChooser(get, set)
  implicit def triggerComponentChooser(action: => Unit): TriggerComponentChooser = new TriggerComponentChooser(action)

//  protected implicit def buildForm[T](builder: DSLFormBuilder[T]): Component = builder.build()
  implicit def buildIntForm: DSLFormBuilder[Int] => BuildMeta = _.build()
  implicit def buildStringForm: DSLFormBuilder[String] => BuildMeta = _.build()
  implicit def buildMapForm: DSLListBuilder[_, _] => BuildMeta = _.build()
  implicit def buildUnitForm: DSLFormBuilder[Unit] => BuildMeta = _.build()

  implicit def intFormToComponent: DSLFormBuilder[Int] => Component = _.component
  implicit def stringFormToComponent: DSLFormBuilder[String] => Component = _.component
  implicit def mapFormToComponent: DSLListBuilder[_, _] => Component = _.component
  implicit def unitFormToComponent: DSLFormBuilder[Unit] => Component = _.component

  implicit def componentToMeta(c: Component): BuildMeta = BuildMeta(c)


  protected class MonitorComponentChooser[T](_get: => T){
    def get = _get

    def text = DSLLabelBuilder(get.lifted)
    def label = text
    def textField = new DSLTextFormBuilder(get)
    def textArea = new DSLTextAreaBuilder(get)

    def asText = text
    def asLabel = text
    def asTextField = textField
    def asTextArea = textArea
  }

  protected class MapMonitorComponentChooser[K, V](_get: => Map[K, V]) extends MonitorComponentChooser[Map[K, V]](_get){
    def list = DSLListBuilder(() => get)

    def asList = list
  }

  protected class ControlComponentChooser[T](_get: => T, val set: T => Unit){
    def get: T = _get

    def textForm = new DSLTextFormBuilder(get)
    def textArea = new DSLTextAreaBuilder(get)
    def dropDownList = new DSLComboBoxBuilder(get)
  }

  protected class NumericControlComponentChooser[N: Numeric](_get: => N, override val set: N => Unit) extends ControlComponentChooser(_get, set){
    def spinner = new DSLSpinnerBuilder(get) 
    def slider(range: NumericRange[N]) = new DSLSliderBuilder(() => get, set, range)
  }
  
  protected class TriggerComponentChooser(action: => Unit){
    def button(label: String) = new DSLButtonBuilder(() => action, label)
    def toggle(label: String, repeatFreq: FiniteDuration) = new DSLToggleButtonBuilder(() => action, label, repeatFreq)
  } 

  object BuildMeta{
    def apply(_component: Component, _layout: (Constraints => Unit)*): BuildMeta = new BuildMeta{
      def component: Component = _component
      def layout: List[(FormCreation.Constraints) => Unit] = _layout.toList
    }

    def build(_component: Component): BuildMeta = apply(_component)

    def unapply(meta: BuildMeta): Option[(Component, List[Constraints => Unit])] = Some(meta.component -> meta.layout)
  }
  trait BuildMeta{
    def component: Component
    def layout: List[Constraints => Unit]
  }

  protected trait DSLFormBuilder[T]{
    type Form <: Component with UpdateInterface

    case class FormBuildMeta(form: Form, layout: List[Constraints => Unit]) extends BuildMeta{
      def component: Component = form
    }

    protected implicit def toFormMeta(p: (Form, List[Constraints => Unit])): FormBuildMeta = FormBuildMeta(p._1, p._2)

    protected[FormCreation] def build(): FormBuildMeta
    

    def affect(effects: (Form => Unit)*): DSLFormBuilder[T]
    def layout(effects: (Constraints => Unit)*): DSLFormBuilder[T]

//    protected[FormCreation] def update()

//    trait Setting
  }

  protected trait UpdateInterface{
    def updateForm()
  }

  case class DSLFormBuilderWrapper[T](builder: DSLFormBuilder[T]){
    def build = builder.build() 
  }
  
  protected case class DSLLabelBuilder[T] protected[FormCreation] (protected[FormCreation] val get: () => T,
                                                                   protected[FormCreation] val effects: List[DSLLabelBuilder[T]#Form => Unit] = Nil,
                                                                   protected[FormCreation] val layout: List[Constraints => Unit] = Nil,
                                                                   protected[FormCreation] val color: Color = Color.black)
    extends DSLFormBuilder[T]
  {
    type Form = Label with UpdateInterface

    protected[FormCreation] def build(): FormBuildMeta = new Label() with UpdateInterface{
      foreground = color
      def updateForm(): Unit = {
        text = get().toString
      }
      effects.foreach(_(this))
    } -> layout

    def affect(effects: (Form => Unit)*) = copy(effects = this.effects ::: effects.toList)
    def layout(effects: (Constraints => Unit)*) = copy(layout = layout ++ effects)

    def color(c: Color): DSLLabelBuilder[T]  = copy(color = c)
    def withColor(c: Color): DSLLabelBuilder[T]  = color(c)
  }

  protected class DSLTextFormBuilder[T](get: => T) extends DSLFormBuilder[T]{
    type Form = TextField with UpdateInterface

    protected[FormCreation] def build(): FormBuildMeta = ???

    def affect(effects: (Form => Unit)*): DSLTextFormBuilder[T] = ???
    def layout(effects: (Constraints => Unit)*): DSLFormBuilder[T] = ???
  }

  protected class DSLTextAreaBuilder[T](get: => T) extends DSLFormBuilder[T]{
    type Form = TextArea with UpdateInterface

    protected[FormCreation] def build(): FormBuildMeta = ???

    def affect(effects: (Form => Unit)*): DSLTextAreaBuilder[T] = ???
    def layout(effects: (Constraints => Unit)*): DSLFormBuilder[T] = ???
  }

  protected class DSLComboBoxBuilder[T] (get: => T) extends DSLFormBuilder[T]{
    type Form = ComboBox[T] with UpdateInterface

    protected[FormCreation] def build(): FormBuildMeta = ???

    def affect(effects: (Form => Unit)*): DSLComboBoxBuilder[T] = ???
    def layout(effects: (Constraints => Unit)*): DSLFormBuilder[T] = ???
  }

  protected class DSLSpinnerBuilder[N: Numeric](get: => N) extends DSLFormBuilder[N]{
    type Form = Null with UpdateInterface//JSpinner // todo

    protected[FormCreation] def build(): FormBuildMeta = ???

    def affect(effects: (Form => Unit)*): DSLSpinnerBuilder[N] = ???
    def layout(effects: (Constraints => Unit)*): DSLFormBuilder[N] = ???
  }
  
  protected case class DSLSliderBuilder[N: Numeric](protected[FormCreation] val get: () => N,
                                                    protected[FormCreation] val set: N => Unit,
                                                    protected[FormCreation] val range: NumericRange[N],
                                                    protected[FormCreation] val effects: List[DSLSliderBuilder[N]#Form => Unit] = Nil,
                                                    protected[FormCreation] val layout: List[Constraints => Unit] = Nil)
    extends DSLFormBuilder[N]
  {
    type Form = Slider with UpdateInterface

    def form: Form = new Slider with UpdateInterface{
      slider =>

      val num = implicitly[Numeric[N]]
      import num._

      val n = range.length
      val scale = roundToClosetPowerOf10(n)
      val label = if(scale != 1) s"1/$scale" else ""

      def roundToClosetPowerOf10(i: Int) = Y[Int, Int]{
        rec => x => if(i <= x) x else rec(x*10)
      }(1)

      val myMin = if(range.min.toInt() == range.min) range.min.toInt() else ???

      min = myMin
      max = myMin + n

      def valueToInt(x: N) = myMin + {
        x - fromInt(myMin) match {
          case d: Double => d / range.step.toDouble()
        }
      }.toInt

      def valueFromInt(i: Int) = fromInt(min - i) * range.step

      def updateForm(): Unit = { value = valueToInt(get()) }

      listenTo(slider)
      reactions += {
        case e@ValueChanged(`slider`) if !slider.adjusting =>
          set(valueFromInt(value))
      }

    }

    // todo: this is for floating
    protected[FormCreation] def build(): FormBuildMeta = form -> layout

    def affect(effects: (Form => Unit)*) = copy(effects = this.effects ++ effects)
    def layout(effects: (Constraints => Unit)*) = copy(layout = layout ++ effects)
  }

  protected case class DSLListBuilder[K, V](protected[FormCreation] val get: () => Map[K, V],
                                            protected[FormCreation] val effects: List[DSLListBuilder[K, V]#Form => Unit] = Nil,
                                            protected[FormCreation] val layout: List[Constraints => Unit] = Nil)
    extends DSLFormBuilder[Map[K, V]]
  {
    type Form = ListView[V] with UpdateInterface

    protected[FormCreation] def build(): FormBuildMeta = new ListView[V](Nil) with UpdateInterface{
      listView =>

      val mapCache = mutable.HashMap.apply(get().toSeq: _*)

      def updateForm(): Unit = {
        val newMap = get()
        val addDiff = newMap.keySet -- mapCache.keySet
        val rmDiff = mapCache.keySet -- newMap.keySet

        mapCache --= rmDiff
        mapCache ++= addDiff.map(k => k -> newMap(k))

        listView.listData = mapCache.values.toSeq
      }
    } -> layout

    def affect(effects: (Form=> Unit) *) = copy(effects = this.effects ++ effects)
    def layout(effects: (Constraints => Unit)*) = copy(layout = layout ++ effects)

  }

  protected case class DSLButtonBuilder(protected[FormCreation] val action: () => Unit,
                                        protected[FormCreation] val label: String,
                                        protected[FormCreation] val effects: List[DSLButtonBuilder#Form => Unit] = Nil,
                                        protected[FormCreation] val layout: List[Constraints => Unit] = Nil)
    extends DSLFormBuilder[Unit]
  {
    builder =>

    type Form = Button with UpdateInterface

    protected[FormCreation] def build(): FormBuildMeta =  new Button() with UpdateInterface{
      button =>

      text = label
      reactions += {
        case c@ButtonClicked(`button`) =>
          println(builder.action)
          builder.action()
      }
      listenTo(button)
      effects.foreach(_(this))
      def updateForm(): Unit = {}
    } -> layout

    def affect(effects: ((Form) => Unit) *): DSLFormBuilder[Unit] = copy(effects = this.effects ++ effects)
    def layout(effects: (Constraints => Unit)*) = copy(layout = layout ++ effects)
  }
  
  protected case class DSLToggleButtonBuilder(protected[FormCreation] val action: () => Unit,
                                              protected[FormCreation] val label: String,
                                              protected[FormCreation] val repeatFreq: FiniteDuration,
                                              protected[FormCreation] val effects: List[DSLToggleButtonBuilder#Form => Unit] = Nil,
                                              protected[FormCreation] val layout: List[Constraints => Unit] = Nil)
    extends DSLFormBuilder[Unit]
  {
    type Form = ToggleButton with UpdateInterface

    protected[FormCreation] def build(): FormBuildMeta =  new ToggleButton with UpdateInterface{
      def updateForm(): Unit = {
        text = label
      }
      effects.foreach(_(this))
    } -> layout

    def affect(effects: (Form => Unit) *): DSLFormBuilder[Unit] = ???
    def layout(effects: (Constraints => Unit)*): DSLFormBuilder[Unit] = ???
  }

  implicit class DSLFormBuilderOps[T](val builder: DSLFormBuilder[T]){

    def sizes(min: Dimension = null,
              max: Dimension = null,
              preferred: Dimension = null): builder.type =
    {
      def helper(dim: Dimension, f: (builder.Form, Dimension) => Unit): Option[builder.Form => Unit] = Option(dim) map (d => f(_, d))

      val h =
        helper(min, _.minimumSize = _) ::
        helper(max, _.maximumSize = _) ::
        helper(preferred, _.preferredSize = _) :: Nil
      builder.affect(h.flatten: _*).asInstanceOf[builder.type]
    }

    def fillHorizontally = builder.layout(_.fill = Fill.Horizontal)
  }
}
