package feh.tec.visual

import scala.swing._
import event.ButtonClicked
import javax.swing.JSpinner
import feh.tec.util.LiftWrapper
import scala.swing.Label
import java.awt.Color
import concurrent.duration.FiniteDuration


trait FormCreation {

  protected trait FormCreationDSL{
    protected def monitorFor[T](get: => T)(implicit chooser:  (=> T) => MonitorComponentChooser[T]) = chooser(get)
    protected def controlFor[T](get: => T)(set: T => Option[Throwable])(implicit chooser: (=> T, T => Option[Throwable]) => ControlComponentChooser[T]) = chooser(get, set)
    protected def numericControlFor[N](get: => N)(set: N => Option[Throwable])
                                      (implicit chooser: (=> N, N => Option[Throwable]) => NumericControlComponentChooser[N], num: Numeric[N]) = chooser(get, set)
    protected def triggerFor(action: => Unit)(implicit chooser: (=> Unit) => TriggerComponentChooser) = chooser(action)

    object A { var x = 0 }
    def test = monitorFor(A.x).text
      .affect(_.border = ???, _.background = ???)

    def test2 = numericControlFor(A.x){case x if x > 0 => None case _ => Some(new Exception(""))}.slider

    def updateForms()
  }

  implicit def monitorComponentChooser[T](get: => T): MonitorComponentChooser[T] = new MonitorComponentChooser(get)
  implicit def controlComponentChooser[T](get: => T, set: T => Option[Throwable]) = new ControlComponentChooser(get, set)
  implicit def numericControlComponentChooser[N: Numeric](get: => N, set: N => Option[Throwable]): NumericControlComponentChooser[N] = new NumericControlComponentChooser(get, set)
  implicit def triggerComponentChooser(action: => Unit): TriggerComponentChooser = new TriggerComponentChooser(action)

//  protected implicit def buildForm[T](builder: DSLFormBuilder[T]): Component = builder.build()
  implicit def buildIntForm: DSLFormBuilder[Int] => Component = _.build()
  implicit def buildStringForm: DSLFormBuilder[String] => Component = _.build()

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

  protected class ControlComponentChooser[T](_get: => T, val set: T => Option[Throwable]){
    def get: T = _get

    def textForm = new DSLTextFormBuilder(get)
    def textArea = new DSLTextAreaBuilder(get)
    def dropDownList = new DSLComboBoxBuilder(get)
  }

  protected class NumericControlComponentChooser[N: Numeric](_get: => N, override val set: N => Option[Throwable]) extends ControlComponentChooser(_get, set){
    def spinner = new DSLSpinnerBuilder(get) 
    def slider = new DSLSliderBuilder(get)
  }
  
  protected class TriggerComponentChooser(action: => Unit){
    def button(label: String) = new DSLButtonBuilder(() => action, label)
    def toggle(label: String, repeatFreq: FiniteDuration) = new DSLToggleButtonBuilder(() => action, label, repeatFreq)
  } 

  protected trait DSLFormBuilder[T]{
    type Form <: Component with UpdateInterface

    protected[FormCreation] def build(): Form

    def affect(effects: (Form => Unit)*): DSLFormBuilder[T]

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
                                                                   protected val color: Color = Color.black)
    extends DSLFormBuilder[T]
  {
    type Form = Label with UpdateInterface

    protected[FormCreation] def build(): Form = new Label() with UpdateInterface{
      foreground = color
      def updateForm(): Unit = {
        text = get().toString
      }
      effects.foreach(_(this))
    }

    protected[FormCreation] def update(): Unit = ???

    def affect(effects: (Form => Unit)*) = copy(effects = this.effects ::: effects.toList)

    def color(c: Color): DSLLabelBuilder[T]  = copy(color = c)
    def withColor(c: Color): DSLLabelBuilder[T]  = color(c)

    def toComponent: Component = build()
  }

  protected class DSLTextFormBuilder[T](get: => T) extends DSLFormBuilder[T]{
    type Form = TextField with UpdateInterface

    protected[FormCreation] def build(): Form = ???

    def affect(effects: (Form => Unit)*): DSLTextFormBuilder[T] = ???

    protected[FormCreation] def update(): Unit = ???
  }

  protected class DSLTextAreaBuilder[T](t: T) extends DSLFormBuilder[T]{
    type Form = TextArea with UpdateInterface

    protected[FormCreation] def build(): Form = ???

    def affect(effects: (Form => Unit)*): DSLTextAreaBuilder[T] = ???

    protected[FormCreation] def update(): Unit = ???
  }

  protected class DSLComboBoxBuilder[T] (t: T) extends DSLFormBuilder[T]{
    type Form = ComboBox[T] with UpdateInterface

    protected[FormCreation] def build(): Form = ???

    def affect(effects: (Form => Unit)*): DSLComboBoxBuilder[T] = ???

    protected[FormCreation] def update(): Unit = ???
  }

  protected class DSLSpinnerBuilder[N: Numeric](t: N) extends DSLFormBuilder[N]{
    type Form = Null with UpdateInterface//JSpinner // todo

    protected[FormCreation] def build(): Form = ???

    def affect(effects: (Form => Unit)*): DSLSpinnerBuilder[N] = ???

    protected[FormCreation] def update(): Unit = ???
  }
  
  protected class DSLSliderBuilder[N: Numeric](t: N) extends DSLFormBuilder[N]{
    type Form = Slider with UpdateInterface

    protected[FormCreation] def build(): Form = ???

    def affect(effects: (Form => Unit)*): DSLSliderBuilder[N] = ???

    protected[FormCreation] def update(): Unit = ???
  }

  protected case class DSLButtonBuilder(protected[FormCreation] val action: () => Unit,
                                        protected[FormCreation] val label: String,
                                        protected[FormCreation] val effects: List[DSLButtonBuilder#Form => Unit] = Nil)
    extends DSLFormBuilder[Unit]
  {
    builder =>

    type Form = Button with UpdateInterface

    protected[FormCreation] def build(): Form =  new Button() with UpdateInterface{
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
    }

    def affect(effects: ((Form) => Unit) *): DSLFormBuilder[Unit] = copy(effects = this.effects ++ effects)

    protected[FormCreation] def update() {}
  }
  
  protected case class DSLToggleButtonBuilder(protected[FormCreation] val action: () => Unit,
                                              protected[FormCreation] val label: String,
                                              protected[FormCreation] val repeatFreq: FiniteDuration,
                                              protected[FormCreation] val effects: List[DSLToggleButtonBuilder#Form => Unit] = Nil)
    extends DSLFormBuilder[Unit]
  {
    type Form = ToggleButton with UpdateInterface

    protected[FormCreation] def build(): Form =  new ToggleButton with UpdateInterface{
      def updateForm(): Unit = {
        text = label
      }
      effects.foreach(_(this))
    }

    def affect(effects: (Form => Unit) *): DSLFormBuilder[Unit] = ???

    protected[FormCreation] def update() {}
  }
}
