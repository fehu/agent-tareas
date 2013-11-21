package feh.tec.visual

import scala.swing._
import javax.swing.JSpinner
import feh.tec.util.LiftWrapper


trait FormCreation {
  
  trait FormCreationDSL{
    protected def monitorFor[T](get: => T)(implicit chooser:  (=> T) => MonitorComponentChooser[T]) = chooser(get)
    protected def controlFor[T](get: => T)(set: T => Option[Throwable])(implicit chooser: (=> T, T => Option[Throwable]) => ControlComponentChooser[T]) = chooser(get, set)
    protected def numericControlFor[N](get: => N)(set: N => Option[Throwable])
                                      (implicit chooser: (=> N, N => Option[Throwable]) => NumericControlComponentChooser[N], num: Numeric[N]) = chooser(get, set)

    object A { var x = 0 }
    def test = monitorFor(A.x).text
      .affect(_.border = ???, _.background = ???)

    def test2 = numericControlFor(A.x){case x if x > 0 => None case _ => Some(new Exception(""))}.slider
  }

  implicit def monitorComponentChooser[T](get: => T): MonitorComponentChooser[T] = new MonitorComponentChooser(get)
  implicit def controlComponentChooser[T](get: => T, set: T => Option[Throwable]) = new ControlComponentChooser(get, set)
  implicit def numericControlComponentChooser[N: Numeric](get: => N, set: N => Option[Throwable]): NumericControlComponentChooser[N] = new NumericControlComponentChooser(get, set)

  implicit def buildForm[T](builder: DSLFormBuilder[T]): Component = builder.build()

  class MonitorComponentChooser[T](_get: => T){
    def get = _get

    def text = DSLLabelBuilder(get.lifted)
    def label = text
    def textField = new DSLTextFormBuilder(get)
    def textArea = new DSLTextAreaBuilder(get)
  }

  class ControlComponentChooser[T](_get: => T, val set: T => Option[Throwable]){
    def get: T = _get

    def textForm = new DSLTextFormBuilder(get)
    def textArea = new DSLTextAreaBuilder(get)
    def dropDownList = new DSLComboBoxBuilder(get)
  }
  
  class NumericControlComponentChooser[N: Numeric](_get: => N, override val set: N => Option[Throwable]) extends ControlComponentChooser(_get, set){
    def spinner = new DSLSpinnerBuilder(get) 
    def slider = new DSLSliderBuilder(get)
  }
  
  trait DSLFormBuilder[T]{
    type Form <: Component

    protected[FormCreation] def build(): Form

    def affect(effects: (Form => Unit)*): DSLFormBuilder[T]


    trait Setting
  }
  
  case class DSLLabelBuilder[T] protected[FormCreation] (protected[FormCreation] val get: () => T,
                                                         protected[FormCreation] val effects: List[DSLLabelBuilder[T]#Form => Unit] = Nil)
    extends DSLFormBuilder[T]
  {
    type Form = Label

    protected[FormCreation] def build(): Form = ???

    def affect(effects: (Form => Unit)*) = copy(effects = this.effects ::: effects.toList)

    def smthMore(): DSLLabelBuilder[T]  = copy() // todo
  }
  
  class DSLTextFormBuilder[T](get: => T) extends DSLFormBuilder[T]{
    type Form = TextField

    protected[FormCreation] def build(): Form = ???

    def affect(effects: (Form => Unit)*): DSLTextFormBuilder[T] = ???
  }
  
  class DSLTextAreaBuilder[T](t: T) extends DSLFormBuilder[T]{
    type Form = TextArea

    protected[FormCreation] def build(): Form = ???

    def affect(effects: (Form => Unit)*): DSLTextAreaBuilder[T] = ???
  }

  class DSLComboBoxBuilder[T] (t: T) extends DSLFormBuilder[T]{
    type Form = ComboBox[T]

    protected[FormCreation] def build(): Form = ???

    def affect(effects: (Form => Unit)*): DSLComboBoxBuilder[T] = ???
  }

  class DSLSpinnerBuilder[N: Numeric](t: N) extends DSLFormBuilder[N]{
    type Form = Null //JSpinner // todo

    protected[FormCreation] def build(): Form = ???

    def affect(effects: (Form => Unit)*): DSLSpinnerBuilder[N] = ???
  }
  
  class DSLSliderBuilder[N: Numeric](t: N) extends DSLFormBuilder[N]{
    type Form = Slider

    protected[FormCreation] def build(): Form = ???

    def affect(effects: (Form => Unit)*): DSLSliderBuilder[N] = ???
  }

}
