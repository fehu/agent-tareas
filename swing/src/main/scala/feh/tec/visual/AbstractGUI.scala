package feh.tec.visual

import scala.swing.event.Event
import scala.swing.Component

trait AbstractGUI extends SwingFrameAppCreation with SwingFrameAppCreation.LayoutDSL{
  object Description{
    sealed trait Conf

    /**
     * General idea is using it to define APIs for elements
     */
    class Elem[Builder <: SwingFrameAppCreation.AbstractDSLBuilder](defaultBuilder: Builder) extends Conf{
      private var _builder = defaultBuilder
      def builder: Builder = _builder
      def reconfigure(f: (Builder => Builder)*) = {
        _builder = Function.chain(f)(builder)
        this
      }
      def has[X](depend: X) = new Elem(builder) with Has[X]{ val dependency = depend }
    }

    trait EventHandler{
      self: Conf =>
      type HandleEvent[E <: Event] = E => Unit
    }
    trait HandlesEvent[E <: Event] extends EventHandler{
      self: Conf =>
      def handle: HandleEvent[E]
    }
    trait Handles2Events[E1 <: Event, E2 <: Event] extends EventHandler{
      self: Conf =>
      def handle1: HandleEvent[E1]
      def handle2: HandleEvent[E2]
    }

    trait Needs[X] {
      self: Conf =>
      def dependency: X
    }

    trait Has[X] extends Needs[X]{ self: Conf => }
    
    object Elem{
      def apply[Builder <: SwingFrameAppCreation.AbstractDSLBuilder](b: Builder) = new Elem[Builder](b)
      def apply[Builder <: SwingFrameAppCreation.AbstractDSLBuilder, E <: Event](b: Builder, event: E => Unit) =
        new Elem[Builder](b) with HandlesEvent[E]{ def handle = event }
    }

    implicit def builderToElem[Builder <: SwingFrameAppCreation.AbstractDSLBuilder](b: Builder): Elem[Builder] = Elem(b)
		implicit def elemToBuilder[Builder <: SwingFrameAppCreation.AbstractDSLBuilder](el: Elem[Builder]): Builder = el.builder
		implicit def elemToComponent(el: Elem[_ <: SwingFrameAppCreation.AbstractDSLBuilder]): Component = el.builder.component
		implicit def formElemToBuildMeta[B <: SwingFrameAppCreation.DSLFormBuilder[_]](el: Elem[B]): SwingFrameAppCreation.BuildMeta = el.builder.formMeta

    implicit def panelElemToBuildMeta(el: Elem[PanelBuilder]): SwingFrameAppCreation.BuildMeta = el.builder.meta
    implicit def boxPanelElemToBuildMeta(el: Elem[BoxPanelBuilder]): SwingFrameAppCreation.BuildMeta = el.builder.meta
    implicit def elemToGridBagMeta(el: Elem[GridBagBuilder]): SwingFrameAppCreation.BuildMeta = el.builder

		
    type AbstractBuilder = SwingFrameAppCreation.AbstractDSLBuilder
    type LabelBuilder[T] = SwingFrameAppCreation.DSLLabelBuilder[T]
    type ButtonBuilder = SwingFrameAppCreation.DSLButtonBuilder
    type KeyedListBuilder[K, V] = SwingFrameAppCreation.DSLKeyedListBuilder[K, V]
    type PanelBuilder = SwingFrameAppCreation.DSLPanelBuilder
    type GridBagBuilder = SwingFrameAppCreation.GridBagMeta
    type BoxPanelBuilder = SwingFrameAppCreation.DSLBoxPanelBuilder
  }
}