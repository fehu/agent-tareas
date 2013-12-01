package feh.tec.visual.util

import java.awt.event.{ComponentListener, ComponentEvent}

trait AwtEventUtils extends AwtUtils{
  object ComponentListener{
    def apply(resized:  ComponentEvent => Unit  =  _ => {},
              moved:    ComponentEvent => Unit  =  _ => {},
              shown:    ComponentEvent => Unit  =  _ => {},
              hidden:   ComponentEvent => Unit  =  _ => {}
              ) =
      new ComponentListener {
        def componentShown(e: ComponentEvent) = shown(e)
        def componentHidden(e: ComponentEvent) = hidden(e)
        def componentMoved(e: ComponentEvent) = moved(e)
        def componentResized(e: ComponentEvent) = resized(e)
      }
  }
}

object AwtEventUtils extends AwtEventUtils