package feh.tec.visual.swing

import scala.swing.Component
import javax.swing.JPanel
import java.awt.{Graphics, Canvas => ACanvas}

class Canvas(val canvas: ACanvas) extends Component{
  override lazy val peer: JCanvas = new JCanvas(canvas) with SuperMixin
}

class JCanvas(val canvas: ACanvas) extends JPanel{
  override def paintComponent(g: Graphics){
    canvas.paint(g)
    super.paintComponent(g)
  }
}