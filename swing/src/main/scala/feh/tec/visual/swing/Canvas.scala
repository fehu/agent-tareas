package feh.tec.visual.swing

import scala.swing.Component
import scala.swing.Swing._
import javax.swing.{JPanel, UIManager, JComponent}
import javax.swing.plaf.ComponentUI
import java.awt.Graphics

class Canvas extends Component{
  override lazy val peer: JCanvas = new JCanvas with SuperMixin
  def canvas = peer.canvas
}

class JCanvas extends JPanel{
  lazy val canvas = new java.awt.Canvas

  override def paintComponent(g: Graphics){
    canvas.paint(g)
    super.paintComponent(g)
  }
}