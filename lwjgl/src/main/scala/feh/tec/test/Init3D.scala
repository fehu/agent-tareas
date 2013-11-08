package feh.tec.test

import nicol.Scene
import org.lwjgl.opengl.{GL11, DisplayMode, Display}
import org.lwjgl.util.glu.GLU

case class Init3D (title: String, width: Int = 800, height: Int = 600, vSync: Boolean = true, fullscreen: Boolean = false) extends Scene {
  def apply() {
    Display.setTitle(title)
    if(vSync) Display.setVSyncEnabled(true)

    val target = Display.getAvailableDisplayModes.find { d =>
      d.getWidth == width && d.getHeight == height
    } getOrElse (new DisplayMode(width, height))

    Display.setDisplayMode(target)

    // Try fullscreen
    Display.setFullscreen(fullscreen)

    Display.create

    import GL11._

//    glMatrixMode(GL_PROJECTION)
//    glLoadIdentity()
//
//    glMatrixMode(GL_MODELVIEW)
//    glOrtho(0, width, height, 0, 1, -1)
//    glViewport(0, 0, width, height)
//
//    glEnable(GL_TEXTURE_2D)
//    glEnable(GL_BLEND)
//    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)


    glMatrixMode(GL_PROJECTION)
    GLU.gluPerspective(70f, 800f/600f, 1, 1000)
    glViewport(0, 0, Display.getWidth(), Display.getHeight())

    glMatrixMode(GL_MODELVIEW)
    glLoadIdentity()

    // Enable Depth Testing
    glEnable(GL_DEPTH_TEST)

    // Enable client states
    glEnableClientState(GL_VERTEX_ARRAY)
    glEnableClientState(GL_COLOR_ARRAY)

//    glMatrixMode(GL_PROJECTION);
//    glOrtho(-1, 1, -1, 1, 1, -1);
//
//    glMatrixMode(GL_MODELVIEW);
//    glLoadIdentity();
//
//    glViewport(0, 0, Display.getWidth(), Display.getHeight());
//
//    // Enable Depth Test
//    glEnable(GL_DEPTH_TEST);

//
//    glMatrixMode(GL_PROJECTION);
//    glOrtho(-1, 1, -1, 1, 1, -10);
//
//    glMatrixMode(GL_MODELVIEW);
//    glLoadIdentity();
//
//    glViewport(0, 0, width, height)
//
//    glEnable(GL_DEPTH_TEST);

  }
}

trait LoopScene3D extends Scene {
  val clearColour: (Float, Float, Float, Float) = (0, 0, 0, 0)

  def apply = {
    var next: Option[Scene] = None

    import GL11._

    while (next == None) {
//      glClearColor(clearColour._1, clearColour._2, clearColour._3, clearColour._4)
//      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
//      GL11.glClear(GL11.GL_COLOR_BUFFER_BIT)
//      glLoadIdentity

      next = update
      Display.update
    }

    next.get.apply
  }

  def update: Option[Scene]
}
