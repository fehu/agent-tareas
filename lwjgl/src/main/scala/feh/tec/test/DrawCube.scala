package feh.tec.test

import nicol._
import nicol.input.Key._
import feh.tec.util.Lifted
import nicol.Init

class DrawCube(exitScene: Lifted[Scene], pauseScene: Scene => Scene) extends LoopScene with SyncableScene with ShowFPS{

  type Coord = (Float, Float, Float)
  val Pi = scala.math.Pi.toFloat
  def cos = scala.math.cos _ andThen(_.toFloat)
  def sin = scala.math.sin _ andThen(_.toFloat)
  def sqrt = scala.math.sqrt _ andThen(_.toFloat)

  implicit class TripleFloatWrapper(v: (Float, Float, Float)){
    def + (v2: (Float, Float, Float)) = (v._1 + v2._1, v._2 + v2._2, v._3 + v2._3)
    def rotatePhi(phi: Float) = (v._1 * cos(phi), v._2* sin(phi), v._3)
    def zProjection = sqrt(v._1 * v._1+ v._2 * v._2)
    def rotateTheta(phi: Float) = (v._1, v._2, v._3 * )
  }

  case class Rect3d(center: Coord, edgeX: Float, edgeY: Float, phi: Float, theta: Float)
  case class Cube(center: Coord, edge: Float, phi: Float, theta: Float)
  case class Camera(position: Coord, phi: Float, theta: Float){
    lazy val viewPlane = Rect3d(position + (1, 1, 1), 2, 1)
  }
  
  val cube = Cube((1, 1, 1), 2, 0, 0)
  val camera = Camera((5, 4, 4), Pi + Pi / 4, Pi + Pi / 4)


  def update: Option[Scene] = {
    sync
    showFPS

    render()

    keyEvent {
      e =>
        e released {
          case _ =>
        }
        e pressed {
          case "escape" => exitScene()
          case "space" => pauseScene(this)
        }
    }
  }

  def render(){

  }

}

//object DrawCube{
//  def drawCube()
//}

class DrawCubeGame extends Game(Init("Draw Cube Test") >> new DrawCube)