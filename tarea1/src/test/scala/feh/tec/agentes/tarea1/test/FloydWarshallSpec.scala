package feh.tec.agentes.tarea1.test

import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import feh.tec.agentes.tarea1._
import feh.tec.map.{TileSnapshot, MapSnapshot}
import feh.tec.agentes.tarea1.Graph
import feh.tec.agentes.tarea1.GraphNode

class FloydWarshallSpec extends Specification with ScalaCheck with Arbitraries{

  "Map graph" should{
    "be created from map correctly" in {
      testGraph1Nodes and testGraph1Connections
     }
  }

  "Floyd-Warshall algorithm implementation" should{
    "find minimal distances correctly" in {
      (minDists1((1, 2), (1, 3)) mustEqual 1) and
      (minDists1((1, 2), (1, 4)) mustEqual 2) and
      (minDists1((1, 5), (1, 2)) mustEqual 3)
    }
  }

  val xRange = 1 to 5
  val yRange = 1 to 5
  def tiles1(m: Map) = (for{x <- xRange; y <- yRange} yield SqTile(m, x -> y, PartialFunction.condOpt(x -> y){
    case (1, 1) => Hole()
    case (1, 3) => Plug()
  })).map(t => t.coordinate -> t).toMap

  lazy val floydWarshall = new FloydWarshall
  lazy val map1 = new Map(xRange, yRange, m => tiles1(m))
  lazy val graph1 = mapAsGraph(map1)
  lazy val minDists1 = floydWarshall.findMinimalDistances(graph1)

  def testGraph1Nodes =
    (for{
      x <- xRange
      y <- yRange
      nodeOpt = graph1.nodesMap.get(x -> y)
    } yield x -> y match {
      case (1, 1) => nodeOpt must beEmpty
      case (1, 3) => nodeOpt.get.contents.exists(_.isPlug) must beTrue
      case _ => nodeOpt.get.contents must beEmpty
    }).reduceLeft(_ and _)

  def testGraph1Connections =
    (for{
      c1 <- xRange.flatMap(x => yRange.map(x ->))
      c2 <- xRange.flatMap(x => yRange.map(x ->))
      if c1 != c2
      connOpt = graph1.connections.find(c1 -> c2 ==)
    } yield c1 -> c2 match{
      case ((1, 1), _) => connOpt must beEmpty
      case (_, (1, 1)) => connOpt must beEmpty
      case ((x1, y1), (x2, y2)) =>
        val diffX = math.abs(x1 - x2)
        val diffY = math.abs(y1 - y2)
        def isNeighbour =
          ((diffX == 1 || diffX == xRange.length - 1) && diffY == 0) ||
          ((diffY == 1 || diffY == yRange.length - 1) && diffX == 0)

        if(isNeighbour) connOpt must beSome
        else connOpt must beNone
    }).reduceLeft(_ and _)

  lazy val mapSnapshotBuilder = Map.snapshotBuilder
  def mapAsGraph(m: Map): Graph = mapAsGraph(mapSnapshotBuilder.snapshot(m))
  def mapAsGraph(snapshot: MapSnapshot[Map, SqTile, (Int, Int)]): Graph = {
    def findConnections(tile: TileSnapshot[SqTile, (Int, Int)]): Seq[((Int, Int), (Int, Int))] =
      tile.neighboursSnapshots.filterNot(_.asTile.contents.exists(_.isHole)).map(tile.coordinate -> _.coordinate)

    val filteredSnapshots = snapshot.tilesSnapshots.filterNot(_.asTile.contents.exists(_.isHole))
    val nodesConnections: Seq[((Int, Int), (Int, Int))] = filteredSnapshots.flatMap(findConnections)

    def connections(coordinate: (Int, Int)) = nodesConnections.filter(_._1 == coordinate)
      .map(_._1).map(snapshot.getSnapshot andThen buildGraphNode).toList

    def buildGraphNode(ts: TileSnapshot[SqTile, (Int, Int)]): GraphNode =
      GraphNode(ts.asTile.contents, ts.coordinate)(connections(ts.coordinate))

    val nodesMap = filteredSnapshots.map(ts => ts.coordinate -> buildGraphNode(ts)).toMap

    Graph(nodesMap, nodesConnections)
  }

}
