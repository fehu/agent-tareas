package feh.tec.agentes.tarea1

import feh.tec.map._
import feh.tec.map.tile.{SquareTile, OptionalMabObjectContainerTile, MapObject}
import java.util.UUID
import feh.tec.util.RangeWrapper._
import scala.{Predef, collection, Some}
import feh.tec.agent.{Route, AgentId}
import scala.collection.mutable
import feh.tec.util._
import spray.json._

/*
  todo: move
 */
  class Map(xRange: Range, yRange: Range, buildTilesMap: Map => Predef.Map[(Int, Int), SqTile])
  extends AbstractSquareMap[SqTile] with EnclosedMap[SqTile, (Int, Int)] with AgentsPositionsProvidingMap[SqTile, (Int, Int)]
{ map =>

  type Tile = SqTile
  type Coordinate = (Int, Int)

  lazy val tilesInitMap = buildTilesMap(this)

  def tilesToMap = tilesInitMap

  def nNeighbours = 4
  def tiles: Seq[Tile] = tilesInitMap.values.toSeq
  def get: PartialFunction[Map#Coordinate, Tile] = tilesInitMap

  lazy val coordinates = new CoordinatesMeta {
    def xRange: Range = map.xRange
    def yRange: Range = map.yRange
  }


  import SimpleDirection._

  def onCoordinateGridEdge(c: (Int, Int)): Seq[SimpleDirection] = MapHelper.onCoordinateGridEdge(xRange, yRange, c)
  def onCoordinateGridEdge(tile: Tile): Seq[SimpleDirection] = onCoordinateGridEdge(tile.coordinate)

  protected val neighborsMap = collection.mutable.Map.empty[Coordinate, Seq[Coordinate]]

  def getNeighbors(tile: Tile): Seq[Tile] = getNeighbors(tile.coordinate)
  def getNeighbors(c: Coordinate): Seq[Tile] = neighborsMap get c map (_ map get) getOrElse neighborsMap.synchronized{
    val neighbors = findNeighbors(c)
    neighborsMap += c -> neighbors.map(_.coordinate)
    neighbors
  }

  protected def findNeighbors(tile: Tile): Seq[Tile] = findNeighbors(tile.coordinate)
  protected def findNeighbors(c: Coordinate): Seq[Tile] = {
    val edge = onCoordinateGridEdge(c)

    Seq(
      edge.find(_ == Top)     map (_ => get(c._1, yRange.max))  getOrElse get(c._1, c._2 - yRange.step),
      edge.find(_ == Right)   map (_ => get(xRange.min, c._2))  getOrElse get(c._1 + xRange.step, c._2),
      edge.find(_ == Bottom)  map (_ => get(c._1, yRange.min))  getOrElse get(c._1, c._2 + yRange.step),
      edge.find(_ == Left)    map (_ => get(xRange.max, c._2))  getOrElse get(c._1 - xRange.step, c._2)
    )
  }

  // todo: ensure assert is always executed after creation
  protected def assertDefinedAtAllCoordinates(){
    for{
      x <- xRange
      y <- yRange
    } assert(get.isDefinedAt(x -> y), s"Map tile is not defined at ($x, $y)")
  }


  def agentsPositions: Predef.Map[AgentId, Tile] = ??? // to be overridden
}

object Map{
  class SnapshotBuilder extends MapSnapshotBuilder[Map, SqTile, (Int, Int)]{
    lazy val tilesSnapshotBuilder = new SqTile.SnapshotBuilder

    def snapshot(m: Map): MapSnapshot[Map, SqTile, (Int, Int)] = new Map(m.coordinates.xRange, m.coordinates.yRange, null) with MapSnapshot[Map, SqTile, (Int, Int)]{
      lazy val tilesSnapshots: Seq[TileSnapshot[SqTile, (Int, Int)]] = tilesSnapshotsMap.values.toSeq

      override lazy val tilesInitMap: Predef.Map[(Int, Int), SqTile] = ???
      val tilesSnapshotsMap: Predef.Map[(Int, Int), TileSnapshot[SqTile, Coordinate]] =
        m.tilesToMap.mapValues(tilesSnapshotBuilder.snapshot)

      def getSnapshot: PartialFunction[Coordinate, TileSnapshot[SqTile, Coordinate]] = tilesSnapshotsMap

      override def getNeighbors(tile: Tile): Seq[Tile] = ???
      def getNeighborsSnapshots(tile: Tile): Seq[TileSnapshot[SqTile, Coordinate]] = super.getNeighbors(tile).map(tilesSnapshotBuilder.snapshot)

      override protected def findNeighbors(tile: Tile): Seq[Tile] = ???
      def findNeighborsSnapshots(tile: Tile): Seq[TileSnapshot[SqTile, Coordinate]] = super.findNeighbors(tile).map(tilesSnapshotBuilder.snapshot)

      override def tiles: Seq[SqTile] = super[MapSnapshot].tiles
      override def get: PartialFunction[(Int, Int), SqTile] = super[MapSnapshot].get
    }
  }

  lazy val snapshotBuilder = new SnapshotBuilder

  implicit class DirectionOps(map: Map){
    def tileTo(from: (Int, Int), direction: SimpleDirection): SqTile = map.get(positionTo(from, direction))

    def positionTo = MapHelper.positionTo(map.coordinates.xRange, map.coordinates.yRange) _
  }

}

object MapHelper{
  import SimpleDirection._

  def onCoordinateGridEdge(xRange: Range, yRange: Range, c: (Int, Int)): Seq[SimpleDirection] =  {
    val leftRight = PartialFunction.condOpt(c){
      case (x, _) if x == xRange.min => Left
      case (x, _) if x == xRange.max => Right
    }
    val bottomTop = PartialFunction.condOpt(c){
      case (_, y) if y == yRange.min => Top
      case (_, y) if y == yRange.max => Bottom
    }
    (leftRight :: bottomTop :: Nil).flatten
  }


  def positionTo(xRange: Range, yRange: Range)(from: (Int, Int), direction: SimpleDirection): (Int, Int) = {
    val onEdge = onCoordinateGridEdge(xRange, yRange, from)
    direction match{
      case Up if onEdge.exists(_ == Top)      => from._1 -> yRange.max
      case Up                                 => from._1 -> (from._2 - 1)
      case Down if onEdge.exists(_ == Bottom) => from._1 -> yRange.min
      case Down                               => from._1 -> (from._2 + 1)
      case Left if onEdge.exists(_ == Left)   => xRange.max -> from._2
      case Left                               => (from._1 - 1) -> from._2
      case Right if onEdge.exists(_ == Right) => xRange.min -> from._2
      case Right                              => (from._1 + 1) -> from._2
    }
  }
}

case class SqTile(map: Map, coordinate: (Int, Int), contents: Option[MapObj])
  extends OptionalMabObjectContainerTile[SqTile, MapObj, (Int, Int)] with SquareTile[SqTile, (Int, Int)]
{
  def neighbours = map.getNeighbors(this)
}

object SqTile{
  class SnapshotBuilder extends TileSnapshotBuilder[SqTile, (Int, Int)]{
    def snapshot(t: SqTile): TileSnapshot[SqTile, (Int, Int)] = new SqTile(t.map, t.coordinate, t.contents) with TileSnapshot[SqTile, (Int, Int)]{
      def neighboursSnapshots: Seq[TileSnapshot[SqTile, (Int, Int)]] = super[SqTile].neighbours.map(snapshot)
    }
  }
}

trait MapObj extends MapObject{
  def isAgent: Boolean = false
  def isPlug: Boolean = false
  def isHole: Boolean = false
  def isEmpty = !isAgent && !isPlug && !isHole
}
case class AgentAvatar(ag: AgentId) extends MapObj {override def isAgent = true}
case class Plug() extends MapObj { override def isPlug = true }
case class Hole() extends MapObj{ override def isHole: Boolean = true }

object DummyMapGenerator{ outer =>
  trait DummyMapGeneratorHelpers
  trait DummyMapGeneratorHelpersBuilder[H <: DummyMapGeneratorHelpers]{
    def build(xRange: Range, yRange: Range): H
  }

  trait DummyMapGeneratorRandomPositionSelectHelper extends DummyMapGeneratorHelpers{
    def uniqueRandomPosition: (Int, Int)
  }

  implicit object DummyMapGeneratorRandomPositionSelectHelperBuilder
    extends DummyMapGeneratorHelpersBuilder[DummyMapGeneratorRandomPositionSelectHelper]
  {
    def build(xRange: Range, yRange: Range) = new DummyMapGeneratorRandomPositionSelectHelper{
      def genRandomPosition = xRange.randomSelect -> yRange.randomSelect
      val uniqueRandomPosition = genRandomPosition
    }
  }

  def withHelpers[H <: DummyMapGeneratorHelpers](implicit hb: DummyMapGeneratorHelpersBuilder[H]) = new WithHelpers[H]

  class WithHelpers[H <: DummyMapGeneratorHelpers](implicit hb: DummyMapGeneratorHelpersBuilder[H]) {
    def apply(xRange: Range, yRange: Range)(build: H => (Int, Int) => Option[MapObj]): Map = outer.apply(xRange, yRange)(build(hb.build(xRange, yRange)))
    def buildTilesMap(xRange: Range, yRange: Range)(build: H => (Int, Int) => Option[MapObj]) = outer.buildTilesMap(xRange, yRange)(build(hb.build(xRange, yRange)))
  }

  def apply(xRange: Range, yRange: Range)(build: (Int, Int) => Option[MapObj]): Map =
    new Map(
      xRange = xRange,
      yRange = yRange,
      buildTilesMap = buildTilesMap(xRange, yRange)(build)
    )

  def buildTilesMap(xRange: Range, yRange: Range)(build: (Int, Int) => Option[MapObj]) = (map: Map) => (
    for{
      x <- xRange
      y <- yRange
      c = x -> y
    } yield c -> SqTile(map, c, build.tupled(c))
    ).toMap
}

class MapShortestRouteFinder extends ShortestRouteFinder[Map, SqTile, (Int, Int)]{
  def shortestRoutes(map: Map)(from: (Int, Int), to: Set[(Int, Int)]): Predef.Map[(Int, Int), Route[(Int, Int)]] =
    shortestRoutes(Map.snapshotBuilder.snapshot(map))(from, to)

  lazy val floydWarshall = new FloydWarshall

  def findMinimalDistances = floydWarshall.findMinimalDistances _

  private val scopedDistMap = new ScopedState[Option[FloydWarshall#MinDistMap]](None)
  private val scopedGraph = new ScopedState[Option[Graph]](None)

  def withMinDistMap[R](minDistMap: FloydWarshall#MinDistMap, f: MapShortestRouteFinder => R): R = withMinDistMap(minDistMap)(f(this))
  def withMinDistMap[R](minDistMap: FloydWarshall#MinDistMap)(r:  => R): R = scopedDistMap.doWith(Option(minDistMap))(r)

  def onGraph[R](gr: Graph, f: MapShortestRouteFinder => R): R = onGraph(gr)(f(this))
  def onGraph[R](gr: Graph)(r: => R): R = scopedGraph.doWith(Option(gr))(r)

  private def getMinDistMap(gr: Graph) = scopedDistMap.get getOrElse findMinimalDistances(gr)

  private def getGraph(snapshot: MapSnapshot[Map, SqTile, (Int, Int)]) =
    scopedGraph.get getOrElse Graph.mapAsGraph(snapshot)

  def shortestRoutes(snapshot: MapSnapshot[Map, SqTile, (Int, Int)])
                    (from: (Int, Int), to: Set[(Int, Int)]): Predef.Map[(Int, Int), Route[(Int, Int)]] = {
    val graph = getGraph(snapshot)
    floydWarshall.findShortestRoutes(graph, from, to)(getMinDistMap(graph))
  }


  def findClosest(map: Map)(relativelyTo: (Int, Int), what: (Int, Int) => Boolean): Predef.Map[(Int, Int), Int] =
    findClosest(Map.snapshotBuilder.snapshot(map))(relativelyTo, what)
  def findClosest(snapshot: MapSnapshot[Map, SqTile, (Int, Int)])(relativelyTo: (Int, Int), what: (Int, Int) => Boolean): Predef.Map[(Int, Int), Int] = {
    val gr = getGraph(snapshot)
    val minDist = getMinDistMap(gr)
    val acceptablePairs =  minDist.withFilter(_._1._1 == relativelyTo).withFilter(what.tupled apply _._1._2).map{case ((_, c), d) => c -> d}

    if(acceptablePairs.nonEmpty) acceptablePairs.filterMin(_._2).toMap
    else Predef.Map.empty
  }

}

object Graph{
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

case class Graph(nodesMap: Predef.Map[(Int, Int), GraphNode], connections: Seq[((Int, Int), (Int, Int))]){
  lazy val coordinatesRange = (nodesMap.minBy(_._1._1)._1._1 to nodesMap.maxBy(_._1._1)._1._1, nodesMap.minBy(_._1._2)._1._2 to nodesMap.maxBy(_._1._2)._1._2)
}
case class GraphNode(contents: Option[MapObj], coord: (Int, Int))(getConnections: => List[GraphNode]){
  lazy val connections: List[GraphNode] = getConnections
}

/**
 *  http://en.wikipedia.org/wiki/Floyd%E2%80%93Warshall_algorithm
 *  todo: move to [[feh.tec.map]] package
 */
class FloydWarshall{
  type Coord = (Int, Int)
  type MinDistMap = Predef.Map[((Int, Int), (Int, Int)), Int]

/*
  def findMinimalDistances(gr: Graph): MinDistMap = {
    val dists = mutable.HashMap.empty[(Coord, Coord), Float].withDefault(_ => Float.PositiveInfinity) // it's flaot only to put Inf
    def dist(c1: Coord, c2: Coord) = dists(c1 -> c2) //getOrElse sys.error("smth is wrong in Floyd-Warshall")
    def update(c1: Coord, c2: Coord, f: Float) = dists += (c1 -> c2) -> f

    // init known distances
    for((from, to) <- gr.connections) {
      update(from, from, 0)
      update(from, to, 1)
    }

    val coordsSeq = gr.nodesMap.keys

    for{
      k <- coordsSeq
      i <- coordsSeq
      j <- coordsSeq
      newDist = dist(i, k) + dist(k, j)
      if newDist < dist(i, j)
    } update(i, j, newDist)

    dists.toMap.mapValues(_.toInt)
  }
*/

  def findMinimalDistances(gr: Graph): MinDistMap = {
    val dists = Array.fill(
      gr.coordinatesRange._1.length,
      gr.coordinatesRange._2.length,
      gr.coordinatesRange._1.length,
      gr.coordinatesRange._2.length)(Float.PositiveInfinity)

    //mutable.HashMap.empty[(Coord, Coord), Float].withDefault(_ => Float.PositiveInfinity) // it's flaot only to put Inf
    def dist(c1: Coord, c2: Coord) = dists(c1._1)(c1._2)(c2._1)(c2._2) //getOrElse sys.error("smth is wrong in Floyd-Warshall")
    def update(c1: Coord, c2: Coord, f: Float) = dists(c1._1)(c1._2)(c2._1)(c2._2) = f

    // init known distances
    for((from, to) <- gr.connections) {
      update(from, from, 0)
      update(from, to, 1)
    }

    val coordsSeq = gr.nodesMap.keys

    for{
      k <- coordsSeq
      i <- coordsSeq
      j <- coordsSeq
      newDist = dist(i, k) + dist(k, j)
      if newDist < dist(i, j)
    } update(i, j, newDist)

    Predef.Map((for{
      c1 <- gr.nodesMap.keySet.toSeq
      c2 <- gr.nodesMap.keySet
//      if c1 != c2
    } yield (c1 -> c2) -> dist(c1, c2).toInt): _*)
  }

  /**
   *
   * @return one route per destination even if there are more
   */
  def findShortestRoutes(gr: Graph, from: Coord, to: Set[Coord])
                        (minDistMap: MinDistMap = findMinimalDistances(gr)): Predef.Map[Coord, Route[Coord]] = {
    val routes = mutable.HashMap.empty[Coord, (Route[Coord], Int)]
    def appendStep(destination: Coord, step: Coord, distance: Int) = {
      routes.get(destination)
        .map(r => routes += destination -> ((r._1 / step) -> distance))
        .getOrElse(routes += destination -> (Route(step) -> distance))

      distance == 0
    }
    val finished = mutable.HashSet.empty[Coord]


    def rec(pos: Coord){
      val conn = gr.nodesMap(pos).connections
      for{
        dest <- to filterNot finished.contains
        dists = conn.map(n => n -> minDistMap(n.coord, dest))
        (min, distLeft) = dists.minBy(_._2)
      } {
        if(appendStep(dest, min.coord, distLeft)) finished += dest
        else rec(min.coord)
      }
    }

    rec(from)
    routes.mapValues(_._1).toMap
  }
}

class MapJsonSerializer extends AbstractMapSerializer[Map, SqTile, (Int, Int)]{
  import MapJsonSerializer._
  import MapSerialized._

  type Serialized = JsValue

  def toJson(map: Map): JsValue = MapSerialized(map.tilesToMap.mapValues(_.serialize).toSet, map.coordinates.xRange, map.coordinates.yRange).toJson(mapSerializedFormat)

  def toMap(jsVal: JsValue): Map = jsVal.convertTo[MapSerialized] match {
    case MapSerialized(set, x, y) => new Map(x, y, m => set.toMap.mapValues{case Tile(coord, cont) => SqTile(m, coord, cont)})
  }

  def serialize(map: Map): Serialized = toJson(map)
}

object MapJsonSerializer{
  case class Tile(coordinate: (Int, Int), contents: Option[MapObj])
  case class MapSerialized(tilesMap: Set[((Int, Int), Tile)], xRange: Range, yRange: Range)

  implicit class SquareTileWrapprt(tile: SqTile){
    def serialize = Tile(tile.coordinate, tile.contents)
  }

  object MapSerialized extends DefaultJsonProtocol with JsonProtocols{
    implicit object ObjSerializedFormat extends RootJsonFormat[MapObj]{
      def write(obj: MapObj): JsValue = obj match{
        case AgentAvatar(AgentId(id)) => JsArray(JsString("avatar"), JsString(id.toString))
        case Hole() => JsString("hole")
        case Plug() => JsString("plug")
      }

      def read(json: JsValue): MapObj = json match{
        case JsArray(JsString("avatar") :: JsString(id) :: Nil) => AgentAvatar(AgentId(UUID.fromString(id)))
        case JsString("hole") => Hole()
        case JsString("plug") => Plug()
      }
    }
    implicit val tileSerializedFormat = jsonFormat2(Tile)
    implicit val mapSerializedFormat = jsonFormat3(MapSerialized.apply)
  }
}