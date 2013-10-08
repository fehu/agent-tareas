package feh.tec.agentes.tarea1

import spray.json._
import scala.collection.immutable.Range.Inclusive

trait JsonProtocols extends DefaultJsonProtocol{
  implicit object RangeFormat extends RootJsonFormat[Range]{
    def write(obj: Range): JsValue = JsArray(JsNumber(obj.start), JsNumber(obj.end), JsNumber(obj.step), JsBoolean(obj.isInclusive))

    def read(json: JsValue): Range = json match{
      case JsArray(JsNumber(start) :: JsNumber(end) :: JsNumber(step) :: JsBoolean(inclusive) :: Nil) =>
        if(inclusive) new Inclusive(start.toInt, end.toInt, step.toInt)
        else new Range(start.toInt, end.toInt, step.toInt)
    }
  }
}

object JsonProtocols extends JsonProtocols