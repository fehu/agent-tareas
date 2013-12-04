package feh.tec.util

import spray.json._

trait JsonImplicits {
  implicit def stringToJson: String => JsString = JsString(_)
  implicit def numericToJson[N: Numeric]: N => JsNumber = n => JsNumber(BigDecimal(n.toString))  // todo
  implicit def booleanToJson: Boolean => JsBoolean = JsBoolean(_)

  implicit def seqToJsArray[J <% JsValue](seq: Seq[J])= JsArray(seq.map(x => x: JsValue).toList)

  implicit def toFieldWrapper[J <% JsValue](p: (String, J)): JsField = p._1 -> p._2
}
