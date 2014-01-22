package feh.tec.util

import scala.concurrent.{Future, ExecutionContext}
import scala.reflect.ClassTag
import scala.concurrent.duration.FiniteDuration
import akka.actor.ActorRef
import akka.pattern._
import feh.util.HasUUID
import feh.util.HasUUID.HasUUIDFutureWrapper

object HasUUIDForAkka {
  implicit class AsyncSendMsgHasUUIDWrapper(a: ActorRef)(implicit context: ExecutionContext) {
    def send[Msg <: HasUUID](msg: Msg) = new {
      def awaitingResponse[Resp <: HasUUID : ClassTag](timeout: FiniteDuration): Future[Resp] =
        (a ? msg)(timeout).mapTo[Resp].havingSameUUID(msg)
    }
  }
}
