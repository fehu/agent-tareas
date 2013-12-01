package feh.tec.util

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}
import akka.actor.{ActorRef, Actor}
import akka.pattern._
import scala.concurrent.duration.FiniteDuration
import scala.reflect.ClassTag

trait HasUUID {
  val uuid: UUID
}

object HasUUID{
  implicit class HasUUIDFutureWrapper[T <: HasUUID](f: Future[T]){
    def havingSameUUID(other: HasUUID)(implicit context: ExecutionContext) = f.withFilter(_.uuid == other.uuid)
  }

  implicit class HasUUIDOptionWrapper[T <: HasUUID](opt: Option[T]){
    def havingSameUUID(other: HasUUID) = opt.filter(_.uuid == other.uuid)
  }

  implicit class AsyncSendMsgHasUUIDWrapper(a: ActorRef)(implicit context: ExecutionContext) {
    def send[Msg <: HasUUID](msg: Msg) = new {
      def awaitingResponse[Resp <: HasUUID : ClassTag](timeout: FiniteDuration): Future[Resp] =
        (a ? msg)(timeout).mapTo[Resp].havingSameUUID(msg)
    }
  }
}

class UUIDed(final val uuid : UUID = UUID.randomUUID()) extends HasUUID