package feh.tec.util

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}
import akka.actor.Actor
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

  implicit class AsyncSendMsgHasUUIDWrapper(a: Actor)(implicit context: ExecutionContext) {
    def send[Msg <: HasUUID](msg: Msg) = new {
      def awaitingResponse[Resp <: HasUUID : ClassTag](timeout: FiniteDuration): Future[Resp] =
        (a.self ? msg)(timeout).mapTo[Resp].havingSameUUID(msg)
    }
  }

  /*
  (agent.self ? msg)(getMapMaxDelay)
        .mapTo[MapByEnvRef].sameUUID(msg).withFilter.map(_.snapshot)
      }
   */
}

class UUIDed(final val uuid : UUID = UUID.randomUUID()) extends HasUUID