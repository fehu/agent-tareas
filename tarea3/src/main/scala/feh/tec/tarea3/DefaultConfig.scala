package feh.tec.tarea3

import feh.tec.agent.GenericMutableActorGameCoordinator.Config
import akka.actor.ActorSystem
import scala.concurrent.duration._

trait DefaultConfigBuilder {
  def actorSystemName: String

  lazy val actorSystem = ActorSystem.create(actorSystemName)

  implicit def config = Config(
    actorSystem = ActorSystem.create(),
    executionContext = actorSystem.dispatcher,
    scheduler = actorSystem.scheduler,
    defaultBlockingTimeout = 50,
    defaultFutureTimeout = 60,
    awaitEndOfTurnTimeout = 60 millis span
  )
}

object DefaultConfig{
  def create(nme: String = "default") = new DefaultConfigBuilder{ def actorSystemName = nme }.config
}
