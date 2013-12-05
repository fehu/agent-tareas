package feh.tec.agent.conf

import akka.actor.{Scheduler, ActorSystem}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

case class AppConfig(actorSystem: ActorSystem,
                     executionContext: ExecutionContext,
                     scheduler: Scheduler,
                     defaultBlockingTimeout: Int,
                     defaultFutureTimeout: Int,
                     awaitEndOfTurnTimeout: FiniteDuration)


trait AppConfigBuilder {
  def actorSystemName: String

  lazy val actorSystem = ActorSystem.create(actorSystemName)

  implicit def config = AppConfig(
    actorSystem = ActorSystem.create(),
    executionContext = actorSystem.dispatcher,
    scheduler = actorSystem.scheduler,
    defaultBlockingTimeout = 50,
    defaultFutureTimeout = 60,
    awaitEndOfTurnTimeout = 60 millis span
  )
}

object DefaultAppConfig{
  def create(nme: String = "default") = new AppConfigBuilder{ def actorSystemName = nme }.config
}
