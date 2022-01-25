package co.topl.demo

import akka.actor.typed.{ActorSystem, DispatcherSelector, Extension, ExtensionId}
import cats.effect.unsafe.{IORuntime, IORuntimeConfig, Scheduler}

import scala.concurrent.duration.FiniteDuration

class AkkaCatsRuntime(system: ActorSystem[_]) extends Extension {

  val scheduler = new Scheduler {

    def sleep(delay: FiniteDuration, task: Runnable): Runnable = {
      val c =
        system.scheduler.scheduleOnce(delay, task)(system.executionContext)
      () => c.cancel()
    }

    def nowMillis(): Long = cats.effect.unsafe.implicits.global.scheduler.nowMillis()

    def monotonicNanos(): Long = cats.effect.unsafe.implicits.global.scheduler.monotonicNanos()
  }

  val runtime: IORuntime =
    IORuntime(
      system.executionContext,
      system.dispatchers.lookup(DispatcherSelector.blocking()),
      scheduler = scheduler,
      shutdown = () => system.terminate(),
      config = IORuntimeConfig.apply()
    )
}

object AkkaCatsRuntime extends ExtensionId[AkkaCatsRuntime] {
  def createExtension(system: ActorSystem[_]): AkkaCatsRuntime = new AkkaCatsRuntime(system)
}
