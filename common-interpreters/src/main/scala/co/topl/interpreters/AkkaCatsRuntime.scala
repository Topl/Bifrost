package co.topl.interpreters

import akka.actor.typed.{ActorSystem, DispatcherSelector, Extension, ExtensionId}
import cats.effect.unsafe.{IORuntime, IORuntimeConfig, Scheduler}

import scala.concurrent.duration.FiniteDuration

class AkkaCatsRuntime(system: ActorSystem[_]) extends Extension {

  val ioRuntimeConfig = IORuntimeConfig.apply()

  val scheduler: Scheduler = new Scheduler {

    def sleep(delay: FiniteDuration, task: Runnable): Runnable = {
      val c =
        system.scheduler.scheduleOnce(delay, task)(system.executionContext)
      () => c.cancel()
    }

    def nowMillis(): Long = System.currentTimeMillis()

    def monotonicNanos(): Long = System.nanoTime()
  }

  val runtime: IORuntime =
    IORuntime(
      system.executionContext,
      system.dispatchers.lookup(DispatcherSelector.blocking()),
      scheduler = scheduler,
      shutdown = () => system.terminate(),
      config = ioRuntimeConfig
    )
}

object AkkaCatsRuntime extends ExtensionId[AkkaCatsRuntime] {
  def createExtension(system: ActorSystem[_]): AkkaCatsRuntime = new AkkaCatsRuntime(system)
}
