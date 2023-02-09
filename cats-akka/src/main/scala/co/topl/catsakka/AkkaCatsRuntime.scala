package co.topl.catsakka

import cats.implicits._
import akka.actor.typed.{ActorSystem, DispatcherSelector, Extension, ExtensionId}
import cats.effect.{Async, Resource, Sync}
import cats.effect.unsafe.{IORuntime, IORuntimeConfig, Scheduler}

import scala.concurrent.Await
import scala.concurrent.duration.FiniteDuration

class AkkaCatsRuntime(system: ActorSystem[_]) extends Extension {

  val ioRuntimeConfig: IORuntimeConfig = IORuntimeConfig.apply()

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
      shutdown = () => {
        system.terminate()
        import scala.concurrent.duration._
        Await.result(system.whenTerminated, 30.seconds)
      },
      config = ioRuntimeConfig
    )
}

object AkkaCatsRuntime extends ExtensionId[AkkaCatsRuntime] {
  def createExtension(system: ActorSystem[_]): AkkaCatsRuntime = new AkkaCatsRuntime(system)

  /**
   * Create an ActorSystem within a Cats Resource
   */
  def systemResource[F[_]: Async, T](createSystem: => ActorSystem[T]): Resource[F, ActorSystem[T]] =
    Resource
      .make(Sync[F].delay(createSystem))(system =>
        Sync[F].delay(system.terminate()) >> Async[F].fromFuture(Sync[F].delay(system.whenTerminated)).void
      )
}
