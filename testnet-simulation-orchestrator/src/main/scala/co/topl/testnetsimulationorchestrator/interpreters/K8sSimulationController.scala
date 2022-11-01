package co.topl.testnetsimulationorchestrator.interpreters

import cats.effect.std.Dispatcher
import cats.effect.{Async, Deferred, Resource, Sync}
import cats.implicits._
import co.topl.testnetsimulationorchestrator.algebras.SimulationController
import io.kubernetes.client.openapi.apis.CoreV1Api
import io.kubernetes.client.openapi.{ApiCallback, ApiException, Configuration}
import io.kubernetes.client.util.Config

import java.util

object K8sSimulationController {

  def resource[F[_]: Async](namespace: String): Resource[F, SimulationController[F]] =
    Resource
      .make(
        Sync[F].delay(Config.defaultClient())
      )(_ => ().pure[F])
      .evalTap(c => Sync[F].delay(Configuration.setDefaultApiClient(c)))
      .as(new CoreV1Api())
      .map(api =>
        new SimulationController[F] {

          def terminate: F[Unit] = withCallback(
            api.deleteNamespaceAsync(namespace, null, null, null, null, null, null, _)
          ).void

          private def createDeferredCallback[T]: Resource[F, (ApiCallback[T], F[T])] =
            Dispatcher[F]
              .evalMap(dispatcher =>
                (Deferred[F, T], Deferred[F, Throwable]).mapN((s, f) =>
                  new ApiCallback[T] {

                    def onFailure(
                      e:               ApiException,
                      statusCode:      Int,
                      responseHeaders: util.Map[String, util.List[String]]
                    ): Unit =
                      dispatcher.unsafeRunAndForget(f.complete(e))

                    def onSuccess(result: T, statusCode: Int, responseHeaders: util.Map[String, util.List[String]])
                      : Unit =
                      dispatcher.unsafeRunAndForget(s.complete(result))

                    def onUploadProgress(bytesWritten: Long, contentLength: Long, done: Boolean): Unit = {}

                    def onDownloadProgress(bytesRead: Long, contentLength: Long, done: Boolean): Unit = {}
                  } -> Async[F].race(f.get, s.get).rethrow
                )
              )

          private def withCallback[T](f: ApiCallback[T] => Unit): F[T] =
            createDeferredCallback[T].use { case (callback, f1) =>
              Sync[F].delay(f(callback)) >> f1
            }

        }
      )
}
