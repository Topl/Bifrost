package co.topl.testnetsimulationorchestrator.interpreters

import cats.effect.std.Dispatcher
import cats.effect.{Async, Deferred, Resource, Sync}
import cats.implicits._
import co.topl.testnetsimulationorchestrator.algebras.SimulationController
import io.kubernetes.client.openapi.apis.CoreV1Api
import io.kubernetes.client.openapi.models.V1Status
import io.kubernetes.client.openapi.{ApiCallback, ApiException}
import io.kubernetes.client.util.Config
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.util

object K8sSimulationController {

  def resource[F[_]: Async](namespace: String): Resource[F, SimulationController[F]] =
    for {
      config <- Resource.eval(Sync[F].delay(Config.fromCluster()))
      api = new CoreV1Api(config)
      implicit0(logger: Logger[F]) <- Resource.eval(Slf4jLogger.fromName("Bifrost.K8sSimulationController"))
    } yield new SimulationController[F] {

      def terminate: F[Unit] = withCallback[V1Status](
        api.deleteNamespaceAsync(namespace, null, null, null, null, null, null, _)
      ).void

      private def createDeferredCallback[T]: Resource[F, (ApiCallback[T], F[T])] =
        for {
          dispatcher <- Dispatcher.parallel[F]
          deferred   <- Resource.eval(Deferred[F, Either[Throwable, T]])
          callback = new ApiCallback[T] {

            def onFailure(
              e:               ApiException,
              statusCode:      Int,
              responseHeaders: util.Map[String, util.List[String]]
            ): Unit =
              dispatcher.unsafeRunAndForget(deferred.complete(e.asLeft))

            def onSuccess(result: T, statusCode: Int, responseHeaders: util.Map[String, util.List[String]]): Unit =
              dispatcher.unsafeRunAndForget(deferred.complete(result.asRight))

            def onUploadProgress(bytesWritten: Long, contentLength: Long, done: Boolean): Unit = {}

            def onDownloadProgress(bytesRead: Long, contentLength: Long, done: Boolean): Unit = {}
          }
        } yield (callback, deferred.get.rethrow)

      private def withCallback[T](f: ApiCallback[T] => Unit): F[T] =
        createDeferredCallback[T]
          .use { case (callback, f1) =>
            Sync[F].delay(f(callback)) >> f1
          }
          .onError { case e: ApiException =>
            Logger[F].error(e)(s"message=${e.getMessage}. (code=${e.getCode})  responseBody=${e.getResponseBody}")
          }

    }
}
