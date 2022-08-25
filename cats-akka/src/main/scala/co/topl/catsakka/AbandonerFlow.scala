package co.topl.catsakka

import cats.implicits._
import akka.NotUsed
import akka.stream.{Attributes, FlowShape, Inlet, Materializer, Outlet}
import akka.stream.scaladsl.Flow
import akka.stream.stage.{AsyncCallback, GraphStage, GraphStageLogic, InHandler, OutHandler}
import cats.effect.{Async, Fiber}

import scala.concurrent.ExecutionContext
import scala.util.Failure

/**
 * A flow which applies a function `f` to each input element.  If the upstream produces faster than the function
 * can complete, then the current operation is cancelled and the input value abandoned.
 */
object AbandonerFlow {

  def apply[F[_]: Async: FToFuture, T, U](f: T => F[U]): Flow[T, U, NotUsed] =
    Flow.fromGraph(new AbandonerFlowImpl[F, T, U](f))
}

private class AbandonerFlowImpl[F[_]: Async: FToFuture, T, U](f: T => F[U]) extends GraphStage[FlowShape[T, U]] {
  val inlet: Inlet[T] = Inlet("AbandonerFlow.In")
  val outlet: Outlet[U] = Outlet("AbandonerFlow.Out")
  val shape: FlowShape[T, U] = FlowShape(inlet, outlet)

  def createLogic(inheritedAttributes: Attributes): GraphStageLogic =
    new GraphStageLogic(shape) with InHandler with OutHandler {

      implicit private var mat: Materializer = _
      implicit private var ec: ExecutionContext = _
      private var fiber: Fiber[F, Throwable, U] = _
      private var currentCancelledCallback: AsyncCallback[Either[Throwable, T]] = _
      private var fiberStartedCallback: AsyncCallback[Either[Throwable, Fiber[F, Throwable, U]]] = _
      private var fiberCompletedCallback: AsyncCallback[Either[Throwable, U]] = _

      override def preStart(): Unit = {
        super.preStart()
        mat = materializer
        ec = materializer.executionContext
        currentCancelledCallback = getAsyncCallback {
          case Left(e)  => failStage(e)
          case Right(v) => handleNextValue(v)
        }
        fiberStartedCallback = getAsyncCallback {
          case Left(e)  => failStage(e)
          case Right(v) => fiberStarted(v)
        }
        fiberCompletedCallback = getAsyncCallback {
          case Left(e)  => failStage(e)
          case Right(v) => fiberCompleted(v)
        }
      }

      setHandlers(inlet, outlet, this)

      private def handleNextValue(nextValue: T): Unit = {
        fiber = null
        implicitly[FToFuture[F]]
          .apply(Async[F].start(f(nextValue)))
          .onComplete(r => fiberStartedCallback.invoke(r.toEither))
      }

      private def fiberStarted(f: Fiber[F, Throwable, U]): Unit = {
        fiber = f
        implicitly[FToFuture[F]]
          .apply(fiber.joinWith(Async[F].raiseError(FCancelled)))
          .andThen {
            // Ignore the Cancellation
            case Failure(FCancelled) =>
            case r                   => fiberCompletedCallback.invoke(r.toEither)
          }
        // Now that the new fiber is running in the background
        pull(inlet)
      }

      private def fiberCompleted(result: U): Unit =
        push(outlet, result)

      def onPush(): Unit = {
        val in = grab(inlet)
        Option(fiber) match {
          case Some(f) =>
            implicitly[FToFuture[F]]
              .apply(f.cancel.as(in))
              .onComplete(r => currentCancelledCallback.invoke(r.toEither))
          case _ =>
            handleNextValue(in)
        }
      }

      def onPull(): Unit =
        if (!hasBeenPulled(inlet)) pull(inlet)
    }
}

/**
 * A specific exception type that is meant to be ignored by the stage
 */
private case object FCancelled extends Exception
