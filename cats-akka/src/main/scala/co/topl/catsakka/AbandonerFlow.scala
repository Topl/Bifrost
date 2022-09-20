package co.topl.catsakka

import akka.NotUsed
import akka.stream.{Attributes, FlowShape, Inlet, Materializer, Outlet}
import akka.stream.scaladsl.Flow
import akka.stream.stage.{AsyncCallback, GraphStage, GraphStageLogic, GraphStageLogicWithLogging, InHandler, OutHandler}
import cats.effect.{Async, Fiber}
import cats.implicits._

import scala.concurrent.ExecutionContext

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
    new GraphStageLogicWithLogging(shape) with InHandler with OutHandler {

      implicit private var mat: Materializer = _
      implicit private var ec: ExecutionContext = _
      private var fiber: Fiber[F, Throwable, U] = _
      // Invoked after the Fiber has launched in the cats-effect runtime
      private var fiberStartedCallback: AsyncCallback[Either[Throwable, Fiber[F, Throwable, U]]] = _
      // Invoked when the currently running fiber completes
      private var fiberCompletedCallback: AsyncCallback[Either[Throwable, U]] = _

      override def preStart(): Unit = {
        super.preStart()
        mat = materializer
        ec = materializer.executionContext
        fiberStartedCallback = getAsyncCallback {
          case Left(e) =>
            log.error(e, "Fiber launch failed")
            failStage(e)
          case Right(v) =>
            log.debug("Fiber launched successfully")
            fiberStarted(v)
        }
        fiberCompletedCallback = getAsyncCallback {
          case Left(FCancelled) =>
            log.debug("Fiber canceled")
            handleNextValue()
          case Left(e) =>
            log.error(e, "Fiber failed")
            failStage(e)
          case Right(v) =>
            log.debug("Fiber completed")
            fiberCompleted(v)
        }
      }

      override def postStop(): Unit = {
        super.postStop()
        cancelCurrentFiber()
      }

      setHandlers(inlet, outlet, this)

      def onPush(): Unit = {
        cancelCurrentFiber()
        if (fiber == null) handleNextValue()
      }

      def onPull(): Unit =
        pullIfAvailable()

      /**
       * Launch a fiber to process the value provided by upstream
       */
      private def handleNextValue(): Unit = {
        val fu: F[U] = f(grab(inlet))
        implicitly[FToFuture[F]]
          .apply(Async[F].start(fu))
          .onComplete(r => fiberStartedCallback.invoke(r.toEither))
      }

      /**
       * A computation fiber was successfully launched in the background
       */
      private def fiberStarted(f: Fiber[F, Throwable, U]): Unit = {
        fiber = f
        implicitly[FToFuture[F]]
          .apply(fiber.joinWith(Async[F].raiseError(FCancelled)))
          .onComplete(r => fiberCompletedCallback.invoke(r.toEither))
        // Now that we're working on the received value, eagerly ask upstream to give us the next value
        pullIfAvailable()
      }

      /**
       * The computation fiber completed successfully, so we can finally push the result downstream
       */
      private def fiberCompleted(result: U): Unit = {
        push(outlet, result)
        fiber = null
      }

      private def cancelCurrentFiber(): Unit =
        Option(fiber).foreach { f =>
          log.debug("Canceling fiber")
          implicitly[FToFuture[F]].apply(f.cancel)
        }

      private def pullIfAvailable(): Unit =
        if (!hasBeenPulled(inlet)) pull(inlet)
    }
}

/**
 * A specific exception type that is meant to be ignored by the stage
 */
private case object FCancelled extends Exception
