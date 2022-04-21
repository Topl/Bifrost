package co.topl.catsakka

import akka.Done
import akka.stream.scaladsl.Flow
import akka.stream.stage.{GraphStageLogic, GraphStageWithMaterializedValue, InHandler, OutHandler}
import akka.stream.{Attributes, FlowShape, Inlet, Outlet}

import scala.concurrent.{Future, Promise}

/**
 * A Flow that materializes a Future which completes when downstream signals demand for the first time
 */
object OnFirstDemandFlow {

  def apply[T]: Flow[T, T, Future[Done]] =
    Flow.fromGraph(new OnFirstDemandFlowImpl[T])
}

class OnFirstDemandFlowImpl[T] extends GraphStageWithMaterializedValue[FlowShape[T, T], Future[Done]] {
  private val inlet = Inlet[T]("OnFirstDemandFlowImpl.In")
  private val outlet = Outlet[T]("OnFirstDemandFlowImpl.Out")

  val shape: FlowShape[T, T] = FlowShape(inlet, outlet)

  def createLogicAndMaterializedValue(inheritedAttributes: Attributes): (GraphStageLogic, Future[Done]) = {
    val promise = Promise[Done]()

    val logic =
      new GraphStageLogic(shape) {

        private val normalOutHandler =
          new OutHandler {
            def onPull(): Unit = pull(inlet)
          }

        private val normalInHandler =
          new InHandler {
            def onPush(): Unit = push(outlet, grab(inlet))
          }

        setHandler(
          inlet,
          new InHandler {
            def onPush(): Unit = {
              push(outlet, grab(inlet))
              setHandler(inlet, normalInHandler)
              setHandler(outlet, normalOutHandler)
            }

            override def onUpstreamFinish(): Unit = {
              super.onUpstreamFinish()
              promise.success(Done) // TODO: Success or fail?
            }

            override def onUpstreamFailure(ex: Throwable): Unit = {
              super.onUpstreamFailure(ex)
              promise.failure(ex)
            }

          }
        )

        setHandler(
          outlet,
          new OutHandler {
            def onPull(): Unit = {
              promise.success(Done)
              pull(inlet)
              setHandler(inlet, normalInHandler)
              setHandler(outlet, normalOutHandler)
            }

            override def onDownstreamFinish(cause: Throwable): Unit = {
              super.onDownstreamFinish(cause)
              promise.failure(cause)
            }
          }
        )
      }

    logic -> promise.future
  }

}
