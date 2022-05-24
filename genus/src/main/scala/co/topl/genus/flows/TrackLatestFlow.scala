package co.topl.genus.flows

import akka.stream.scaladsl.Flow
import akka.stream.stage.{AsyncCallback, GraphStageLogic, GraphStageWithMaterializedValue, InHandler, OutHandler}
import akka.stream.{Attributes, FlowShape, Inlet, Outlet}

import scala.concurrent.{Future, Promise}

/**
 * An Akka Streams [[Flow]] which tracks the latest value that has passed through the stream.
 * @tparam T the type of values provided from upstream
 */
class TrackLatestFlow[T] extends GraphStageWithMaterializedValue[FlowShape[T, T], () => Future[T]] {

  private val inlet = Inlet[T]("TrackLatestFlow.Inlet")
  private val outlet = Outlet[T]("TrackLatestFlow.Outlet")

  override def shape: FlowShape[T, T] = FlowShape(inlet, outlet)

  override def createLogicAndMaterializedValue(inheritedAttributes: Attributes): (GraphStageLogic, () => Future[T]) = {
    var asyncCallback: AsyncCallback[Promise[T]] = null

    val logic: GraphStageLogic = new GraphStageLogic(shape) {
      // the list of promises that have not yet received the latest value
      var unfulfilled: List[Promise[T]] = List.empty

      // the latest value that has been passed through the flow (if there has been one)
      var latestValue: Option[T] = None

      override def preStart(): Unit = {
        super.preStart()

        /*
        The handler will provide all new requests with the current state of the latest value.

        If no value has been seen yet, we will add the promise to the list of unfulfilled promises to be
        fulfilled upon the next value received.
         */
        asyncCallback = getAsyncCallback(promise => latestValue.fold(unfulfilled :+= promise)(promise.success))
      }

      setHandler(
        inlet,
        new InHandler {
          def onPush(): Unit = {
            val next = grab(inlet)

            /*
            Update the latest value seen to the element grabbed from upstream.
            Any future requests will be completed with this element until another element passes through this flow.
             */
            latestValue = Some(next)

            // fulfill any existing promises waiting for the first latest value
            unfulfilled.foreach(_.success(next))
            unfulfilled = Nil

            push(outlet, next)
          }
        }
      )

      setHandler(
        outlet,
        new OutHandler {
          def onPull(): Unit = pull(inlet)
        }
      )

    }

    (
      logic,
      () => {
        /*
        When the current value is requested, create a promise for a future value of T, provide that promise to the
        current async handler, and return that value to the caller.
        This will create a channel for the flow to communicate the latest value to the caller in a thread-safe way.
         */
        val promise = Promise[T]()
        asyncCallback.invoke(promise)
        promise.future
      }
    )
  }
}

object TrackLatestFlow {

  /**
   * Creates an Akka Streams [[Flow]] which keeps track of the latest value seen.
   * Materializes to a function for retrieving the latest value
   * @tparam T the type of value provided from upstream
   * @return an instance of a [[Flow]] which takes in and outputs a value of T with a materialized function for
   *         retrieving the latest value
   */
  def create[T]: Flow[T, T, () => Future[T]] = Flow.fromGraph(new TrackLatestFlow)

}
