package co.topl.genus.flows

import akka.stream.scaladsl.Flow
import akka.stream.stage.{GraphStageLogic, GraphStageWithMaterializedValue, InHandler, OutHandler}
import akka.stream.{Attributes, FlowShape, Inlet, Outlet}

/**
 * An Akka Streams [[Flow]] which tracks the latest value that has passed through the stream.
 * @param initial the initial and default value
 * @tparam T the type of values provided from upstream
 */
class TrackLatestFlow[T](initial: T) extends GraphStageWithMaterializedValue[FlowShape[T, T], () => T] {

  private val inlet = Inlet[T]("RetriveLatestValue.Inlet")
  private val outlet = Outlet[T]("RetrieveLatestValue.Outlet")

  override def shape: FlowShape[T, T] = FlowShape(inlet, outlet)

  override def createLogicAndMaterializedValue(inheritedAttributes: Attributes): (GraphStageLogic, () => T) = {
    var latestValue: T = initial

    val logic = new GraphStageLogic(shape) {
      setHandler(
        inlet,
        new InHandler {
          def onPush(): Unit = {
            // get the next value, set it as the latest value seen, and push the value downstream
            val next = grab(inlet)
            latestValue = next
            push(outlet, next)
          }

          override def onUpstreamFinish(): Unit = super.onUpstreamFinish()

          override def onUpstreamFailure(ex: Throwable): Unit = super.onUpstreamFailure(ex)

        }
      )

      setHandler(
        outlet,
        new OutHandler {
          def onPull(): Unit = pull(inlet)

          override def onDownstreamFinish(cause: Throwable): Unit = super.onDownstreamFinish(cause)
        }
      )
    }

    // any calls to the materialized value will retrieve the current state of the latest value variable
    logic -> (() => latestValue)
  }
}

object TrackLatestFlow {

  /**
   * Creates an Akka Streams [[Flow]] which keeps track of the latest value seen.
   * Materializes to a function for retrieving the latest value
   * @param initial the initial and default value
   * @tparam T the type of value provided from upstream
   * @return an instance of a [[Flow]] which takes in and outputs a value of T with a materialized function for
   *         retrieving the latest value
   */
  def create[T](initial: T): Flow[T, T, () => T] = Flow.fromGraph(new TrackLatestFlow(initial))
}
