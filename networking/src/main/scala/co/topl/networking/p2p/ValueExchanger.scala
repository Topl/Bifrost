package co.topl.networking.p2p

import akka.stream.scaladsl.{Flow, Keep, Source}
import akka.stream.stage.{GraphStageLogic, GraphStageWithMaterializedValue, InHandler, OutHandler}
import akka.stream.{Attributes, FlowShape, Inlet, Outlet}
import akka.util.ByteString
import cats.implicits._

import scala.concurrent.{Future, Promise}

/**
 * Expects a value of some type T from upstream, and sends `localValue` of type T downstream.  Once upstream sends
 * the value, the sub-flow is materialized using the `next` function.
 */
object ValueExchanger {

  def apply[T, Mat](
    localValue:  => T,
    encodeValue: T => ByteString,
    decodeValue: ByteString => Option[(T, ByteString)],
    next:        T => Flow[ByteString, ByteString, Mat]
  ): Flow[ByteString, ByteString, Future[Mat]] =
    Flow.fromGraph(new ValueExchangerImpl[T, Mat](localValue, encodeValue, decodeValue, next))
}

class ValueExchangerImpl[T, Mat](
  localValue:  => T,
  encodeValue: T => ByteString,
  decodeValue: ByteString => Option[(T, ByteString)],
  next:        T => Flow[ByteString, ByteString, Mat]
) extends GraphStageWithMaterializedValue[FlowShape[ByteString, ByteString], Future[Mat]] {

  private val inlet: Inlet[ByteString] = Inlet("ValueExchangerImpl.In")
  private val outlet: Outlet[ByteString] = Outlet("ValueExchangerImpl.Out")

  val shape: FlowShape[ByteString, ByteString] = FlowShape(inlet, outlet)

  def createLogicAndMaterializedValue(inheritedAttributes: Attributes): (GraphStageLogic, Future[Mat]) = {
    val matPromise = Promise[Mat]()
    val logic = new ValueExchangerImplLogic(matPromise)
    logic -> matPromise.future
  }

  private class ValueExchangerImplLogic(matPromise: Promise[Mat]) extends GraphStageLogic(shape) {

    def setUninitializedHandlers(): Unit = {
      var buffer: ByteString = ByteString.empty
      var pullOnInit = false
      var localDataSent = false
      var remoteData = none[T]

      def moveToInitialized(remoteData: T): Unit =
        setInitializedHandlers(remoteData, buffer, pullOnInit)

      setHandler(
        inlet,
        new InHandler {
          def onPush(): Unit = {
            val data = grab(inlet)
            buffer ++= data
            decodeValue(buffer) match {
              case Some((decoded, remaining)) =>
                remoteData = decoded.some
                buffer = remaining
                if (localDataSent) moveToInitialized(decoded)
              case _ =>
                pull(inlet)
            }
          }

          override def onUpstreamFailure(ex: Throwable): Unit = {
            super.onUpstreamFailure(ex)
            matPromise.failure(ex)
          }
        }
      )
      setHandler(
        outlet,
        new OutHandler {
          def onPull(): Unit =
            if (!localDataSent) {
              push(outlet, encodeValue(localValue))
              remoteData match {
                case Some(n) => moveToInitialized(n)
                case _       => localDataSent = true
              }
            } else {
              pullOnInit = true
            }

          override def onDownstreamFinish(cause: Throwable): Unit = {
            super.onDownstreamFinish(cause)
            matPromise.failure(cause)
          }
        }
      )
    }

    def setInitializedHandlers(remoteData: T, buffer: ByteString, pullOnInit: Boolean): Unit = {
      val subFlow = next(remoteData)
      val subSourceOutlet = new SubSourceOutlet[ByteString]("ValueExchanger.Sub.Out")
      val subSinkInlet = new SubSinkInlet[ByteString]("ValueExchanger.Sub.In")

      val normalOutHandler =
        new OutHandler {
          def onPull(): Unit = pull(inlet)
        }
      if (buffer.nonEmpty)
        subSourceOutlet.setHandler(
          new OutHandler {
            def onPull(): Unit = {
              if (buffer.nonEmpty) subSourceOutlet.push(buffer)
              else pull(inlet)
              subSourceOutlet.setHandler(normalOutHandler)
            }
          }
        )
      else
        subSourceOutlet.setHandler(normalOutHandler)

      subSinkInlet.setHandler(
        new InHandler {
          def onPush(): Unit = push(outlet, subSinkInlet.grab())
        }
      )

      setHandler(
        inlet,
        new InHandler {
          def onPush(): Unit = subSourceOutlet.push(grab(inlet))
        }
      )

      setHandler(
        outlet,
        new OutHandler {
          def onPull(): Unit = subSinkInlet.pull()
        }
      )

      val graph =
        Source.fromGraph(subSourceOutlet.source).viaMat(subFlow)(Keep.right).toMat(subSinkInlet.sink)(Keep.left)
      val mat =
        subFusingMaterializer.materialize(graph)

      matPromise.success(mat)
      if (pullOnInit) subSinkInlet.pull()
    }

    override def preStart(): Unit = {
      super.preStart()
      pull(inlet)
    }

    setUninitializedHandlers()
  }
}
