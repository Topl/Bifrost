package co.topl.networking.p2p

import akka.NotUsed
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.{DelayStrategy, Flow}
import akka.util.ByteString

import scala.concurrent.duration._
import scala.util.Random

object SimulatedGeospatialDelayFlow {

  def apply(
    peerACoordinate: (Double, Double),
    peerBCoordinate: (Double, Double)
  ): Flow[ByteString, ByteString, NotUsed] = {
    val strategy = new GeospatialDelayStrategy(peerACoordinate, peerBCoordinate)
    Flow[ByteString].delayWith(() => strategy, OverflowStrategy.backpressure)
  }
}

private class GeospatialDelayStrategy(peerACoordinate: (Double, Double), peerBCoordinate: (Double, Double))
    extends DelayStrategy[ByteString] {
  import GeospatialDelayStrategy._

  private val estimatedDistanceKm =
    distanceKm(peerACoordinate._1, peerACoordinate._2, peerBCoordinate._1, peerBCoordinate._2).toLong

  private val staticDistanceDelay = DurationPerKilometer * estimatedDistanceKm

  def nextDelay(elem: ByteString): FiniteDuration =
    Random.nextLong(Noise.toNanos).nanos + (DurationPerByte * elem.length) + staticDistanceDelay
}

object GeospatialDelayStrategy {
  val DurationPerKilometer: FiniteDuration = 20.micros
  // TODO: A download speed of 8Mbps = 1MBps = ~1 kilobyte per millisecond = ~1 byte per microsecond
  val DurationPerByte: FiniteDuration = 1.micros
  val Noise: FiniteDuration = 100.milli

  def distanceKm(lat1: Double, lon1: Double, lat2: Double, lon2: Double): Double =
    if ((lat1 == lat2) && (lon1 == lon2)) 0
    else {
      val theta = lon1 - lon2
      val dist =
        Math.sin(Math.toRadians(lat1)) * Math.sin(Math.toRadians(lat2))
      +Math.cos(Math.toRadians(lat1)) * Math.cos(Math.toRadians(lat2)) * Math.cos(Math.toRadians(theta))
      val dist1 = Math.acos(dist)
      val dist2 = Math.toDegrees(dist1)
      val dist3 = dist2 * 60 * 1.1515
      dist3 * 1.609344
    }

}
