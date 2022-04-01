package co.topl.networking.p2p

import akka.NotUsed
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.{DelayStrategy, Flow}
import akka.util.ByteString

import scala.concurrent.duration._
import scala.util.Random

object SimulatedGeospatialDelayFlow {

  /**
   * Simulates network delay as estimated between two (lat, lng) coordinates.
   * @param peerACoordinate Coordinate of the local peer
   * @param peerBCoordinate Coordinate of the remote peer
   * @param durationPerKilometer The "cost" (in duration) of each kilometer of distance between the peers
   * @param durationPerByte The "cost" (in duration) of each byte transmitted between the peers
   *                        TODO: A download speed of 8Mbps = 1 megabyte per second = ~1 kilobyte per millisecond = ~1 byte per microsecond
   * @param noise A maximum randomness factor to add to each transmission
   * @return an Akka Flow
   */
  def apply(
    peerACoordinate:      (Double, Double),
    peerBCoordinate:      (Double, Double),
    durationPerKilometer: FiniteDuration = 20.micros,
    durationPerByte:      FiniteDuration = 1.micros,
    noise:                FiniteDuration = 100.milli
  ): Flow[ByteString, ByteString, NotUsed] = {
    val strategy =
      new GeospatialDelayStrategy(peerACoordinate, peerBCoordinate, durationPerKilometer, durationPerByte, noise)
    Flow[ByteString].delayWith(() => strategy, OverflowStrategy.backpressure)
  }
}

private class GeospatialDelayStrategy(
  peerACoordinate:      (Double, Double),
  peerBCoordinate:      (Double, Double),
  durationPerKilometer: FiniteDuration,
  durationPerByte:      FiniteDuration,
  noise:                FiniteDuration
) extends DelayStrategy[ByteString] {
  import GeospatialDelayStrategy._

  private val estimatedDistanceKm =
    distanceKm(peerACoordinate._1, peerACoordinate._2, peerBCoordinate._1, peerBCoordinate._2).toLong

  private val staticDistanceDelay = durationPerKilometer * estimatedDistanceKm

  private val noiseValueF =
    if (noise > Duration.Zero) () => Random.nextLong(noise.toNanos).nanos else () => Duration.Zero

  def nextDelay(elem: ByteString): FiniteDuration =
    noiseValueF() + (durationPerByte * elem.length) + staticDistanceDelay
}

object GeospatialDelayStrategy {

  /**
   * Calculate the distance (in kilometers) between two points using the haversine formula.
   */
  def distanceKm(lat1: Double, lon1: Double, lat2: Double, lon2: Double): Double =
    if ((lat1 == lat2) && (lon1 == lon2)) 0
    else {
      val theta = lon1 - lon2
      val dist =
        Math.sin(Math.toRadians(lat1)) * Math.sin(Math.toRadians(lat2)) +
        Math.cos(Math.toRadians(lat1)) * Math.cos(Math.toRadians(lat2)) * Math.cos(Math.toRadians(theta))
      val dist1 = Math.acos(dist)
      val dist2 = Math.toDegrees(dist1)
      val dist3 = dist2 * 60 * 1.1515
      dist3 * 1.609344
    }

}
