package bifrost.network

import java.time.Instant
import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import io.iohk.iodb.ByteArrayWrapper
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58
import serializer.ProducerProposal

import scala.collection.concurrent.TrieMap
import scala.concurrent.duration.Duration
import scala.util.{Success, Try}


case class PeerMessageManager(var messages: TrieMap[ByteArrayWrapper, ProducerProposal]) extends ScorexLogging {

  setHeartbeatCheck()

  private val heartbeatInterval = 10000L

  private def setHeartbeatCheck(): Unit = {
    val actorSystem = ActorSystem()
    val scheduler = actorSystem.scheduler
    val task = new Runnable {
      def run(): Unit = cleanup()
    }
    implicit val executor = actorSystem.dispatcher

    val timeoutPeriod = messages.values.map(_.timestamp).foldLeft(heartbeatInterval)((a, b) => {
      val delta = heartbeatInterval - (Instant.now.toEpochMilli - b)
      if (delta < a) delta
      else a
    })

    scheduler.scheduleOnce(Duration(timeoutPeriod, TimeUnit.MILLISECONDS),task)
  }

  private def key(p: ProducerProposal): ByteArrayWrapper = ByteArrayWrapper(p.producer.toByteArray ++ FastCryptographicHash(p.details.toByteArray))

  //getters
  def getById(id: ByteArrayWrapper): Option[ProducerProposal] = messages.get(id)

  def contains(id: ByteArrayWrapper): Boolean = messages.contains(id)

  def getAll(ids: Seq[ByteArrayWrapper]): Seq[ProducerProposal] = ids.flatMap(getById)

  private def cleanup(): Unit = {
    messages = messages.filter {
      case (_, value) => Instant.now.toEpochMilli - value.timestamp < heartbeatInterval
    }
    setHeartbeatCheck()
  }

  //modifiers
  def put(p: ProducerProposal): Try[PeerMessageManager] = Try {
    messages.put(key(p), p)
    this
  }

  def put(proposals: Iterable[ProducerProposal]): Try[PeerMessageManager] = Success(putWithoutCheck(proposals))

  def putWithoutCheck(proposals: Iterable[ProducerProposal]): PeerMessageManager = {
    proposals.foreach(p => messages.put(key(p), p))
    this
  }

  def remove(p: ProducerProposal): PeerMessageManager = {
    messages.remove(key(p))
    this
  }

  def take(limit: Int): Iterable[ProducerProposal] =
    messages.toSeq.sortBy(m => Base58.encode(m._1.data)).map(_._2).take(limit)

  def filter(condition: (ProducerProposal) => Boolean): PeerMessageManager = {
    messages.retain { (k, v) => condition(v) }
    this
  }

  def size: Int = messages.size
}


object PeerMessageManager {
  lazy val emptyManager: PeerMessageManager = PeerMessageManager(TrieMap())
}