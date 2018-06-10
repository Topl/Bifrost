package bifrost.network

import java.time.Instant
import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import io.iohk.iodb.ByteArrayWrapper
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58
import serializer.PeerMessage

import scala.collection.concurrent.TrieMap
import scala.concurrent.duration.Duration
import scala.util.{Success, Try}


case class PeerMessageManager(var messages: TrieMap[ByteArrayWrapper, PeerMessage]) extends ScorexLogging {

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
      if (delta < a) delta else a
    })

    scheduler.scheduleOnce(Duration(timeoutPeriod, TimeUnit.MILLISECONDS), task)
  }

  private def key(p: PeerMessage): ByteArrayWrapper = ByteArrayWrapper(p.messageBytes.toByteArray)

  //getters
  def getById(id: ByteArrayWrapper): Option[PeerMessage] = messages.get(id)

  def contains(id: ByteArrayWrapper): Boolean = messages.contains(id)

  def getAll(ids: Seq[ByteArrayWrapper]): Seq[PeerMessage] = ids.flatMap(getById)

  private def cleanup(): Unit = {
    messages = messages.filter {
      case (_, value) => Instant.now.toEpochMilli - value.timestamp < heartbeatInterval
    }
    setHeartbeatCheck()
  }

  //modifiers
  def put(p: PeerMessage): Try[PeerMessageManager] = Try {
    messages.put(key(p), p)
    this
  }

  def put(proposals: Iterable[PeerMessage]): Try[PeerMessageManager] = Success(putWithoutCheck(proposals))

  def putWithoutCheck(proposals: Iterable[PeerMessage]): PeerMessageManager = {
    proposals.foreach(p => messages.put(key(p), p))
    this
  }

  def remove(p: PeerMessage): PeerMessageManager = {
    messages.remove(key(p))
    this
  }

  def take(limit: Int): Iterable[PeerMessage] =
    messages.toSeq.sortBy(m => Base58.encode(m._1.data)).map(_._2).take(limit)

  def filter(condition: (PeerMessage) => Boolean): PeerMessageManager = {
    messages.retain { (k, v) => condition(v) }
    this
  }

  def size: Int = messages.size
}


object PeerMessageManager {
  lazy val emptyManager: PeerMessageManager = PeerMessageManager(TrieMap())
}