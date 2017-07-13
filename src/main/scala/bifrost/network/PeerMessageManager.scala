package bifrost.network

import io.iohk.iodb.ByteArrayWrapper
import scorex.core.NodeViewModifier.ModifierId
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58
import serializer.ProducerProposal

import scala.collection.concurrent.TrieMap
import scala.util.{Success, Try}


case class PeerMessageManager(messages: TrieMap[ByteArrayWrapper, ProducerProposal]) extends ScorexLogging {

  private def key(id: Array[Byte]): ByteArrayWrapper = ByteArrayWrapper(id)

  //getters
  def getById(id: ModifierId): Option[ProducerProposal] = messages.get(key(id))

  def contains(id: ModifierId): Boolean = messages.contains(key(id))

  def getAll(ids: Seq[ModifierId]): Seq[ProducerProposal] = ids.flatMap(getById)

  //modifiers
  def put(p: ProducerProposal): Try[PeerMessageManager] = Try {
    val id = p.producer.toByteArray ++ FastCryptographicHash(p.details.toByteArray)
    messages.put(key(id), p)
    this
  }

  def put(proposals: Iterable[ProducerProposal]): Try[PeerMessageManager] = Success(putWithoutCheck(proposals))

  def putWithoutCheck(proposals: Iterable[ProducerProposal]): PeerMessageManager = {
    proposals.foreach(p => {
      val id = p.producer.toByteArray ++ FastCryptographicHash(p.details.toByteArray)
      messages.put(key(id), p)
    })
    this
  }

  def remove(p: ProducerProposal): PeerMessageManager = {
    val id = p.producer.toByteArray ++ FastCryptographicHash(p.details.toByteArray)
    messages.remove(key(id))
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