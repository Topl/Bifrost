package co.topl.network.message

import co.topl.modifier.NodeViewModifier.ModifierTypeId
import co.topl.modifier.{ModifierId, NodeViewModifier}
import co.topl.network.message
import co.topl.network.message.Message.MessageCode
import co.topl.network.peer.{PeerFeature, PeerSpec, PeerSpecSerializer}
import co.topl.utils.Extensions._
import co.topl.utils.Logging
import co.topl.utils.codecs.binary.legacy.modifier.ModifierIdSerializer
import co.topl.utils.codecs.binary.legacy.{Reader, Writer}
import co.topl.utils.codecs._

/** Sequence of modifiers to send to the remote peer */
case class ModifiersData(typeId: ModifierTypeId, modifiers: Map[ModifierId, Array[Byte]])

/** Inventory data (a sequence of modifier ids) */
case class InvData(typeId: ModifierTypeId, ids: Seq[ModifierId]) {
  override def toString: String = NodeViewModifier.idsToString(typeId, ids)
}

/** Sequence of PeerSpec containing the declared information about peer */
case class PeersData(peers: Seq[PeerSpec])

/* ----------------------------------------------------------------------------------------------------------------- */
/**
 * The `SyncInfo` message requests an `Inv` message that provides modifier ids
 * required be sender to synchronize his blockchain with the recipient.
 * It allows a peer which has been disconnected or started for the first
 * time to get the data it needs to request the blocks it hasn't seen.
 *
 * Payload of this message should be determined in underlying applications.
 */
class SyncInfoSpec extends MessageSpecV1[BifrostSyncInfo] {

  override val messageCode: MessageCode = SyncInfoSpec.MessageCode
  override val messageName: String = SyncInfoSpec.MessageName

  override def serialize(data: BifrostSyncInfo, w: Writer): Unit = {
    w.putUShort(data.lastBlockIds.size)
    data.lastBlockIds.foreach(id => w.putBytes(ModifierIdSerializer.toBytes(id)))
  }

  override def parse(r: Reader): BifrostSyncInfo = {
    val length = r.getUShort()
    val ids = (1 to length).map(_ => ModifierIdSerializer.parse(r))
    BifrostSyncInfo(ids)
  }
}

object SyncInfoSpec {
  val MessageCode: Byte = 65
  val MessageName: String = "Sync"
}

/* ----------------------------------------------------------------------------------------------------------------- */
/**
 * The `Inv` message (inventory message) transmits one or more inventories of
 * objects known to the transmitting peer.
 * It can be sent unsolicited to announce new transactions or blocks,
 * or it can be sent in reply to a `SyncInfo` message (or application-specific messages like `GetMempool`).
 *
 * @param maxInvObjects maximum inventory objects
 */
class InvSpec(maxInvObjects: Int) extends MessageSpecV1[InvData] {

  override val messageCode: MessageCode = InvSpec.MessageCode
  override val messageName: String = InvSpec.MessageName

  override def serialize(data: InvData, w: Writer): Unit = {
    val typeId = data.typeId
    val elems = data.ids
    require(elems.nonEmpty, "empty inv list")
    require(elems.lengthCompare(maxInvObjects) <= 0, s"more invs than $maxInvObjects in a message")
    w.put(typeId.value)
    w.putUInt(elems.size)
    elems.foreach { id =>
      val bytes = ModifierIdSerializer.toBytes(id)
      assert(bytes.length == NodeViewModifier.modifierIdSize)
      w.putBytes(bytes)
    }
  }

  override def parse(r: Reader): InvData = {
    val typeId = ModifierTypeId(r.getByte())
    val count = r.getUInt().toIntExact
    require(count > 0, "empty inv list")
    require(count <= maxInvObjects, s"$count elements in a message while limit is $maxInvObjects")
    val elems = (0 until count).map(_ => ModifierIdSerializer.parse(r))

    InvData(typeId, elems)
  }

}

object InvSpec {
  val MessageCode: Byte = 55
  val MessageName: String = "Inv"
}

/** ------------------------------------------------------------------------------------------------------------------ */
/**
 * The `RequestModifier` message requests one or more modifiers from another node.
 * The objects are requested by an inventory, which the requesting node
 * typically received previously by way of an `Inv` message.
 *
 * This message cannot be used to request arbitrary data, such as historic transactions no
 * longer in the memory pool. Full nodes may not even be able to provide older blocks if
 * they’ve pruned old transactions from their block database.
 * For this reason, the `RequestModifier` message should usually only be used to request
 * data from a node which previously advertised it had that data by sending an `Inv` message.
 *
 * @param maxInvObjects maximum inventory objects
 */
class RequestModifierSpec(maxInvObjects: Int) extends MessageSpecV1[InvData] {

  override val messageCode: MessageCode = RequestModifierSpec.MessageCode
  override val messageName: String = RequestModifierSpec.MessageName

  private val invSpec = new InvSpec(maxInvObjects)

  override def serialize(data: InvData, w: Writer): Unit =
    invSpec.serialize(data, w)

  override def parse(r: Reader): InvData =
    invSpec.parse(r)
}

object RequestModifierSpec {
  val MessageCode: MessageCode = 22: Byte
  val MessageName: String = "RequestModifier"
}

/** ------------------------------------------------------------------------------------------------------------------ */
/**
 * The `Modifier` message is a reply to a `RequestModifier` message which requested these modifiers.
 *
 * @param maxMessageSize maximum income package size (bytes), maxPacketSize in configs
 */
class ModifiersSpec(maxMessageSize: Int) extends MessageSpecV1[ModifiersData] with Logging {

  override val messageCode: MessageCode = ModifiersSpec.MessageCode
  override val messageName: String = ModifiersSpec.MessageName

  override def serialize(data: ModifiersData, w: Writer): Unit = {

    val typeId = data.typeId
    val modifiers = data.modifiers
    require(modifiers.nonEmpty, "empty modifiers list")

    val (msgCount, msgSize) = modifiers.foldLeft((0, 5)) { case ((c, s), (_, modifier)) =>
      val size = s + NodeViewModifier.modifierIdSize + 4 + modifier.length
      val count = if (size <= maxMessageSize) c + 1 else c
      count -> size
    }

    val start = w.length()
    w.put(typeId.value)
    w.putUInt(msgCount)

    modifiers.take(msgCount).foreach { case (id, modifier) =>
      w.putBytes(ModifierIdSerializer.toBytes(id))
      w.putUInt(modifier.length)
      w.putBytes(modifier)
    }

    if (msgSize > maxMessageSize) {
      log.warn(
        s"Message with modifiers ${modifiers.keySet} have size $msgSize exceeding limit $maxMessageSize." +
        s" Sending ${w.length() - start} bytes instead"
      )
    }
  }

  override def parse(r: Reader): ModifiersData = {
    val typeId = ModifierTypeId(r.getByte())
    val count = r.getUInt().toIntExact
    val seq = (0 until count).map { _ =>
      val id = ModifierIdSerializer.parse(r)
      val objBytesCnt = r.getUInt().toIntExact
      val obj = r.getBytes(objBytesCnt)
      id -> obj
    }
    ModifiersData(typeId, seq.toMap)
  }
}

object ModifiersSpec {
  val MessageCode: MessageCode = 33: Byte
  val MessageName: String = "Modifier"
}

/** ------------------------------------------------------------------------------------------------------------------ */
/**
 * The `GetPeer` message requests an `Peers` message from the receiving node,
 * preferably one with lots of `PeerSpec` of other receiving nodes.
 * The transmitting node can use those `PeerSpec` addresses to quickly update
 * its database of available nodes rather than waiting for unsolicited `Peers`
 * messages to arrive over time.
 */
class GetPeersSpec extends MessageSpecV1[Unit] {

  override val messageCode: MessageCode = GetPeersSpec.MessageCode
  override val messageName: String = GetPeersSpec.MessageName

  override def serialize(obj: Unit, w: Writer): Unit = {}

  override def parse(r: Reader): Unit =
    require(r.remaining == 0, "Non-empty data for GetPeers")
}

object GetPeersSpec {
  val MessageCode: MessageCode = 1: Byte
  val MessageName: String = "GetPeers message"
}

/** ------------------------------------------------------------------------------------------------------------------ */
/**
 * The `Peers` message is a reply to a `GetPeer` message and relays connection information about peers
 * on the network.
 *
 * @param featureSerializers searializer for feature
 * @param peersLimit maximum number of PeerSpec objects in one Peers message
 */
class PeersSpec(featureSerializers: PeerFeature.Serializers, peersLimit: Int) extends MessageSpecV1[PeersData] {

  override val messageCode: Message.MessageCode = PeersSpec.MessageCode
  override val messageName: String = PeersSpec.MessageName

  private val peerSpecSerializer = new PeerSpecSerializer(featureSerializers)

  override def serialize(data: PeersData, w: Writer): Unit = {
    w.putUInt(data.peers.size)
    data.peers.foreach(p => peerSpecSerializer.serialize(p, w))
  }

  override def parse(r: Reader): PeersData = {
    val length = r.getUInt().toIntExact
    require(length <= peersLimit, s"Too many peers. $length exceeds limit $peersLimit")
    val peers = (0 until length).map { _ =>
      peerSpecSerializer.parse(r)
    }
    PeersData(peers)
  }
}

object PeersSpec {
  val MaxPeersInMessage: Int = 100
  val MessageCode: MessageCode = 2: Byte
  val MessageName: String = "Peers message"
}

/** ------------------------------------------------------------------------------------------------------------------ */
/**
 * The `Handshake` message provides information about the transmitting node
 * to the receiving node at the beginning of a connection. Until both peers
 * have exchanged `Handshake` messages, no other messages will be accepted.
 */
class HandshakeSpec(featureSerializers: PeerFeature.Serializers, sizeLimit: Int) extends MessageSpecV1[Handshake] {

  private val peersDataSerializer = new PeerSpecSerializer(featureSerializers)

  override val messageCode: MessageCode = HandshakeSpec.MessageCode
  override val messageName: String = HandshakeSpec.MessageName

  override def serialize(obj: Handshake, w: Writer): Unit = {
    w.putULong(obj.time)
    peersDataSerializer.serialize(obj.peerSpec, w)
  }

  override def parse(r: Reader): Handshake = {
    require(r.remaining <= sizeLimit, s"Too big handshake. Size ${r.remaining} exceeds $sizeLimit limit")
    val t = r.getULong()
    val data = peersDataSerializer.parse(r)
    message.Handshake(data, t)
  }
}

object HandshakeSpec {
  val MessageCode: MessageCode = 75: Byte
  val MessageName: String = "Handshake"
}
