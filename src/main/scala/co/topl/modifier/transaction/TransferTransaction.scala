package co.topl.modifier.transaction

import co.topl.attestation.Address
import co.topl.attestation.proof.Proof
import co.topl.attestation.proposition.Proposition
import co.topl.nodeView.state.box.{Box, BoxId, TokenBox}
import com.google.common.primitives.Longs
import scorex.crypto.hash.{Blake2b256, Digest32}

abstract class TransferTransaction[P <: Proposition, PR <: Proof[P]] (val from       : IndexedSeq[(Address, Box.Nonce)],
                                                                      val to         : IndexedSeq[(Address, TokenBox.Value)],
                                                                      val attestation: Map[P, PR],
                                                                      val fee        : Long,
                                                                      val timestamp  : Long,
                                                                      val data       : String
                                                                     ) extends Transaction[TokenBox.Value, P, PR] {

  override val newBoxes: Traversable[TokenBox]

  lazy val boxIdsToOpen: IndexedSeq[BoxId] = from.map { case (addr, nonce) =>
    BoxId.idFromEviNonce(addr.evidence, nonce)
  }

  // todo: JAA - why not include the from bytes here? I assume this was intentional because the name is "no nonces" so was that a problem?
  lazy val hashNoNonces: Digest32 = Blake2b256(
    to.flatMap(_._1.bytes).toArray ++
      Longs.toByteArray(timestamp) ++
      Longs.toByteArray(fee) ++
      data.getBytes
  )

  //YT NOTE - removed timestamp and unlockers since that will be updated after signatures are received
  def messageToSign: Array[Byte] =
    to.flatMap(_._1.pubKeyBytes).toArray ++
      newBoxes.foldLeft(Array[Byte]())((acc, x) => acc ++ x.bytes)

  Longs.toByteArray(fee) ++
    boxIdsToOpen.foldLeft(Array[Byte]())(_ ++ _.hashBytes) ++
    data.getBytes
}



