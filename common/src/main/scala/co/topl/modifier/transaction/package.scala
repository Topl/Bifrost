package co.topl.modifier

import co.topl.attestation.Address
import co.topl.crypto.hash.blake2b256
import co.topl.modifier.box.{AssetBox, Box, BoxId, TokenValueHolder}
import co.topl.utils.Int128
import com.google.common.primitives.{Ints, Longs}

package object transaction {

  def boxFunds(fromBoxes: List[(Address, Box[TokenValueHolder])]): Int128 =
    fromBoxes.map(_._2.value.quantity).sum

  def boxNonceGenerator(
    txIdPrefix: Byte,
    inputBoxes: List[BoxId],
    fee:        Int128,
    timestamp:  Long
  ): Int => Box.Nonce = {
    val boxIdsToOpenAccumulator = inputBoxes.foldLeft(Array[Byte]())((acc, x) => acc ++ x.hash.value)
    val timestampBytes = Longs.toByteArray(timestamp)
    val feeBytes = fee.toByteArray

    val inputBytes =
      Array(txIdPrefix) ++ boxIdsToOpenAccumulator ++ timestampBytes ++ feeBytes

    (index: Int) => {
      val digest = blake2b256.hash(inputBytes ++ Ints.toByteArray(index))
      Transaction.nonceFromDigest(digest)
    }
  }
}
