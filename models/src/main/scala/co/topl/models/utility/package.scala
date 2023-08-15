package co.topl.models

import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.node.models.{BlockBody, FullBlockBody}
import com.google.protobuf.ByteString
import scodec.bits.ByteVector

import scala.language.implicitConversions

package object utility {

  implicit def byteStringToByteVector(byteString: ByteString): ByteVector =
    ByteVector(byteString.asReadOnlyByteBuffer())

  implicit def byteVectorToByteString(byteVector: ByteVector): ByteString =
    ByteString.copyFrom(byteVector.toByteBuffer)

  implicit def byteStringToByteArray(byteString: ByteString): Array[Byte] = byteString.toByteArray

  implicit def byteArrayToByteString(byteArray: Array[Byte]): ByteString = ByteString.copyFrom(byteArray)

  implicit class BlockBodyOps(val body: BlockBody) extends AnyVal {

    /**
     * Return all Transaction IDs in this block, including the reward transaction ID if provided
     */
    def allTransactionIds: Seq[TransactionId] = body.transactionIds ++ body.rewardTransactionId

  }

  implicit class FullBlockBodyOps(val body: FullBlockBody) extends AnyVal {

    /**
     * Return all Transactions in this block, including the reward transaction if provided
     * @return
     */
    def allTransactions: Seq[IoTransaction] = body.transactions ++ body.rewardTransaction

  }

}
