//package bifrost.transaction.serialization
//
//import bifrost.serialization.Serializer
//import bifrost.transaction.bifrostTransaction.{ArbitTransfer, NewArbitTransfer}
//import com.google.common.primitives.Ints
//
//import scala.util.Try
//
//object NewArbitTransferCompanion extends Serializer[NewArbitTransfer] with TransferSerializer {
//
//  override def toBytes(ac: NewArbitTransfer): Array[Byte] = {
//    //TransferTransactionCompanion.prefixBytes ++ toChildBytes(ac)
//    Array(0)
//  }
//
////  def toChildBytes(ac: NewArbitTransfer): Array[Byte] = {
////    transferToBytes(ac, "ArbitTransfer") ++
////      ac.data.getBytes++
////      Ints.toByteArray(ac.data.getBytes.length)
////  }
//
//  override def parseBytes(bytes: Array[Byte]): Try[NewArbitTransfer] = Try {
////    val params = parametersParseBytes(bytes)
////    val dataLen: Int = Ints.fromByteArray(bytes.slice(bytes.length - Ints.BYTES, bytes.length))
////    val data: String = new String(
////      bytes.slice(bytes.length - Ints.BYTES - dataLen, bytes.length - Ints.BYTES)
////    )
////    NewArbitTransfer(params._1, params._2, params._3, params._4, params._5, data)
//    NewArbitTransfer(IndexedSeq(), IndexedSeq(), IndexedSeq(), 0L, 0L, "")
//  }
//}