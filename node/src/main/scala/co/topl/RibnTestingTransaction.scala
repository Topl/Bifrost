package co.topl

import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpMethods
import cats.data.NonEmptyChain
import co.topl.attestation.{Address, PublicKeyPropositionCurve25519}
import co.topl.attestation.keyManagement.{KeyRing, KeyfileCurve25519, KeyfileCurve25519Companion, PrivateKeyCurve25519}
import co.topl.modifier.transaction.builder.BoxSelectionAlgorithms
import co.topl.rpc.ToplRpc
import co.topl.rpc.ToplRpc.Transaction.RawPolyTransfer
import co.topl.utils.NetworkType
import co.topl.rpc.implicits.client._
import co.topl.utils.StringDataTypes.Base58Data
import co.topl.akkahttprpc.implicits.client.rpcToClient

import scala.util.Success
import co.topl.attestation.implicits._
import co.topl.utils.IdiomaticScalaTransition.implicits._
import cats.implicits._
import co.topl.akkahttprpc.RequestModifier

import scala.concurrent.duration._
import scala.concurrent.Await

object RibnTestingTransaction {
  implicit val networkPrefix = NetworkType.PrivateTestnet.netPrefix

  def run(args: List[String]): Unit = {
    val recipientAddress = Base58Data.unsafe(args(0)).decodeAddress.getOrThrow()
    val recipientAmount = args(1).toLong
    val nodeUri = args.lift(2).getOrElse("http://bifrost:9085")

    implicit val requestModifier: RequestModifier = RequestModifier(
      _.withMethod(HttpMethods.POST)
        .withUri(nodeUri)
    )

    println(s"Sending seed transaction to $recipientAddress with amount $recipientAmount")

    implicit val system: ActorSystem = ActorSystem("RibnTestingTransaction")
    import system.dispatcher

    implicit val keyfileCurve25519Companion: KeyfileCurve25519Companion.type =
      KeyfileCurve25519Companion

    val keyRing = KeyRing.empty[PrivateKeyCurve25519, KeyfileCurve25519]()
    val Success(keys) = keyRing.generateNewKeyPairs(10, Some("test"))

    val key = keys.head

    val params: RawPolyTransfer.Params = ToplRpc.Transaction.RawPolyTransfer.Params(
      propositionType =
        PublicKeyPropositionCurve25519.typeString, // required fixed string for now, exciting possibilities in the future!
      sender =
        NonEmptyChain(key.publicImage.address), // Set of addresses whose state you want to use for the transaction
      recipients = NonEmptyChain(
        (recipientAddress, args(1).toLong)
      ), // Chain of (Recipients, Value) tuples that represent the output boxes
      fee = 0, // fee to be paid to the network for the transaction (unit is nanoPoly)
      changeAddress = key.publicImage.address, // who will get ALL the change from the transaction?
      data = None,
      boxSelectionAlgorithm = BoxSelectionAlgorithms.All
    )
    val response = for {
      rawTx <- ToplRpc.Transaction.RawPolyTransfer.rpc(params).map(_.rawTx)
      signTx = {
        val msg2Sign = rawTx.messageToSign
        val signFunc = (addr: Address) => keyRing.generateAttestation(addr)(msg2Sign)
        val signatures = keyRing.addresses.map(signFunc).reduce(_ ++ _)
        rawTx.copy(attestation = signatures)
      }
      broadcastTx <- ToplRpc.Transaction.BroadcastTx.rpc(ToplRpc.Transaction.BroadcastTx.Params(signTx))
    } yield broadcastTx

    println(Await.result(response.value, 20.seconds))

    system.terminate()
  }
}
