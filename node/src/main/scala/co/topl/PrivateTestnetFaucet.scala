package co.topl

import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpMethods
import cats.data.NonEmptyChain
import cats.implicits._
import co.topl.akkahttprpc.RequestModifier
import co.topl.akkahttprpc.implicits.client.rpcToClient
import co.topl.attestation.implicits._
import co.topl.attestation.keyManagement.{KeyRing, KeyfileCurve25519, KeyfileCurve25519Companion, PrivateKeyCurve25519}
import co.topl.attestation.{Address, PublicKeyPropositionCurve25519}
import co.topl.modifier.transaction.builder.BoxSelectionAlgorithms
import co.topl.rpc.ToplRpc
import co.topl.rpc.ToplRpc.Transaction.RawPolyTransfer
import co.topl.rpc.implicits.client._
import co.topl.utils.IdiomaticScalaTransition.implicits._
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.StringDataTypes.Base58Data
import co.topl.utils.{Logging, NetworkType}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Success

/**
 * A simple application which serves as a "faucet" for a private blockchain testnet.  A private testnet is launched with
 * funds available at pre-seeded addresses.  This utility creates a Transaction which spends those funds and moves a
 * requested quantity to a requested address. It then broadcasts the transaction to a requested node (via json-rpc).
 */
object PrivateTestnetFaucet extends Logging {
  implicit val networkPrefix: NetworkPrefix = NetworkType.PrivateTestnet.netPrefix

  def run(args: List[String]): Unit = {
    val recipientAddress = Base58Data.unsafe(args(0)).decodeAddress.getOrThrow()
    val quantity = args(1).toLong
    val nodeUri = args.lift(2).getOrElse("http://bifrost:9085")
    val seed = args.lift(3).getOrElse("test")

    implicit val requestModifier: RequestModifier = RequestModifier(
      _.withMethod(HttpMethods.POST)
        .withUri(nodeUri)
    )

    log.info(s"Sending faucet transaction to recipient=$recipientAddress with quantity=$quantity")

    implicit val system: ActorSystem = ActorSystem("RibnTestingTransaction")
    import system.dispatcher

    implicit val keyfileCurve25519Companion: KeyfileCurve25519Companion.type =
      KeyfileCurve25519Companion

    val keyRing = KeyRing.empty[PrivateKeyCurve25519, KeyfileCurve25519]()
    val Success(keys) = keyRing.generateNewKeyPairs(10, Some(seed))

    val key = keys.head

    val params: RawPolyTransfer.Params = ToplRpc.Transaction.RawPolyTransfer.Params(
      propositionType = PublicKeyPropositionCurve25519.typeString,
      sender = NonEmptyChain(key.publicImage.address),
      recipients = NonEmptyChain((recipientAddress, args(1).toLong)),
      fee = 0,
      changeAddress = key.publicImage.address,
      data = None,
      boxSelectionAlgorithm = BoxSelectionAlgorithms.All
    )
    val broadcastFuture = for {
      rawTx <- ToplRpc.Transaction.RawPolyTransfer.rpc(params).map(_.rawTx)
      signTx = {
        val msg2Sign = rawTx.messageToSign
        val signFunc = (addr: Address) => keyRing.generateAttestation(addr)(msg2Sign)
        val signatures = keyRing.addresses.map(signFunc).reduce(_ ++ _)
        rawTx.copy(attestation = signatures)
      }
      broadcastTx <- ToplRpc.Transaction.BroadcastTx.rpc(ToplRpc.Transaction.BroadcastTx.Params(signTx))
    } yield broadcastTx

    val broadcastResult = Await.result(broadcastFuture.value, 20.seconds)

    log.info(broadcastResult.toString)

    system.terminate()
  }
}
