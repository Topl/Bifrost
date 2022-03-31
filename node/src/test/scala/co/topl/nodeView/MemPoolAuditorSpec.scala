package co.topl.nodeView

import akka.actor.testkit.typed.FishingOutcome
import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.adapter._
import co.topl.attestation.{PublicKeyPropositionCurve25519, SignatureCurve25519}
import co.topl.crypto.signatures.Curve25519
import co.topl.modifier.transaction.PolyTransfer
import co.topl.nodeView.history.MockImmutableBlockHistory
import co.topl.nodeView.mempool.{MemPool, UnconfirmedTx}
import co.topl.nodeView.state.MockStateReader
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.TimeProvider.Time
import co.topl.utils._
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.collection.concurrent.TrieMap
import scala.collection.immutable.ListMap
import scala.concurrent.duration._

class MemPoolAuditorSpec
    extends ScalaTestWithActorTestKit
    with AnyFlatSpecLike
    with TestSettings
    with ScalaCheckDrivenPropertyChecks
    with CommonGenerators {

  behavior of "MemPoolAuditor"

  import org.scalacheck.Gen
  Gen.chooseNum(1, 400)

  it should "Tell nodeViewHolder to eliminate transactions that stayed in mem-pool longer than mempoolTimeout" in {
    forAll(keyCurve25519Gen, addressGen, polyBoxGen, positiveLongGen, positiveMediumIntGen) {
      (sender, recipient, polyBox, timestamp, timeout) =>
        implicit val networkPrefix: NetworkPrefix = 9.toByte

        val unsignedTransfer =
          PolyTransfer[PublicKeyPropositionCurve25519](
            IndexedSeq(sender._2.address -> polyBox.nonce),
            IndexedSeq(recipient         -> polyBox.value),
            ListMap.empty,
            0,
            timestamp,
            None,
            minting = false
          )

        val signedTransfer =
          unsignedTransfer
            .copy(attestation =
              ListMap[PublicKeyPropositionCurve25519, SignatureCurve25519](
                sender._2 -> SignatureCurve25519(Curve25519.sign(sender._1.privateKey, unsignedTransfer.messageToSign))
              )
            )

        val readableNodeView =
          ReadableNodeView(
            MockImmutableBlockHistory(List.empty),
            MockStateReader(Map(sender._2.address -> List(polyBox))),
            MemPool(TrieMap(signedTransfer.id -> UnconfirmedTx(signedTransfer, timestamp)))
          )

        val nodeViewHolderBehavior = MockNodeViewHolderBehavior.readOnly(readableNodeView)

        val nodeViewProbe = createTestProbe[NodeViewHolder.ReceivableMessage]()

        val nodeViewHolder = spawn(Behaviors.monitor(nodeViewProbe.ref, nodeViewHolderBehavior))

        implicit val timeProvider: TimeProvider = new TimeProvider {
          override def time: Time = timestamp + timeout + 1
        }

        val appSettings =
          TestSettings.defaultSettings.copy(application =
            TestSettings.defaultSettings.application.copy(mempoolTimeout = timeout.millis)
          )

        val underTest = spawn(MemPoolAuditor(nodeViewHolder, spawn(Behaviors.ignore).toClassic, appSettings))

        underTest.tell(MemPoolAuditor.ReceivableMessages.RunCleanup)

        // search for an eliminate transactions message
        val nodeViewHolderMessages =
          nodeViewProbe.fishForMessage(5.seconds) { // 5 seconds might be too long
            case NodeViewHolder.ReceivableMessages.EliminateTransactions(txs)
                if txs.toList.contains(signedTransfer.id) =>
              FishingOutcome.Complete
            case _ => FishingOutcome.ContinueAndIgnore
          }

        nodeViewHolderMessages should contain(
          NodeViewHolder.ReceivableMessages.EliminateTransactions(List(signedTransfer.id))
        )
    }
  }

  it should "Invalidate transactions with invalid input boxes after state changes" in {
    forAll(keyCurve25519Gen, addressGen, polyBoxGen, positiveLongGen, positiveMediumIntGen) {
      (sender, recipient, polyBox, timestamp, timeout) =>
        implicit val networkPrefix: NetworkPrefix = 9.toByte

        val unsignedTransfer =
          PolyTransfer[PublicKeyPropositionCurve25519](
            IndexedSeq(sender._2.address -> polyBox.nonce),
            IndexedSeq(recipient         -> polyBox.value),
            ListMap.empty,
            0,
            timestamp,
            None,
            minting = false
          )

        val signedTransfer =
          unsignedTransfer
            .copy(attestation =
              ListMap[PublicKeyPropositionCurve25519, SignatureCurve25519](
                sender._2 -> SignatureCurve25519(Curve25519.sign(sender._1.privateKey, unsignedTransfer.messageToSign))
              )
            )

        val readableNodeView =
          ReadableNodeView(
            MockImmutableBlockHistory(List.empty),
            MockStateReader(Map.empty), // no state boxes available
            MemPool(TrieMap.empty)
          )

        val nodeViewHolderBehavior = MockNodeViewHolderBehavior.readOnly(readableNodeView)

        val nodeViewProbe = createTestProbe[NodeViewHolder.ReceivableMessage]()

        val nodeViewHolder = spawn(Behaviors.monitor(nodeViewProbe.ref, nodeViewHolderBehavior))

        implicit val timeProvider: TimeProvider = new TimeProvider {
          override def time: Time = timestamp + timeout + 1
        }

        val appSettings =
          TestSettings.defaultSettings.copy(application =
            TestSettings.defaultSettings.application.copy(mempoolTimeout = timeout.millis)
          )

        val underTest = spawn(MemPoolAuditor(nodeViewHolder, spawn(Behaviors.ignore).toClassic, appSettings))

        underTest.tell(MemPoolAuditor.ReceivableMessages.RunCleanup)

        // search for an eliminate transactions message
        val nodeViewHolderMessages =
          nodeViewProbe.fishForMessage(1.second) {
            case NodeViewHolder.ReceivableMessages.EliminateTransactions(txs)
                if txs.toList.contains(signedTransfer.id) =>
              FishingOutcome.Complete
            case _ => FishingOutcome.ContinueAndIgnore
          }

        nodeViewHolderMessages should contain(
          NodeViewHolder.ReceivableMessages.EliminateTransactions(List(signedTransfer.id))
        )
    }
  }
}
