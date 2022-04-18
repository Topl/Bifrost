package co.topl.nodeView

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.adapter._
import co.topl.attestation.{PublicKeyPropositionCurve25519, SignatureCurve25519}
import co.topl.crypto.signatures.Curve25519
import co.topl.modifier.box.PolyBox
import co.topl.modifier.transaction.PolyTransfer
import co.topl.nodeView.history.MockImmutableBlockHistory
import co.topl.nodeView.mempool.{MemPool, UnconfirmedTx}
import co.topl.nodeView.state.MockStateReader
import co.topl.utils.TimeProvider.Time
import co.topl.utils._
import org.scalacheck.Gen
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

  it should "tell nodeViewHolder to eliminate transactions that stayed in mem-pool longer than mempoolTimeout" in {
    forAll(
      keyCurve25519Gen,
      addressGen,
      Gen.zip(Gen.posNum[Long], simpleValueGen),
      positiveLongGen,
      positiveMediumIntGen
    ) { (sender, recipient, polyBoxValues, timestamp, timeout) =>
      val polyBox = PolyBox(sender._2.address.evidence, polyBoxValues._1, polyBoxValues._2)

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

      implicit val timeProvider: TimeProvider = new TimeProvider {
        override def time: Time = timestamp + timeout + 1
      }

      val readableNodeView =
        ReadableNodeView(
          MockImmutableBlockHistory(List.empty),
          MockStateReader(Map(sender._2.address -> List(polyBox))),
          MemPool(TrieMap(signedTransfer.id -> UnconfirmedTx(signedTransfer, timestamp)))
        )

      // set up a mock node view holder
      val nodeViewHolderBehavior = MockNodeViewHolderBehavior.readOnly(readableNodeView)
      val nodeViewProbe = createTestProbe[NodeViewHolder.ReceivableMessage]()
      val nodeViewHolder = spawn(Behaviors.monitor(nodeViewProbe.ref, nodeViewHolderBehavior))

      val appSettings =
        TestSettings.defaultSettings.copy(application =
          TestSettings.defaultSettings.application.copy(mempoolTimeout = timeout.millis)
        )

      val underTest = spawn(MemPoolAuditor(nodeViewHolder, spawn(Behaviors.ignore).toClassic, appSettings))

      underTest.tell(MemPoolAuditor.ReceivableMessages.RunCleanup)

      nodeViewProbe.receiveMessage()

      nodeViewProbe.expectMessage(NodeViewHolder.ReceivableMessages.EliminateTransactions(List(signedTransfer.id)))
    }
  }

  it should "invalidate transactions with invalid input boxes after state changes" in {
    forAll(
      keyCurve25519Gen,
      addressGen,
      Gen.zip(Gen.posNum[Long], simpleValueGen),
      positiveLongGen,
      positiveMediumIntGen
    ) { (sender, recipient, polyBoxValues, timestamp, timeout) =>
      val polyBox = PolyBox(sender._2.address.evidence, polyBoxValues._1, polyBoxValues._2)

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

      nodeViewProbe.receiveMessage()

      nodeViewProbe.expectMessage(
        NodeViewHolder.ReceivableMessages.EliminateTransactions(List(signedTransfer.id))
      )
    }
  }

  it should "eliminate transactions that have output boxes that already exist in state" in {
    forAll(
      keyCurve25519Gen,
      addressGen,
      Gen.zip(Gen.posNum[Long], simpleValueGen),
      positiveLongGen,
      positiveMediumIntGen,
      blockCurve25519Gen
    ) { (sender, recipient, polyBoxValues, timestamp, timeout, block) =>
      val polyBox = PolyBox(sender._2.address.evidence, polyBoxValues._1, polyBoxValues._2)

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
              sender._2 -> SignatureCurve25519(
                Curve25519.sign(sender._1.privateKey, unsignedTransfer.messageToSign)
              )
            )
          )

      val readableNodeView =
        ReadableNodeView(
          MockImmutableBlockHistory.empty,
          MockStateReader(
            Map(
              sender._2.address -> List(polyBox),
              recipient         -> signedTransfer.newBoxes.toList
            )
          ),
          MemPool(
            TrieMap(
              signedTransfer.id -> UnconfirmedTx(signedTransfer, timestamp)
            )
          )
        )

      val nodeViewHolderBehavior = MockNodeViewHolderBehavior.readOnly(readableNodeView)

      val nodeViewProbe = createTestProbe[NodeViewHolder.ReceivableMessage]()

      val nodeViewHolder = spawn(Behaviors.monitor(nodeViewProbe.ref, nodeViewHolderBehavior))

      implicit val timeProvider: TimeProvider = new TimeProvider {
        override def time: Time = timestamp
      }

      val appSettings =
        TestSettings.defaultSettings.copy(application =
          TestSettings.defaultSettings.application.copy(mempoolTimeout = timeout.millis)
        )

      val underTest = spawn(MemPoolAuditor(nodeViewHolder, spawn(Behaviors.ignore).toClassic, appSettings))

      underTest.tell(MemPoolAuditor.ReceivableMessages.RunCleanup)

      nodeViewProbe.receiveMessage()

      nodeViewProbe.expectMessage(
        NodeViewHolder.ReceivableMessages.EliminateTransactions(List(signedTransfer.id))
      )
    }
  }
}
