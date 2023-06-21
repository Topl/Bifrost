package co.topl.ledger.models

import cats.effect.IO
import cats.implicits._
import co.topl.brambl.models.box.{Attestation, Value}
import co.topl.brambl.models.transaction.{IoTransaction, SpentTransactionOutput, UnspentTransactionOutput}
import co.topl.brambl.models.{Datum, LockAddress, LockId, TransactionOutputAddress}
import co.topl.brambl.syntax._
import com.google.protobuf.ByteString
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import quivr.models.Int128

class MempoolGraphSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  private val lvlValue: Value =
    Value().withLvl(Value.LVL(Int128(ByteString.copyFrom(BigInt(100).toByteArray))))

  private val emptyLockAddress =
    LockAddress(id = LockId(ByteString.copyFrom(Array.fill(32)(0.toByte))))

  private val confirmedTransaction =
    IoTransaction(datum = Datum.IoTransaction.defaultInstance)
      .withOutputs(
        List(
          UnspentTransactionOutput(value = lvlValue, address = emptyLockAddress),
          UnspentTransactionOutput(value = lvlValue, address = emptyLockAddress)
        )
      )
      .embedId

  test("add and remove a transaction") {
    val tx1 =
      IoTransaction(datum = Datum.IoTransaction.defaultInstance)
        .withInputs(
          List(stxo(confirmedTransaction, 0))
        )
        .withOutputs(
          List(
            UnspentTransactionOutput(value = lvlValue, address = emptyLockAddress)
          )
        )
        .embedId

    for {
      graph1 <- MempoolGraph.empty.add(tx1).pure[F]
      _      <- IO(graph1.transactions(tx1.id)).assertEquals(tx1)
      _      <- IO(graph1.unresolved(tx1.id).toList).assertEquals(List(0))
      _      <- IO(graph1.spenders(tx1.id).isEmpty).assert
      (graph2, removed) = graph1.removeSubtree(tx1)
      _ <- IO(removed).assertEquals(Set(tx1))
      _ <- IO(graph2.transactions.isEmpty).assert
      _ <- IO(graph2.spenders.isEmpty).assert
      _ <- IO(graph2.unresolved.isEmpty).assert
      _ <- IO(graph2).assertEquals(MempoolGraph.empty)
      graph3 = graph1.removeSingle(tx1)
      _ <- IO(graph3).assertEquals(graph2)
    } yield ()
  }

  test("add and remove a transaction graph") {
    val tx1 =
      IoTransaction(datum = Datum.IoTransaction.defaultInstance)
        .withInputs(
          List(stxo(confirmedTransaction, 0))
        )
        .withOutputs(
          List(
            UnspentTransactionOutput(value = lvlValue, address = emptyLockAddress)
          )
        )
        .embedId
    val tx2 =
      IoTransaction(datum = Datum.IoTransaction.defaultInstance)
        .withInputs(
          List(
            stxo(tx1, 0),
            stxo(confirmedTransaction, 1)
          )
        )
        .withOutputs(
          List(
            UnspentTransactionOutput(value = lvlValue, address = emptyLockAddress)
          )
        )
        .embedId
    val tx3 =
      IoTransaction(datum = Datum.IoTransaction.defaultInstance)
        .withInputs(
          List(
            stxo(tx2, 0)
          )
        )
        .embedId

    for {
      graph1 <- MempoolGraph.empty.add(tx1).add(tx2).add(tx3).pure[F]
      _      <- IO(graph1.unresolved(tx1.id).toList).assertEquals(List(0))
      _      <- IO(graph1.unresolved(tx2.id).toList).assertEquals(List(1))
      _      <- IO(!graph1.unresolved.contains(tx3.id)).assert
      _      <- IO(graph1.spenders(tx1.id)).assertEquals(Map(0 -> Set((tx2.id, 0))))
      _      <- IO(graph1.spenders(tx2.id)).assertEquals(Map(0 -> Set((tx3.id, 0))))
      _      <- IO(graph1.spenders(tx3.id).isEmpty).assert
      (graph2, removed1) = graph1.removeSubtree(tx3)
      _ <- IO(removed1).assertEquals(Set(tx3))
      _ <- IO(graph2.spenders(tx1.id)).assertEquals(Map(0 -> Set((tx2.id, 0))))
      _ <- IO(graph2.spenders(tx2.id)(0).isEmpty).assert
      (graph3, removed2) = graph1.removeSubtree(tx2)
      _ <- IO(removed2).assertEquals(Set(tx2, tx3))
      _ <- IO(graph3.spenders(tx1.id)(0).isEmpty).assert
      (graph4, removed3) = graph1.removeSubtree(tx1)
      _ <- IO(removed3).assertEquals(Set(tx1, tx2, tx3))
      _ <- IO(graph4).assertEquals(MempoolGraph.empty)
    } yield ()

  }

  test("add and remove a transaction graph with double spends") {
    val tx1 =
      IoTransaction(datum = Datum.IoTransaction.defaultInstance)
        .withInputs(
          List(stxo(confirmedTransaction, 0))
        )
        .withOutputs(
          List(
            UnspentTransactionOutput(value = lvlValue, address = emptyLockAddress)
          )
        )
        .embedId
    val tx2 =
      IoTransaction(datum = Datum.IoTransaction.defaultInstance)
        .withInputs(
          List(
            stxo(tx1, 0),
            stxo(confirmedTransaction, 1)
          )
        )
        .withOutputs(
          List(
            UnspentTransactionOutput(value = lvlValue, address = emptyLockAddress)
          )
        )
        .embedId
    val tx2a =
      IoTransaction(datum = Datum.IoTransaction.defaultInstance)
        .withInputs(
          List(
            stxo(confirmedTransaction, 1),
            stxo(tx1, 0)
          )
        )
        .withOutputs(
          List(
            UnspentTransactionOutput(value = lvlValue, address = emptyLockAddress),
            UnspentTransactionOutput(value = lvlValue, address = emptyLockAddress)
          )
        )
        .embedId
    val tx3 =
      IoTransaction(datum = Datum.IoTransaction.defaultInstance)
        .withInputs(
          List(
            stxo(tx2, 0)
          )
        )
        .embedId

    for {
      graph1 <- MempoolGraph.empty.add(tx1).add(tx2).add(tx2a).add(tx3).pure[F]
      _      <- IO(graph1.unresolved(tx1.id).toList).assertEquals(List(0))
      _      <- IO(graph1.unresolved(tx2.id).toList).assertEquals(List(1))
      _      <- IO(graph1.unresolved(tx2a.id).toList).assertEquals(List(0))
      _      <- IO(!graph1.unresolved.contains(tx3.id)).assert
      _      <- IO(graph1.spenders(tx1.id)).assertEquals(Map(0 -> Set((tx2.id, 0), (tx2a.id, 1))))
      _      <- IO(graph1.spenders(tx2.id)).assertEquals(Map(0 -> Set((tx3.id, 0))))
      _      <- IO(graph1.spenders(tx2a.id).isEmpty).assert
      (graph2, removed1) = graph1.removeSubtree(tx3)
      _ <- IO(removed1).assertEquals(Set(tx3))
      _ <- IO(graph1.spenders(tx1.id)).assertEquals(Map(0 -> Set((tx2.id, 0), (tx2a.id, 1))))
      _ <- IO(graph2.spenders(tx2.id)(0).isEmpty).assert
      _ <- IO(graph1.spenders(tx2a.id).isEmpty).assert
      (graph3, removed2) = graph1.removeSubtree(tx2)
      _ <- IO(removed2).assertEquals(Set(tx2, tx3))
      _ <- IO(graph3.spenders(tx1.id)).assertEquals(Map(0 -> Set((tx2a.id, 1))))
      (graph4, removed3) = graph1.removeSubtree(tx2a)
      _ <- IO(removed3).assertEquals(Set(tx2a))
      _ <- IO(graph4.spenders(tx1.id)).assertEquals(Map(0 -> Set((tx2.id, 0))))
      (graph5, removed4) = graph1.removeSubtree(tx1)
      _ <- IO(removed4).assertEquals(Set(tx1, tx2, tx2a, tx3))
      _ <- IO(graph5).assertEquals(MempoolGraph.empty)
    } yield ()

  }

  private def stxo(tx: IoTransaction, index: Int) =
    SpentTransactionOutput(
      TransactionOutputAddress(id = tx.id).withIndex(index),
      Attestation().withPredicate(Attestation.Predicate.defaultInstance),
      tx.outputs(index).value
    )

}
