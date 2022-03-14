package co.topl.eventtree

import cats.data.OptionT
import cats.effect.{IO, Sync}
import cats.implicits._
import cats.{Functor, MonadThrow, Semigroupal}
import co.topl.algebras.Store
import co.topl.interpreters.RefStore
import co.topl.models._
import co.topl.typeclasses.implicits._
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, EitherValues, OptionValues}
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class EventSourcedStateSpec
    extends AnyFlatSpec
    with BeforeAndAfterAll
    with MockFactory
    with Matchers
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with EitherValues
    with OptionValues {

  type F[A] = IO[A]
  import LedgerTreeTestSupport._

  behavior of "EventSourcedState"

  it should "traverse events forwards and backwards to provide the correct state along a tree" in {
    val eventStore = RefStore.Eval.make[F, Tx]().value
    val deltaStore = RefStore.Eval.make[F, LedgerUnapply]().value
    val tree = ParentChildTree.FromRef.make[F, TypedIdentifier].value

    txData
      .foldLeftM[F, Unit](()) { case (_, (txId, from, amount, to)) =>
        eventStore.put(txId, Tx(txId, (from, amount), to))
      }
      .value

    txAssociations.foldLeftM[F, Unit](()) { case (_, (c, p)) => tree.associate(c.asTxId, p.asTxId) }.value
    val ledgerStore = RefStore.Eval.make[F, Bytes]().value
    val initialEventId = "-1".asTxId

    ledgerStore.put(Ledger.Eval.CurrentEventIdId, initialEventId.allBytes).value

    val ledger = Ledger.Eval.make[F](ledgerStore)

    ledger.modifyBalanceOf("alice", _ => 100L.some.pure[F]).value

    val eventTree = EventSourcedState.OfTree
      .make[F, Tx, Ledger[F], LedgerUnapply](
        initialState = Sync[F].delay(ledger),
        initialEventId = Sync[F].delay(initialEventId),
        eventAsUnapplyEvent = (tx, ledger) =>
          for {
            currentId      <- ledger.eventId
            currentBalance <- OptionT(ledger.balanceOf(tx.from._1)).getOrElse(0L)
          } yield LedgerUnapply(currentId, currentBalance, tx),
        applyEvent = (ledger, tx) =>
          for {
            _ <- ledger.modifyBalanceOf(tx.from._1, b => (b.getOrElse(0L) - tx.from._2).some.pure[F])
            _ <- ledger.modifyBalanceOf(tx.to, b => (b.getOrElse(0L) + tx.from._2).some.pure[F])
            _ <- ledger.setEventId(tx.id)
          } yield ledger,
        unapplyEvent = (ledger, unapply) =>
          for {
            _ <- ledger.modifyBalanceOf(unapply.tx.from._1, _ => unapply.senderPreviousBalance.some.pure[F])
            _ <- ledger.modifyBalanceOf(unapply.tx.to, b => (b.getOrElse(0L) - unapply.tx.from._2).some.pure[F])
            _ <- ledger.setEventId(unapply.previousTxId)
          } yield ledger,
        eventStore = eventStore,
        unapplyEventStore = deltaStore,
        parentChildTree = tree
      )
      .value
    val ledgerC1 = eventTree.stateAt("c1".asTxId).value

    ledgerC1.balanceOf("alice").value.value shouldBe 75
    ledgerC1.balanceOf("bob").value.value shouldBe 15
    ledgerC1.balanceOf("chelsea").value.value shouldBe 10

    val ledgerC2 = eventTree.stateAt("c2".asTxId).value
    ledgerC2.balanceOf("alice").value.value shouldBe 75
    ledgerC2.balanceOf("bob").value.value shouldBe 10
    ledgerC2.balanceOf("chelsea").value.value shouldBe 15

    val ledgerE1D1C1 = eventTree.stateAt("e1d1c1".asTxId).value
    ledgerE1D1C1.balanceOf("alice").value.value shouldBe 80
    ledgerE1D1C1.balanceOf("bob").value.value shouldBe 10
    ledgerE1D1C1.balanceOf("chelsea").value.value shouldBe 10
  }
}

trait Ledger[F[_]] {
  def eventId: F[TypedIdentifier]
  def setEventId(id:        TypedIdentifier): F[Unit]
  def balanceOf(name:       String): F[Option[Long]]
  def modifyBalanceOf(name: String, f: Option[Long] => F[Option[Long]]): F[Unit]
}

object Ledger {

  object Eval {
    val CurrentEventIdId = TypedBytes(-1: Byte, Bytes(-1: Byte))

    def make[F[_]: MonadThrow](store: Store[F, Bytes]): Ledger[F] = new Ledger[F] {

      def eventId: F[TypedIdentifier] =
        OptionT(store.get(CurrentEventIdId)).foldF(
          (new NoSuchElementException(CurrentEventIdId.show).raiseError[F, TypedIdentifier])
        )(TypedBytes(_).pure[F])

      def setEventId(id: TypedIdentifier): F[Unit] =
        store.put(CurrentEventIdId, id.allBytes)

      def balanceOf(name: String): F[Option[Long]] =
        OptionT
          .fromOption[F](Bytes.encodeUtf8(name).toOption)
          .map(TypedBytes(1: Byte, _))
          .flatMapF(store.get)
          .map(_.toLong())
          .value

      def modifyBalanceOf(name: String, f: Option[Long] => F[Option[Long]]): F[Unit] =
        OptionT
          .fromOption[F](Bytes.encodeUtf8(name).toOption)
          .map(TypedBytes(1: Byte, _))
          .getOrElseF(???)
          .flatTap(key =>
            OptionT(store.get(key))
              .map(_.toLong())
              .flatTransform(f)
              .foldF(store.remove(key))(t => store.put(key, Bytes.fromLong(t)))
          )
          .void
    }
  }
}

object LedgerTreeTestSupport {

  case class Tx(id: TypedIdentifier, from: (String, Long), to: String)

  val txData = List(
    ("a".asTxId, "alice", 10L, "bob"),
    ("b".asTxId, "alice", 10L, "chelsea"),
    ("c1".asTxId, "alice", 5L, "bob"),
    ("c2".asTxId, "alice", 5L, "chelsea"),
    ("d1c1".asTxId, "bob", 5L, "chelsea"),
    ("e1d1c1".asTxId, "chelsea", 5L, "alice")
  )

  val txAssociations = List(
    "e1d1c1" -> "d1c1",
    "d1c1"   -> "c1",
    "c1"     -> "b",
    "b"      -> "a",
    "a"      -> "-1",
    "c2"     -> "b"
  )

  case class LedgerUnapply(previousTxId: TypedIdentifier, senderPreviousBalance: Long, tx: Tx)

  def printLedgerBalances[F[_]: Functor: Semigroupal](ledger: Ledger[F]): F[Unit] =
    (ledger.balanceOf("alice"), ledger.balanceOf("bob"), ledger.balanceOf("chelsea")).mapN(
      (aBalance, bBalance, cBalance) => println(show"Balances: alice=$aBalance bob=$bBalance chelsea=$cBalance")
    )

  implicit class StringOps(string: String) {
    def asTxId: TypedIdentifier = TypedBytes(1: Byte, Bytes(string.getBytes()))
  }

  implicit class CatsIOOps[T](io: IO[T]) {
    def value: T = io.unsafeRunSync()(cats.effect.unsafe.IORuntime.global)
  }
}
