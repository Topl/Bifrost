package co.topl.blockchain

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestKitBase}

import cats.effect.IO
import cats.implicits._
import co.topl.ledger.algebras.MempoolAlgebra
import co.topl.models.ModelGenerators._
import co.topl.models.TypedIdentifier
import co.topl.typeclasses.implicits._
import fs2.concurrent.Topic
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory

class MempoolBroadcasterSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory with TestKitBase {

  type F[A] = IO[A]

  test("Transactions should be produced in a stream whenever they are added to the mempool") {
    PropF.forAllF { transactionId: TypedIdentifier =>
      withMock {
        for {
          delegate <- mock[MempoolAlgebra[F]].pure[F]
          _ = (delegate.add _).expects(transactionId).once().returning(IO.unit)
          txsAdoptionsTopic <- Topic[F, TypedIdentifier]
          (underTest)       <- MempoolBroadcaster.make(delegate, txsAdoptionsTopic)
          _ <- txsAdoptionsTopic.subscribeAwait(Int.MaxValue).use { stream =>
            for {
              _ <- underTest.add(transactionId)
              i <- stream.take(1).compile.toVector
              _ = IO(i.head === transactionId).assert
            } yield ()
          }
        } yield ()
      }
    }
  }

  implicit val system: ActorSystem = ActorSystem("MempoolBroadcasterSpec")

  override def afterAll(): Unit = {
    super.afterAll()
    TestKit.shutdownActorSystem(system)
  }
}
