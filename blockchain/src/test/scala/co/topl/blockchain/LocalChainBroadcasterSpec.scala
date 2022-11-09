package co.topl.blockchain

import akka.actor.ActorSystem
import akka.stream.testkit.scaladsl.TestSink
import akka.testkit.{TestKit, TestKitBase}
import cats.data.Validated
import cats.effect.IO
import cats.implicits._
import co.topl.consensus.algebras.LocalChainAlgebra
import co.topl.models.ModelGenerators._
import co.topl.models.SlotData
import co.topl.typeclasses.implicits._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory

class LocalChainBroadcasterSpec
    extends CatsEffectSuite
    with ScalaCheckEffectSuite
    with AsyncMockFactory
    with TestKitBase {

  type F[A] = IO[A]

  test("Block IDs should be produced in a stream whenever they are adopted locally") {
    PropF.forAllF { slotData: SlotData =>
      withMock {
        for {
          delegate <- mock[LocalChainAlgebra[F]].pure[F]
          _ = (delegate.adopt _).expects(Validated.Valid(slotData)).once().returning(IO.unit)
          (underTest, source, _, _) <- LocalChainBroadcaster.make(delegate)
          sub = source.runWith(TestSink.probe)
          _ = sub.request(1)
          _ <- underTest.adopt(Validated.Valid(slotData))
          id = sub.expectNext()
          _ = IO(id === slotData.slotId.blockId).assert
        } yield ()
      }
    }
  }

  implicit val system: ActorSystem = ActorSystem("LocalChainBroadcasterSpec")

  override def afterAll(): Unit = {
    super.afterAll()
    TestKit.shutdownActorSystem(system)
  }
}
