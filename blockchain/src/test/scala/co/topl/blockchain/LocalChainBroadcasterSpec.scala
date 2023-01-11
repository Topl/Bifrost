package co.topl.blockchain

import cats.data.Validated
import cats.effect.IO
import cats.effect.kernel.Sync
import cats.implicits._
import co.topl.consensus.algebras.LocalChainAlgebra
import co.topl.models.ModelGenerators._
import co.topl.models.SlotData
import co.topl.typeclasses.implicits._
import fs2._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory

import scala.concurrent.duration._

class LocalChainBroadcasterSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  test("Block IDs should be produced in a stream whenever they are adopted locally") {
    PropF.forAllF { slotData: SlotData =>
      withMock {
        for {
          delegate <- mock[LocalChainAlgebra[F]].pure[F]
          _ = (delegate.adopt _).expects(*).once().returning(IO.unit)
          (underTest, adoptionsTopic) <- LocalChainBroadcaster.make(delegate)
          id <- adoptionsTopic.subscribeUnbounded
            .concurrently(
              Stream.eval(
                underTest.adopt(Validated.Valid(slotData)) >> Sync[F].delay(
                  adoptionsTopic.close.unsafeRunAndForget()
                )
              )
            )
            .timeout(3.seconds)
            .compile
            .lastOrError
          _ = IO(id === slotData.slotId.blockId).assert
        } yield ()
      }
    }
  }
}
