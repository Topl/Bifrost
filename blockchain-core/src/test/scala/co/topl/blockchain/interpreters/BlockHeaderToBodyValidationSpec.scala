package co.topl.blockchain.interpreters

import cats.effect.IO
import co.topl.consensus.interpreters.BlockHeaderToBodyValidation
import co.topl.consensus.models.BlockHeaderToBodyValidationFailure.IncorrectTxRoot
import co.topl.models.generators.node.ModelGenerators._
import co.topl.node.models._
import co.topl.typeclasses.implicits._
import munit.CatsEffectSuite
import munit.ScalaCheckEffectSuite
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory

class BlockHeaderToBodyValidationSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  test("validation should fail if block header txRoot is not match block body, i.e. block is arbitrary") {
    PropF.forAllF { block: Block =>
      withMock {
        for {
          underTest <- BlockHeaderToBodyValidation.make[F]()
          result    <- underTest.validate(block)
          _         <- IO(result.left.exists(_.isInstanceOf[IncorrectTxRoot])).assert
        } yield ()
      }
    }
  }

  test("validation should success if block header txRoot is match header body") {
    PropF.forAllF { block: Block =>
      val merkleRootHash = block.body.merkleTreeRootHash
      val correctBlock = block.copy(header = block.header.copy(txRoot = merkleRootHash.data))
      withMock {
        for {
          underTest <- BlockHeaderToBodyValidation.make[F]()
          result    <- underTest.validate(correctBlock)
          _         <- IO(result.exists(_ == correctBlock)).assert
        } yield ()
      }
    }
  }
}
