package co.topl.consensus.interpreters

import cats.effect.IO
import co.topl.consensus.BlockHeaderToBodyValidationFailure.IncorrectTxRoot
import co.topl.models.BlockV2
import co.topl.models.ModelGenerators._
import co.topl.typeclasses.implicits._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory

class BlockHeaderToBodyValidationSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  test("validation should fail if block header txRoot is not match block body, i.e. block is arbitrary") {
    PropF.forAllF { block: BlockV2 =>
      withMock {
        for {
          underTest <- BlockHeaderToBodyValidation.Eval.make[F]()
          result    <- underTest.validate(block)
          _         <- IO(result.left.exists(_.isInstanceOf[IncorrectTxRoot])).assert
        } yield ()
      }
    }
  }

  test("validation should success if block header txRoot is match header body") {
    PropF.forAllF { block: BlockV2 =>
      val merkleRootHash = block.blockBodyV2.merkleTreeRootHash
      val correctBlock = block.copy(headerV2 = block.headerV2.copy(txRoot = merkleRootHash))
      withMock {
        for {
          underTest <- BlockHeaderToBodyValidation.Eval.make[F]()
          result    <- underTest.validate(correctBlock)
          _         <- IO(result.exists(_ == correctBlock)).assert
        } yield ()
      }
    }
  }
}
