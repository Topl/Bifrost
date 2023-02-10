package co.topl.ledger.interpreters

import cats.data.{Chain, NonEmptyChain}
import cats.effect.IO
import cats.implicits._
import co.topl.algebras.UnsafeResource
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.crypto.hash.Blake2b256
import co.topl.crypto.signing._
import co.topl.models.ModelGenerators._
import co.topl.models._
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.Sized
import co.topl.consensus.models.SlotData
import co.topl.models.generators.consensus.ModelGenerators.arbitrarySlotData
import co.topl.typeclasses.implicits._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory

import scala.collection.immutable.ListSet

class TransactionAuthorizationValidationSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  type F[A] = IO[A]

  test("Propositions.Knowledge.Ed25519 Authorization") {
    PropF.forAllF { (blockId: TypedIdentifier, sk: SecretKeys.Ed25519) =>
      withMock {
        val vk = VerificationKeys.Ed25519(Sized.strictUnsafe(Ed25519.instance.getVerificationKey(sk.bytes.data)))
        val proposition = Propositions.Knowledge.Ed25519(vk)

        val transaction = createTestTransaction(
          proposition,
          unprovenTransaction =>
            Proofs.Knowledge.Ed25519(
              Sized.strictUnsafe(Ed25519.instance.sign(sk.bytes.data, unprovenTransaction.signableBytes))
            )
        )

        for {
          ed25519Resource <- staticUnsafeResource(Ed25519.instance).pure[F]
          underTest       <- makeValidation(ed25519Resource = ed25519Resource)
          _               <- underTest.validate(blockId)(transaction).map(_.isValid).assert
        } yield ()
      }
    }
  }

  test("Propositions.Knowledge.HashLock Authorization") {
    PropF.forAllF { (blockId: TypedIdentifier, password: Bytes) =>
      withMock {
        val blake2b256 = new Blake2b256()

        val proposition = Propositions.Knowledge.HashLock(Sized.strictUnsafe(blake2b256.hash(password)))

        val transaction = createTestTransaction(
          proposition,
          _ => Proofs.Knowledge.HashLock(password)
        )

        for {
          blake2b256Resource <- staticUnsafeResource(blake2b256).pure[F]
          underTest          <- makeValidation(blake2b256Resource = blake2b256Resource)
          _                  <- underTest.validate(blockId)(transaction).map(_.isValid).assert
        } yield ()
      }
    }
  }

  test("Propositions.Compositional.And Authorization") {
    PropF.forAllF { (blockId: TypedIdentifier, sk: SecretKeys.Ed25519, sk2: SecretKeys.Ed25519) =>
      withMock {
        val vk = VerificationKeys.Ed25519(Sized.strictUnsafe(Ed25519.instance.getVerificationKey(sk.bytes.data)))
        val vk2 = VerificationKeys.Ed25519(Sized.strictUnsafe(Ed25519.instance.getVerificationKey(sk2.bytes.data)))
        val propositionA = Propositions.Knowledge.Ed25519(vk)
        val propositionB = Propositions.Knowledge.Ed25519(vk2)
        val proposition = Propositions.Compositional.And(propositionA, propositionB)

        val transaction = createTestTransaction(
          proposition,
          unprovenTransaction =>
            Proofs.Compositional.And(
              Proofs.Knowledge.Ed25519(
                Sized.strictUnsafe(Ed25519.instance.sign(sk.bytes.data, unprovenTransaction.signableBytes))
              ),
              Proofs.Knowledge.Ed25519(
                Sized.strictUnsafe(Ed25519.instance.sign(sk2.bytes.data, unprovenTransaction.signableBytes))
              )
            )
        )

        val badTransaction = createTestTransaction(
          proposition,
          unprovenTransaction =>
            Proofs.Compositional.And(
              Proofs.Knowledge.Ed25519(
                Sized.strictUnsafe(Ed25519.instance.sign(sk.bytes.data, unprovenTransaction.signableBytes))
              ),
              Proofs.Undefined
            )
        )

        for {
          ed25519Resource <- staticUnsafeResource(Ed25519.instance).pure[F]
          underTest       <- makeValidation(ed25519Resource = ed25519Resource)
          _               <- underTest.validate(blockId)(transaction).map(_.isValid).assert
          _               <- underTest.validate(blockId)(badTransaction).map(_.isInvalid).assert
        } yield ()
      }
    }
  }

  test("Propositions.Compositional.Or Authorization") {
    PropF.forAllF { (blockId: TypedIdentifier, sk: SecretKeys.Ed25519, sk2: SecretKeys.Ed25519) =>
      withMock {
        val vk = VerificationKeys.Ed25519(Sized.strictUnsafe(Ed25519.instance.getVerificationKey(sk.bytes.data)))
        val vk2 = VerificationKeys.Ed25519(Sized.strictUnsafe(Ed25519.instance.getVerificationKey(sk2.bytes.data)))
        val propositionA = Propositions.Knowledge.Ed25519(vk)
        val propositionB = Propositions.Knowledge.Ed25519(vk2)
        val proposition = Propositions.Compositional.Or(propositionA, propositionB)

        val transaction1 = createTestTransaction(
          proposition,
          unprovenTransaction =>
            Proofs.Compositional.Or(
              Proofs.Knowledge.Ed25519(
                Sized.strictUnsafe(Ed25519.instance.sign(sk.bytes.data, unprovenTransaction.signableBytes))
              ),
              Proofs.Knowledge.Ed25519(
                Sized.strictUnsafe(Ed25519.instance.sign(sk2.bytes.data, unprovenTransaction.signableBytes))
              )
            )
        )

        val transaction2 = createTestTransaction(
          proposition,
          unprovenTransaction =>
            Proofs.Compositional.Or(
              Proofs.Knowledge.Ed25519(
                Sized.strictUnsafe(Ed25519.instance.sign(sk.bytes.data, unprovenTransaction.signableBytes))
              ),
              Proofs.Undefined
            )
        )

        val transaction3 = createTestTransaction(
          proposition,
          unprovenTransaction =>
            Proofs.Compositional.Or(
              Proofs.Undefined,
              Proofs.Knowledge.Ed25519(
                Sized.strictUnsafe(Ed25519.instance.sign(sk2.bytes.data, unprovenTransaction.signableBytes))
              )
            )
        )

        val badTransaction = createTestTransaction(
          proposition,
          _ =>
            Proofs.Compositional.Or(
              Proofs.Undefined,
              Proofs.Undefined
            )
        )

        for {
          ed25519Resource <- staticUnsafeResource(Ed25519.instance).pure[F]
          underTest       <- makeValidation(ed25519Resource = ed25519Resource)
          _               <- underTest.validate(blockId)(transaction1).map(_.isValid).assert
          _               <- underTest.validate(blockId)(transaction2).map(_.isValid).assert
          _               <- underTest.validate(blockId)(transaction3).map(_.isValid).assert
          _               <- underTest.validate(blockId)(badTransaction).map(_.isInvalid).assert
        } yield ()
      }
    }
  }

  test("Propositions.Compositional.Threshold Authorization") {
    PropF.forAllF {
      (blockId: TypedIdentifier, sk: SecretKeys.Ed25519, sk2: SecretKeys.Ed25519, sk3: SecretKeys.Ed25519) =>
        withMock {
          val vk = VerificationKeys.Ed25519(Sized.strictUnsafe(Ed25519.instance.getVerificationKey(sk.bytes.data)))
          val vk2 = VerificationKeys.Ed25519(Sized.strictUnsafe(Ed25519.instance.getVerificationKey(sk2.bytes.data)))
          val vk3 = VerificationKeys.Ed25519(Sized.strictUnsafe(Ed25519.instance.getVerificationKey(sk3.bytes.data)))
          val propositionA = Propositions.Knowledge.Ed25519(vk)
          val propositionB = Propositions.Knowledge.Ed25519(vk2)
          val propositionC = Propositions.Knowledge.Ed25519(vk3)
          val proposition = Propositions.Compositional.Threshold(2, ListSet(propositionA, propositionB, propositionC))

          val transaction1 = createTestTransaction(
            proposition,
            unprovenTransaction =>
              Proofs.Compositional.Threshold(
                List(
                  Proofs.Knowledge.Ed25519(
                    Sized.strictUnsafe(Ed25519.instance.sign(sk.bytes.data, unprovenTransaction.signableBytes))
                  ),
                  Proofs.Knowledge.Ed25519(
                    Sized.strictUnsafe(Ed25519.instance.sign(sk2.bytes.data, unprovenTransaction.signableBytes))
                  ),
                  Proofs.Knowledge.Ed25519(
                    Sized.strictUnsafe(Ed25519.instance.sign(sk3.bytes.data, unprovenTransaction.signableBytes))
                  )
                )
              )
          )

          val transaction2 = createTestTransaction(
            proposition,
            unprovenTransaction =>
              Proofs.Compositional.Threshold(
                List(
                  Proofs.Undefined,
                  Proofs.Knowledge.Ed25519(
                    Sized.strictUnsafe(Ed25519.instance.sign(sk2.bytes.data, unprovenTransaction.signableBytes))
                  ),
                  Proofs.Knowledge.Ed25519(
                    Sized.strictUnsafe(Ed25519.instance.sign(sk3.bytes.data, unprovenTransaction.signableBytes))
                  )
                )
              )
          )

          val transaction3 = createTestTransaction(
            proposition,
            unprovenTransaction =>
              Proofs.Compositional.Threshold(
                List(
                  Proofs.Knowledge.Ed25519(
                    Sized.strictUnsafe(Ed25519.instance.sign(sk.bytes.data, unprovenTransaction.signableBytes))
                  ),
                  Proofs.Undefined,
                  Proofs.Knowledge.Ed25519(
                    Sized.strictUnsafe(Ed25519.instance.sign(sk3.bytes.data, unprovenTransaction.signableBytes))
                  )
                )
              )
          )

          val transaction4 = createTestTransaction(
            proposition,
            unprovenTransaction =>
              Proofs.Compositional.Threshold(
                List(
                  Proofs.Knowledge.Ed25519(
                    Sized.strictUnsafe(Ed25519.instance.sign(sk.bytes.data, unprovenTransaction.signableBytes))
                  ),
                  Proofs.Knowledge.Ed25519(
                    Sized.strictUnsafe(Ed25519.instance.sign(sk2.bytes.data, unprovenTransaction.signableBytes))
                  ),
                  Proofs.Undefined
                )
              )
          )

          val badTransaction = createTestTransaction(
            proposition,
            _ =>
              Proofs.Compositional.Threshold(
                List(
                  Proofs.Undefined,
                  Proofs.Undefined,
                  Proofs.Undefined
                )
              )
          )

          // Same as transaction1, but the signatures/proofs are out-of-order
          val badTransaction1 = createTestTransaction(
            proposition,
            unprovenTransaction =>
              Proofs.Compositional.Threshold(
                List(
                  Ed25519.instance.sign(sk2.bytes.data, unprovenTransaction.signableBytes),
                  Ed25519.instance.sign(sk.bytes.data, unprovenTransaction.signableBytes),
                  Ed25519.instance.sign(sk3.bytes.data, unprovenTransaction.signableBytes)
                )
                  .map(bytes => Proofs.Knowledge.Ed25519(Sized.strictUnsafe(bytes)))
              )
          )

          for {
            ed25519Resource <- staticUnsafeResource(Ed25519.instance).pure[F]
            underTest       <- makeValidation(ed25519Resource = ed25519Resource)
            _               <- underTest.validate(blockId)(transaction1).map(_.isValid).assert
            _               <- underTest.validate(blockId)(transaction2).map(_.isValid).assert
            _               <- underTest.validate(blockId)(transaction3).map(_.isValid).assert
            _               <- underTest.validate(blockId)(transaction4).map(_.isValid).assert
            _               <- underTest.validate(blockId)(badTransaction).map(_.isInvalid).assert
            _               <- underTest.validate(blockId)(badTransaction1).map(_.isInvalid).assert
          } yield ()
        }
    }
  }

  test("Propositions.Compositional.Not Authorization") {
    PropF.forAllF { (blockId: TypedIdentifier, sk: SecretKeys.Ed25519) =>
      withMock {
        val vk = VerificationKeys.Ed25519(Sized.strictUnsafe(Ed25519.instance.getVerificationKey(sk.bytes.data)))
        val propositionA = Propositions.Knowledge.Ed25519(vk)
        val proposition = Propositions.Compositional.Not(propositionA)

        val badTransaction = createTestTransaction(
          proposition,
          unprovenTransaction =>
            Proofs.Compositional.Not(
              Proofs.Knowledge.Ed25519(
                Sized.strictUnsafe(Ed25519.instance.sign(sk.bytes.data, unprovenTransaction.signableBytes))
              )
            )
        )

        for {
          ed25519Resource <- staticUnsafeResource(Ed25519.instance).pure[F]
          underTest       <- makeValidation(ed25519Resource = ed25519Resource)
          _               <- underTest.validate(blockId)(badTransaction).map(_.isInvalid).assert
        } yield ()
      }
    }
  }

  test("Propositions.Contextual.HeightLock Authorization") {
    PropF.forAllF { (blockId: TypedIdentifier, height: Long, slotData: SlotData) =>
      withMock {
        for {
          transaction <- createTestTransaction(
            Propositions.Contextual.HeightLock(height),
            _ => Proofs.Contextual.HeightLock()
          ).pure[F]
          fetchSlotData = (_: TypedIdentifier) => slotData.pure[F]
          underTest <- makeValidation(fetchSlotData = fetchSlotData)
          _         <- underTest.validate(blockId)(transaction).map(_.isValid == (slotData.height >= height)).assert
        } yield ()
      }
    }
  }

  test("Propositions.Contextual.RequiredTransactionIO Authorization") {
    PropF.forAllF { (blockId: TypedIdentifier, box: Box) =>
      withMock {
        for {
          badTransaction <- createTestTransaction(
            Propositions.Contextual.RequiredTransactionIO(
              NonEmptyChain(Propositions.Contextual.RequiredTransactionIO.Requirement(box, BoxLocations.Output(0)))
            ),
            _ => Proofs.Contextual.RequiredTransactionIO()
          ).pure[F]
          underTest <- makeValidation()
          _         <- underTest.validate(blockId)(badTransaction).map(_.isInvalid).assert
        } yield ()
      }
    }
  }

  private def makeValidation(
    blake2b256Resource: UnsafeResource[F, Blake2b256] = mock[UnsafeResource[F, Blake2b256]],
    ed25519Resource:    UnsafeResource[F, Ed25519] = mock[UnsafeResource[F, Ed25519]],
    fetchSlotData:      TypedIdentifier => F[SlotData] = mockFunction[TypedIdentifier, F[SlotData]]
  ) =
    TransactionAuthorizationValidation.make[F](
      blake2b256Resource,
      ed25519Resource,
      fetchSlotData
    )

  private def createTestTransaction(proposition: Proposition, prove: Transaction.Unproven => Proof) = {
    val input = arbitraryTransactionUnprovenInput.arbitrary.first
    val unprovenTransaction = Transaction.Unproven(
      Chain(input.copy(proposition = proposition)),
      Chain.empty,
      Transaction.Schedule(0L, 0L, Long.MaxValue),
      None
    )

    unprovenTransaction.prove(_ => prove(unprovenTransaction))
  }

  private def staticUnsafeResource[T](t: T) =
    new UnsafeResource[F, T] {
      def use[Res](f: T => F[Res]): F[Res] = f(t)
    }

}
