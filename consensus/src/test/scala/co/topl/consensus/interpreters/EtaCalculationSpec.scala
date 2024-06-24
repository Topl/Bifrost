package co.topl.consensus.interpreters

import cats.effect.IO
import cats.implicits._
import cats.effect.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.brambl.utils.CatsUnsafeResource
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.consensus.models.BlockHeader
import co.topl.consensus.models.{BlockId, SlotData, SlotId, VrfArgument}
import co.topl.consensus.rhoToRhoNonceHash
import co.topl.crypto.signing.Ed25519VRF
import co.topl.crypto.hash.{Blake2b256, Blake2b512}
import co.topl.models.ModelGenerators._
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.models._
import co.topl.models.utility.HasLength.instances.byteStringLength
import co.topl.models.utility._
import com.google.protobuf.ByteString
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.Gen
import org.scalamock.munit.AsyncMockFactory
import co.topl.algebras.Stats.Implicits._

class EtaCalculationSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  test("compute the eta for an epoch") {
    withMock {
      val ed25519Vrf: Ed25519VRF = Ed25519VRF.precomputed()
      val clock = mock[ClockAlgebra[F]]
      val bigBangHeader = arbitraryHeader.arbitrary.first.copy(slot = 0L, parentSlot = -1L)

      (() => clock.slotsPerEpoch)
        .expects()
        .anyNumberOfTimes()
        .returning(15L.pure[F])

      val fetchSlotData = mockFunction[BlockId, F[SlotData]]
      val epoch = 0L
      val (skVrf, _) = ed25519Vrf.generateRandom
      val args: List[(Slot, ByteString)] = List.tabulate(8) { offset =>
        val slot = offset.toLong + 1
        val signature =
          ed25519Vrf.sign(
            skVrf,
            VrfArgument(Sized.strictUnsafe(bigBangHeader.eligibilityCertificate.eta), slot).signableBytes.toByteArray
          )
        slot -> ByteString.copyFrom(signature)
      }

      val blocks: List[BlockHeader] =
        bigBangHeader ::
        LazyList
          .unfold(List(bigBangHeader)) {
            case items if items.length == args.length + 1 => None
            case items =>
              val (slot, signature) = args(items.length - 1)
              val nextHeader = headerGen(
                slotGen = Gen.const[Long](slot),
                parentSlotGen = Gen.const(items.last.slot),
                eligibilityCertificateGen = arbitraryEligibilityCertificate.arbitrary.map(c =>
                  c.copy(vrfSig = signature, eta = bigBangHeader.eligibilityCertificate.eta)
                ),
                parentHeaderIdGen = Gen.const(items.last.id)
              ).first
              (nextHeader -> (items :+ nextHeader)).some
          }
          .toList

      val slotDataMap =
        blocks.map(_.slotData(ed25519Vrf)).map(sd => sd.slotId.blockId -> sd).toMap

      fetchSlotData
        .expects(*)
        .onCall((id: BlockId) => slotDataMap(id).pure[F])
        .anyNumberOfTimes()

      val r =
        for {
          blake2b256R <- CatsUnsafeResource.make[F, Blake2b256](new Blake2b256, 1).toResource
          blake2b512R <- CatsUnsafeResource.make[F, Blake2b512](new Blake2b512, 1).toResource
          underTest <- EtaCalculation
            .make[F](
              fetchSlotData,
              clock,
              Sized.strictUnsafe(bigBangHeader.eligibilityCertificate.eta),
              blake2b256R,
              blake2b512R
            )
            .toResource
          actual <- underTest.etaToBe(SlotId(blocks.last.slot, blocks.last.id), 16L).toResource
          expected = EtaCalculationSpec.expectedEta(
            Sized.strictUnsafe(bigBangHeader.eligibilityCertificate.eta),
            epoch + 1,
            blocks.tail
              .map(_.eligibilityCertificate.vrfSig.toByteArray)
              .map(ed25519Vrf.proofToHash)
              .map(ByteString.copyFrom)
              .map(bytes => Rho(Sized.strictUnsafe(bytes)))
          )
          _ = assert(actual == expected)
        } yield ()

      r.use_
    }
  }
}

object EtaCalculationSpec {

  private[consensus] def expectedEta(previousEta: Eta, epoch: Epoch, rhoValues: List[Rho]): Eta = {
    implicit val blake2b256: Blake2b256 = new Blake2b256
    implicit val blake2b512: Blake2b512 = new Blake2b512
    val messages: List[Bytes] =
      List(previousEta.data) ++ List(ByteString.copyFrom(BigInt(epoch).toByteArray)) ++ rhoValues
        .map(_.sizedBytes.data)
        .map(rhoToRhoNonceHash)
    Sized.strictUnsafe(blake2b256.hash(messages.foldLeft(ByteString.EMPTY)(_ concat _)))
  }
}
