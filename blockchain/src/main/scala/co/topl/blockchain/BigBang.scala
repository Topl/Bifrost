package co.topl.blockchain

import cats.data.Chain
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.crypto.hash.Blake2b256
import co.topl.models._
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility._
import co.topl.typeclasses.implicits._

/**
 * The beginning of everything.  ("everything" of course just means the first block of a blockchain)
 */
object BigBang {

  /**
   * Represents a way of configuring the variables of the Big Bang Block.
   * @param timestamp The unix epoch (in milliseconds) of the block.  Also represents the timestamp of the first slot
   *                  of the blockchain's clock.
   * @param outputs The initial set of outputs for the blockchain.  This generally includes the initial distribution
   *                to the blockchain's initial investors.  In addition, it should specify the staking registrations
   *                for the blockchain's initial operators.
   * @param etaPrefix a sequence of bytes to be prepended to the value that gets hashed to produce the Big Bang Eta.
   */
  case class Config(
    timestamp: Timestamp,
    outputs:   Chain[Transaction.Output],
    etaPrefix: Bytes = Config.DefaultEtaPrefix
  )

  object Config {

    val DefaultEtaPrefix: Bytes =
      Bytes.encodeUtf8("genesis").toOption.get

  }

  /**
   * Constructs a full block using the given Big Bang Configuration
   */
  def block(implicit config: Config): BlockV2.Full = {
    val transactions: Chain[Transaction] =
      Chain(
        Transaction(
          inputs = Chain.empty,
          outputs = config.outputs,
          schedule = Transaction.Schedule(0L, Slot, Slot), // This transaction is only valid at the BigBang slot
          data = None
        )
      )

    val eta: Eta =
      new Blake2b256().hash(
        config.etaPrefix +:
        transactions.map(_.immutableBytes).toList: _*
      )

    val header =
      BlockHeaderV2(
        parentHeaderId = ParentId,
        parentSlot = ParentSlot,
        txRoot = transactions.merkleTreeRootHash,
        bloomFilter = transactions.bloomFilter,
        timestamp = config.timestamp,
        height = Height,
        slot = Slot,
        eligibilityCertificate = vrfCertificate(eta),
        operationalCertificate = kesCertificate,
        metadata = None,
        address = StakingAddresses.Operator(
          VerificationKeys.Ed25519(zeroBytes(Lengths.`32`))
        )
      )
    BlockV2.Full(header, transactions)
  }

  val ParentId: TypedIdentifier = TypedBytes(IdentifierTypes.Block.HeaderV2, Bytes(Array.fill[Byte](32)(0)))
  val ParentSlot: Slot = -1
  val Slot = 0
  val Height = 1

  def vrfCertificate(eta: Eta): EligibilityCertificate = EligibilityCertificate(
    Proofs.Knowledge.VrfEd25519(zeroBytes(Lengths.`80`)),
    VerificationKeys.VrfEd25519(VerificationKeys.Ed25519(zeroBytes[VerificationKeys.VrfEd25519.Length]).bytes),
    thresholdEvidence = Sized.strictUnsafe(Bytes(Array.fill[Byte](32)(0))),
    eta = eta
  )

  val kesCertificate: OperationalCertificate = OperationalCertificate(
    VerificationKeys.KesProduct(zeroBytes(Lengths.`32`), 0),
    Proofs.Knowledge.KesProduct(
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519(zeroBytes(Lengths.`32`)),
        Proofs.Knowledge.Ed25519(zeroBytes(Lengths.`64`)),
        Vector.empty
      ),
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519(zeroBytes(Lengths.`32`)),
        Proofs.Knowledge.Ed25519(zeroBytes(Lengths.`64`)),
        Vector.empty
      ),
      zeroBytes(Lengths.`32`)
    ),
    VerificationKeys.Ed25519(zeroBytes(Lengths.`32`)),
    Proofs.Knowledge.Ed25519(zeroBytes(Lengths.`64`))
  )

  def zeroBytes[L <: Length](implicit l: L): Sized.Strict[Bytes, L] =
    Sized.strictUnsafe[Bytes, L](Bytes(Array.fill(l.value)(0: Byte)))
}
