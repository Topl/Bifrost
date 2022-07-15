package co.topl.blockchain

import cats.data.Chain
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.codecs.json.tetra.instances._
import co.topl.crypto.hash.Blake2b256
import co.topl.models._
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility._
import co.topl.typeclasses.implicits._
import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, Encoder, Json}

object BigBang {

  case class Config(
    timestamp: Timestamp,
    outputs:   Chain[Transaction.Output],
    etaPrefix: Bytes = Config.DefaultEtaPrefix
  )

  object Config {

    val DefaultEtaPrefix: Bytes =
      Bytes.encodeUtf8("genesis").toOption.get

    implicit val jsonEncoder: Encoder[Config] =
      config =>
        Json.obj(
          "timestamp" -> config.timestamp.asJson,
          "outputs"   -> config.outputs.asJson,
          "etaPrefix" -> config.etaPrefix.toBase58.asJson
        )

    implicit val jsonDecoder: Decoder[Config] =
      h =>
        for {
          timestamp <- h.downField("timestamp").as[Timestamp]
          outputs   <- h.downField("outputs").as[Chain[Transaction.Output]]
          etaPrefix <- h
            .downField("etaPrefix")
            .as[String]
            .flatMap(
              Bytes.fromBase58(_).toRight(DecodingFailure("Not Base58", Nil))
            )
        } yield Config(timestamp, outputs, etaPrefix)

  }

  def block(implicit config: Config): BlockV2.Full = {
    val transactions: Chain[Transaction] =
      Chain(
        Transaction(
          inputs = Chain.empty,
          outputs = config.outputs,
          chronology = Transaction.Chronology(0L, 0L, 0L), // This transaction is only valid at the BigBang slot
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
        txRoot = transactions.merkleTree,
        bloomFilter = transactions.bloomFilter,
        timestamp = config.timestamp,
        height = 1,
        slot = 0,
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
