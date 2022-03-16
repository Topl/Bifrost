package co.topl.codecs.binary.typeclasses

import cats.data.Chain
import cats.implicits._
import co.topl.codecs.binary.scodecs._
import co.topl.codecs.bytes.typeclasses.Signable
import co.topl.crypto.hash.Blake2b256
import co.topl.models.Transaction.{ArbitOutput, AssetOutput, PolyOutput}
import co.topl.models._
import co.topl.models.utility.HasLength.instances.{bigIntLength, bytesLength, latin1DataLength}
import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility.{Lengths, Sized}
import co.topl.modifier.box.{ArbitBox, AssetBox, AssetCode, PolyBox}
import co.topl.modifier.transaction.{ArbitTransfer, AssetTransfer, PolyTransfer}
import co.topl.utils.StringDataTypes.{Latin1Data => DionLatin1Data}
import co.topl.utils.{Int128 => DionInt128}
import com.google.common.primitives.{Ints, Longs}
import scodec.bits.BitVector
import scodec.interop.cats._
import scodec.{Attempt, Codec, Encoder, Err}
import shapeless.HNil

trait SignableInstances {

  /**
   * Encodes the transfer-type of a Tetra-Transfer
   */
  private val tetraTransferTypeEncoder: Encoder[Transaction.Unproven] =
    Encoder(transaction =>
      transaction.coinOutputs.head match {
        case _: PolyOutput =>
          Attempt.successful(BitVector(PolyTransfer.typePrefix))
        case _: ArbitOutput =>
          Attempt.successful(BitVector(ArbitTransfer.typePrefix))
        case _: AssetOutput =>
          Attempt.successful(BitVector(AssetTransfer.typePrefix))
      }
    )

  private val typedEvidenceEncoder: Codec[TypedEvidence] =
    (byteCodec :: bytesCodec(32))
      .exmap[TypedEvidence](
        bytes =>
          Sized
            .strict[Bytes, Lengths.`32`.type](Bytes(bytes.tail.head))
            .map(evidence => Attempt.successful(TypedEvidence(bytes.head, evidence)))
            .valueOr(error => Attempt.failure(Err(s"Failed to create evidence: $error"))),
        typedEvidence => Attempt.successful(typedEvidence.typePrefix :: typedEvidence.evidence.data.toArray :: HNil)
      )

  private val dionAddressCodec: Codec[DionAddress] =
    (byteCodec.xmap[NetworkPrefix](NetworkPrefix.apply, _.value) :: typedEvidenceEncoder).as[DionAddress]

  private val shortNameCodec: Codec[Sized.Max[Latin1Data, Lengths.`8`.type]] =
    bytesCodec(AssetCode.shortNameLimit)
      .exmap(
        bytes =>
          Sized
            .max[Latin1Data, Lengths.`8`.type](Latin1Data.fromData(bytes))
            .fold(
              failure => Attempt.failure(Err(s"Invalid Latin-1 Data: $failure")),
              Attempt.successful
            ),
        data => Attempt.successful(data.data.bytes.padTo(Lengths.`8`.value, 0: Byte))
      )

  private val assetCodeCodec: Codec[Box.Values.Asset.Code] =
    (byteCodec :: dionAddressCodec :: shortNameCodec).as[Box.Values.Asset.Code]

  private val tetraInt128Codec: Codec[Int128] =
    int128Codec
      .exmap(
        int128 =>
          Sized
            .max[BigInt, Lengths.`128`.type](BigInt(int128.toByteArray))
            .fold(failure => Attempt.failure(Err(s"Invalid Int-128 value: $failure")), Attempt.successful),
        int128 => Attempt.successful(DionInt128(int128.data))
      )

  private val metadataCodec: Codec[Option[Sized.Max[Latin1Data, Lengths.`127`.type]]] =
    optionCodec(
      byteStringCodec.exmap(
        string =>
          Latin1Data
            .validated(string)
            .leftMap(failure => Err(s"Invalid Latin-1 String: $failure"))
            .toEither
            .flatMap(
              Sized.max[Latin1Data, Lengths.`127`.type](_).leftMap(failure => Err(s"Invalid Latin-1 String: $failure"))
            )
            .fold(Attempt.failure, Attempt.successful),
        latin1Data => Attempt.successful(latin1Data.data.value)
      )
    )

  private val assetValueCodec: Codec[Box.Values.Asset] =
    (tetraInt128Codec :: assetCodeCodec :: arrayCodec[Byte].xmap[Bytes](Bytes.apply, _.toArray) :: metadataCodec)
      .as[Box.Values.Asset]

  private def nonceEncoder(inputData: BitVector)(implicit blake2b256: Blake2b256): Encoder[Int] =
    Encoder(index =>
      longCodec.encode(
        Longs.fromByteArray(
          blake2b256
            .hash(inputData.toByteVector ++ Bytes(Ints.toByteArray(index)))
            .data
            .toArray
            .take(Longs.BYTES)
        )
      )
    )

  private def outputsEncoder(
    inputData:           BitVector
  )(implicit blake2b256: Blake2b256): Encoder[(Option[Transaction.PolyOutput], Chain[Transaction.CoinOutput])] =
    Encoder(outputs =>
      (
        outputs._1.fold(Chain.empty[(Transaction.CoinOutput, Int)])(value => Chain(value -> 0)) ++
        outputs._2.mapWithIndex((output, index) => output -> (index + 1))
      ).map {
        case (Transaction.PolyOutput(address, amount), index) =>
          tetraInt128Codec.encode(amount).map((BitVector(PolyBox.typePrefix), _, address.typedEvidence, index))
        case (Transaction.ArbitOutput(address, amount), index) =>
          tetraInt128Codec.encode(amount).map((BitVector(ArbitBox.typePrefix), _, address.typedEvidence, index))
        case (Transaction.AssetOutput(address, asset), index) =>
          assetValueCodec.encode(asset).map((BitVector(AssetBox.typePrefix), _, address.typedEvidence, index))
      }.sequence
        .flatMap(
          _.traverse { case (typeBits, valueBits, evidence, index) =>
            (
              typedEvidenceEncoder.encode(evidence),
              nonceEncoder(inputData).encode(index)
            ).mapN((evidenceBits, nonceBits) => typeBits ++ evidenceBits ++ nonceBits ++ valueBits)
          }
        )
        .map(_.fold)
    )

  private def inputsEncoder(implicit blake2b256: Blake2b256): Encoder[Chain[BoxReference]] =
    Encoder(inputs =>
      inputs
        .map(input =>
          typedEvidenceEncoder
            .encode(input._1.typedEvidence)
            .map(_.toByteVector)
            .map(evidenceBytes => blake2b256.hash(evidenceBytes ++ Bytes(Longs.toByteArray(input._2))))
            .map(_.data)
        )
        .sequence
        .map(_.fold.toBitVector)
    )

  private val timestampEncoder: Encoder[Timestamp] =
    Encoder(timestamp => Attempt.successful(BitVector(Longs.toByteArray(timestamp))))

  private val feeEncoder: Encoder[Int128] =
    Encoder(int128 => Attempt.successful(BitVector(DionInt128(int128.data).toByteArray)))

  private val mintingEncoder: Encoder[Boolean] =
    Encoder(minting =>
      if (minting) Attempt.successful(BitVector(1: Byte))
      else Attempt.successful(BitVector(0: Byte))
    )

  private val dataCodec: Codec[TransactionData] =
    latin1DataCodec.exmap(
      latin1Data =>
        Sized
          .max[Latin1Data, Lengths.`127`.type](Latin1Data.fromData(latin1Data.value))
          .fold(failure => Attempt.failure(Err(s"Invalid 127-byte Latin-1 data: $failure")), Attempt.successful),
      latin1Data => Attempt.successful(DionLatin1Data.fromData(latin1Data.data.bytes))
    )

  private def tetraTransactionSignableMessageEncoder(implicit blake2b256: Blake2b256): Encoder[Transaction.Unproven] =
    Encoder(transaction =>
      (
        tetraTransferTypeEncoder.encode(transaction),
        inputsEncoder.encode(Chain.fromSeq(transaction.inputs)),
        timestampEncoder.encode(transaction.timestamp),
        feeEncoder.encode(transaction.fee),
        optionCodec(dataCodec).encode(transaction.data),
        mintingEncoder.encode(transaction.minting)
      ).mapN((txTypeBits, inputsBits, timestampBits, feeBits, dataBits, mintingBits) =>
        outputsEncoder(txTypeBits ++ inputsBits ++ timestampBits ++ feeBits)
          .encode(
            transaction.feeOutput -> transaction.coinOutputs.toChain
          )
          .map(bits => txTypeBits ++ bits ++ inputsBits ++ timestampBits ++ feeBits ++ dataBits ++ mintingBits)
      ).flatten
    )

  implicit def tetraTransactionSignable(implicit blake2b256: Blake2b256): Signable[Transaction.Unproven] =
    Signable.fromScodecEncoder(tetraTransactionSignableMessageEncoder)
}
