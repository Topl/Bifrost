package co.topl.codecs.binary.typeclasses

import cats.data.{Chain, NonEmptyChain}
import cats.implicits._
import co.topl.codecs.binary.scodecs._
import co.topl.codecs.bytes.typeclasses.Signable
import co.topl.crypto.hash.Blake2b256
import co.topl.models.Transaction.{ArbitOutput, AssetOutput, PolyOutput}
import co.topl.models.{Box => TetraBox, _}
import co.topl.models.utility.HasLength.instances.{bigIntLength, bytesLength, latin1DataLength}
import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility.{Lengths, Sized}
import co.topl.modifier.box._
import co.topl.modifier.transaction.{ArbitTransfer, AssetTransfer, PolyTransfer}
import co.topl.utils.StringDataTypes.{Latin1Data => DionLatin1Data}
import co.topl.utils.{Int128 => DionInt128}
import com.google.common.primitives.{Ints, Longs}
import scodec.bits.BitVector
import scodec.interop.cats._
import scodec.{Attempt, Codec, Encoder, Err}
import shapeless.HNil

/**
 * Defines instances of the [[Signable]] typeclass.
 */
object SignableInstances {

  /**
   * Instances of [[Codec]] and [[Encoder]] typeclasses which encode values into their "signable" bits.
   *
   * Intended for usage in defining [[Signable]] instances.
   */
  object SignableCodecs {

    /**
     * Encodes the transfer-type of a [[Transaction.Unproven]].
     */
    val tetraTransferTypeEncoder: Encoder[Transaction.Unproven] =
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

    /**
     * Encoder/decoder for the [[TypedEvidence]] of a [[DionAddress]] in a [[Transaction.Unproven]].
     */
    val typedEvidenceCodec: Codec[TypedEvidence] =
      (byteCodec :: bytesCodec(32))
        .exmap[TypedEvidence](
          bytes =>
            Sized
              .strict[Bytes, Lengths.`32`.type](Bytes(bytes.tail.head))
              .fold(
                error => Attempt.failure(Err(s"Failed to create evidence: $error")),
                evidence => Attempt.successful(TypedEvidence(bytes.head, evidence))
              ),
          typedEvidence => Attempt.successful(typedEvidence.typePrefix :: typedEvidence.evidence.data.toArray :: HNil)
        )

    /**
     * Encoder/decoder for a [[DionAddress]] in a [[Transaction.Unproven]].
     */
    val dionAddressCodec: Codec[DionAddress] =
      (byteCodec.xmap[NetworkPrefix](NetworkPrefix.apply, _.value) :: typedEvidenceCodec).as[DionAddress]

    /**
     * Encoder/decoder for the `shortName` field in a [[TetraBox.Values.Asset.Code]].
     */
    val shortNameCodec: Codec[Sized.Max[Latin1Data, Lengths.`8`.type]] =
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

    /**
     * Encoder/decoder for a [[TetraBox.Values.Asset.Code]] value in a [[TetraBox.Values.Asset]] within a [[Transaction.Unproven]].
     */
    val assetCodeCodec: Codec[TetraBox.Values.Asset.Code] =
      (byteCodec :: dionAddressCodec :: shortNameCodec).as[TetraBox.Values.Asset.Code]

    /**
     * Encoder/decoder for an [[Int128]] value within a [[Transaction.Unproven]].
     */
    val tetraInt128Codec: Codec[Int128] =
      int128Codec
        .exmap(
          int128 =>
            Sized
              .max[BigInt, Lengths.`128`.type](BigInt(int128.toByteArray))
              .fold(failure => Attempt.failure(Err(s"Invalid Int-128 value: $failure")), Attempt.successful),
          int128 => Attempt.successful(DionInt128(int128.data))
        )

    /**
     * Encoder/decoder for the `metadata` field in a [[TetraBox.Values.Asset]] within a [[Transaction.Unproven]].
     */
    val metadataCodec: Codec[Option[Sized.Max[Latin1Data, Lengths.`127`.type]]] =
      optionCodec(
        byteStringCodec.exmap(
          string =>
            Latin1Data
              .validated(string)
              .leftMap(failure => Err(s"Invalid Latin-1 String: $failure"))
              .toEither
              .flatMap(
                Sized
                  .max[Latin1Data, Lengths.`127`.type](_)
                  .leftMap(failure => Err(s"Invalid Latin-1 String: $failure"))
              )
              .fold(Attempt.failure, Attempt.successful),
          latin1Data => Attempt.successful(latin1Data.data.value)
        )
      )

    /**
     * Encoder/decoder for a [[TetraBox.Values.Asset]] within a [[Transaction.Unproven]].
     */
    val assetValueCodec: Codec[TetraBox.Values.Asset] =
      (tetraInt128Codec :: assetCodeCodec :: bytesCodec(SecurityRoot.size).xmap[Bytes](
        bytes => Bytes(bytes),
        bytes => bytes.toArray
      ) :: metadataCodec)
        .as[TetraBox.Values.Asset]

    private def deriveBoxNonce(inputData: BitVector, index: Int)(implicit blake2b256: Blake2b256): BoxNonce =
      Longs.fromByteArray(
        blake2b256
          .hash(
            inputData.toByteVector ++ Bytes(Ints.toByteArray(index))
          )
          .data
          .toArray
          .take(Longs.BYTES)
      )

    /**
     * Determines which outputs will become new boxes in the box state.
     * Only these boxes will be encoded into the signable-message for a [[Transaction.Unproven]].
     * @param feeChange the fee change output
     * @param coinOutputs the collection of coin outputs
     * @return a collection of tuples containing a coin output and its index used for creating the nonce
     */
    private def newBoxOutputs(
      feeChange:   Option[Transaction.PolyOutput],
      coinOutputs: NonEmptyChain[Transaction.CoinOutput]
    ): Chain[(Transaction.CoinOutput, Int)] = {
      // create a chain containing the fee box if it exists and has some positive non-zero value
      val newFeeBox = Chain.fromOption(feeChange.map(_ -> 0).filter(_._1.value.data > 0))

      // create a chain of output boxes where the values are positive and non-zero
      val newCoinBoxes =
        coinOutputs
          .mapWithIndex((output, index) => output -> (index + 1))
          .filter {
            case (Transaction.PolyOutput(_, amount), _)  => amount.data > 0
            case (Transaction.ArbitOutput(_, amount), _) => amount.data > 0
            case (Transaction.AssetOutput(_, asset), _)  => asset.quantity.data > 0
          }

      /*
       whether or not to include the fee box depends on the new coin boxes and the type of transfer (determined
       by the coin type of the first output
       */
      coinOutputs.head match {
        case _: Transaction.PolyOutput                           => newFeeBox ++ newCoinBoxes
        case _: Transaction.ArbitOutput if newCoinBoxes.nonEmpty => newFeeBox ++ newCoinBoxes
        case _: Transaction.ArbitOutput                          => Chain.empty
        case _: Transaction.AssetOutput if newCoinBoxes.nonEmpty => newFeeBox ++ newCoinBoxes
        case _: Transaction.AssetOutput                          => Chain.empty
      }
    }

    /**
     * Encoder for a set of fee-change and transfer outputs in a [[Transaction.Unproven]].
     * @param inputData the collection of unique input bits for the transfer
     * @param blake2b256 an instance of [[Blake2b256]]
     * @return an encoder instance for a set of outputs to their signable data
     */
    def outputsEncoder(
      inputData: BitVector
    )(implicit
      blake2b256: Blake2b256
    ): Encoder[(Option[Transaction.PolyOutput], NonEmptyChain[Transaction.CoinOutput])] =
      Encoder(outputs =>
        newBoxOutputs(outputs._1, outputs._2)
          .foldMapM {
            case (Transaction.PolyOutput(address, amount), index) =>
              for {
                typeBits     <- BitVector(PolyBox.typePrefix).pure[Attempt]
                evidenceBits <- typedEvidenceCodec.encode(address.typedEvidence)
                nonceBits    <- longCodec.encode(deriveBoxNonce(inputData, index))
                valueBits    <- tetraInt128Codec.encode(amount)
              } yield typeBits ++ evidenceBits ++ nonceBits ++ valueBits
            case (Transaction.ArbitOutput(address, amount), index) =>
              for {
                typeBits     <- BitVector(ArbitBox.typePrefix).pure[Attempt]
                evidenceBits <- typedEvidenceCodec.encode(address.typedEvidence)
                nonceBits    <- longCodec.encode(deriveBoxNonce(inputData, index))
                valueBits    <- tetraInt128Codec.encode(amount)
              } yield typeBits ++ evidenceBits ++ nonceBits ++ valueBits
            case (Transaction.AssetOutput(address, asset), index) =>
              for {
                typeBits     <- BitVector(AssetBox.typePrefix).pure[Attempt]
                evidenceBits <- typedEvidenceCodec.encode(address.typedEvidence)
                nonceBits    <- longCodec.encode(deriveBoxNonce(inputData, index))
                valueBits    <- assetValueCodec.encode(asset)
              } yield typeBits ++ evidenceBits ++ nonceBits ++ valueBits
          }
      )

    /**
     * Encoder for a set of [[BoxReference]] values provided as inputs to a [[Transaction.Unproven]].
     * @param blake2b256 an instance of [[Blake2b256]]
     * @return an encoder for a set of [[BoxReference]] inputs to a [[Transaction.Unproven]]
     */
    def inputsEncoder(implicit blake2b256: Blake2b256): Encoder[Chain[BoxReference]] =
      Encoder(inputs =>
        inputs.foldMapM(input =>
          typedEvidenceCodec
            .encode(input._1.typedEvidence)
            .map(_.toByteVector)
            .map(evidenceBytes => blake2b256.hash(evidenceBytes ++ Bytes(Longs.toByteArray(input._2))))
            .map(_.data.toBitVector)
        )
      )

    /**
     * Encoder/decoder for [[TransactionData]] within a [[Transaction.Unproven]].
     */
    val dataCodec: Codec[TransactionData] =
      latin1DataCodec.exmap(
        latin1Data =>
          Sized
            .max[Latin1Data, Lengths.`127`.type](Latin1Data.fromData(latin1Data.value))
            .fold(failure => Attempt.failure(Err(s"Invalid 127-byte Latin-1 data: $failure")), Attempt.successful),
        latin1Data => Attempt.successful(DionLatin1Data.fromData(latin1Data.data.bytes))
      )

    /**
     * Encoder for generating a signable-message from a [[Transaction.Unproven]].
     * @param blake2b256 instance of [[Blake2b256]]
     * @return an encoder to generate the signable-message of a [[Transaction.Unproven]]
     */
    def tetraTransactionEncoder(implicit blake2b256: Blake2b256): Encoder[Transaction.Unproven] =
      Encoder(transaction =>
        (
          tetraTransferTypeEncoder.encode(transaction),
          inputsEncoder.encode(Chain.fromSeq(transaction.inputs)),
          tetraInt128Codec.encode(transaction.fee),
          optionCodec(dataCodec).encode(transaction.data),
          boolCodec.encode(transaction.minting)
        ).mapN { (txTypeBits, inputsBits, feeBits, dataBits, mintingBits) =>
          val timestampBits = BitVector(Longs.toByteArray(transaction.timestamp))

          // use the TX-Type, Input Boxes, Timestamp, and Fee bits to define the output bits
          outputsEncoder(txTypeBits ++ inputsBits ++ timestampBits ++ feeBits)
            .encode(
              transaction.feeOutput -> transaction.coinOutputs
            )
            // create the signable-message
            .map { bits =>
              txTypeBits ++ bits ++ inputsBits ++ timestampBits ++ feeBits ++ dataBits ++ mintingBits
            }
        }.flatten
      )
  }

  /**
   * Instances of the [[Signable]] typeclass for generating messages that can be used to create proofs for transactions
   * and blocks.
   */
  trait Instances {

    /**
     * [[Signable]] instance for a [[Transaction.Unproven]].
     *
     * @param blake2b256 an instance of [[Blake2b256]]
     * @return a [[Signable]] instance for generating signable-messages from a [[Transaction.Unproven]] value
     */
    implicit def unprovenTransactionSignable(implicit blake2b256: Blake2b256): Signable[Transaction.Unproven] =
      Signable.fromScodecEncoder(SignableCodecs.tetraTransactionEncoder)
  }

  trait Implicits extends Instances

  object implicits extends Implicits

}
