package co.topl.codecs.binary.scodecs.tetra

import cats.data.NonEmptyChain
import co.topl.attestation.{
  Evidence => DionEvidence,
  PublicKeyPropositionCurve25519,
  PublicKeyPropositionEd25519,
  ThresholdPropositionCurve25519
}
import co.topl.codecs.binary.scodecs.valuetypes._
import co.topl.crypto.signing
import co.topl.models._
import co.topl.models.utility.HasLength.instances.{bigIntLength, bytesLength, latin1DataLength}
import co.topl.models.utility.Sized
import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.modifier.box.SimpleValue
import co.topl.modifier.transaction.PolyTransfer
import co.topl.typeclasses.implicits._
import co.topl.utils.Extensions.LongOps
import co.topl.utils.{Int128 => DionInt128, StringDataTypes}
import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs.discriminated
import shapeless.{::, HNil}

import scala.collection.immutable.{ListMap, ListSet}

trait TetraCodecs {

  private val typedEvidenceCodec: Codec[TypedEvidence] =
    bytesCodec(DionEvidence.size)
      .xmapc(bytes => TypedEvidence(bytes.head, Sized.strictUnsafe(ByteVector(bytes.tail))))(typedEvidence =>
        typedEvidence.allBytes.toArray
      )

  private val networkPrefixCodec: Codec[NetworkPrefix] =
    byteCodec.xmap[NetworkPrefix](byte => NetworkPrefix(byte), prefix => prefix.value)

  private val dionAddressCodec: Codec[DionAddress] =
    (networkPrefixCodec :: typedEvidenceCodec).as[DionAddress]

  private val curvePropositionCodec: Codec[Propositions.Knowledge.Curve25519] =
    bytesCodec(signing.Curve25519.instance.KeyLength)
      .xmap(
        bytes => Propositions.Knowledge.Curve25519(VerificationKeys.Curve25519(Sized.strictUnsafe(ByteVector(bytes)))),
        curve => curve.key.bytes.data.toArray
      )

  private val edPropositionCodec: Codec[Propositions.Knowledge.Ed25519] =
    bytesCodec(signing.Ed25519.instance.KeyLength)
      .xmap(
        bytes => Propositions.Knowledge.Ed25519(VerificationKeys.Ed25519(Sized.strictUnsafe(ByteVector(bytes)))),
        ed => ed.key.bytes.data.toArray
      )

  private def listSetCodec[T: Codec]: Codec[ListSet[T]] =
    listCodec[T].xmap(list => ListSet.from(list), listSet => listSet.toList)

  private val thresholdPropositionCodec: Codec[Propositions.Compositional.Threshold] =
    (uIntCodec :: listSetCodec(curvePropositionCodec))
      .xmapc { case uInt :: curveProps :: HNil =>
        Propositions.Compositional.Threshold(uInt.toIntExact, curveProps.map[Proposition](x => x))
      }(threshold =>
        threshold.threshold :: threshold.propositions.map(_.asInstanceOf[Propositions.Knowledge.Curve25519]) :: HNil
      )

  private val propositionCodec: Codec[Proposition] =
    discriminated[Proposition]
      .by(byteCodec)
      .typecase(PublicKeyPropositionCurve25519.typePrefix, curvePropositionCodec)
      .typecase(PublicKeyPropositionEd25519.typePrefix, edPropositionCodec)
      .typecase(ThresholdPropositionCurve25519.typePrefix, thresholdPropositionCodec)

  private val curveProofCodec: Codec[Proofs.Knowledge.Curve25519] =
    bytesCodec(signing.Curve25519.instance.SignatureLength)
      .xmap(
        bytes => Proofs.Knowledge.Curve25519(Sized.strictUnsafe(ByteVector(bytes))),
        proof => proof.bytes.data.toArray
      )

  private val edProofCodec: Codec[Proofs.Knowledge.Ed25519] =
    bytesCodec(signing.Ed25519.instance.SignatureLength)
      .xmap(
        bytes => Proofs.Knowledge.Ed25519(Sized.strictUnsafe(ByteVector(bytes))),
        proof => proof.bytes.data.toArray
      )

  private val thresholdProofCodec: Codec[Proofs.Compositional.Threshold] =
    listCodec(curveProofCodec)
      .xmap[List[Proof]](
        curveProofs => curveProofs.map(c => c),
        proofs => proofs.map(p => p.asInstanceOf[Proofs.Knowledge.Curve25519])
      )
      .as[Proofs.Compositional.Threshold]

  private val proofCodec: Codec[Proof] =
    discriminated[Proof]
      .by(byteCodec)
      .typecase(PublicKeyPropositionCurve25519.typePrefix, curveProofCodec)
      .typecase(PublicKeyPropositionEd25519.typePrefix, edProofCodec)
      .typecase(ThresholdPropositionCurve25519.typePrefix, thresholdProofCodec)

  private val tetraInt128Codec: Codec[Int128] =
    int128Codec
      .xmap[Int128](int128 => Sized.maxUnsafe(BigInt(int128.toLong)), int128 => DionInt128(int128.data))

  private val simpleValueCompatibleCodec: Codec[Byte :: Int128 :: HNil] =
    byteCodec :: tetraInt128Codec

  private val polyTransferCurveTransactionCodec: Codec[Transaction] =
    (listCodec(tupleCodec(dionAddressCodec, longCodec)) ::
      listCodec(
        tupleCodec(
          dionAddressCodec,
          simpleValueCompatibleCodec
        )
      ) ::
      listMapCodec(propositionCodec, proofCodec) ::
      tetraInt128Codec ::
      uLongCodec ::
      optionCodec(latin1DataCodec) ::
      boolCodec)
      .xmapc[Transaction] { case inputs :: to :: attestation :: fee :: timestamp :: data :: minting :: HNil =>
        Transaction(
          mergeBoxesWithAttestation(inputs, attestation),
          to.head match {
            case (address, _ :: quantity :: HNil) =>
              Option.when(quantity.data > 0)(Transaction.PolyOutput(address, quantity))
          },
          NonEmptyChain.fromSeq(to.tail).get.map[Transaction.CoinOutput] { case (address, _ :: quantity :: HNil) =>
            Transaction.PolyOutput(address, quantity)
          },
          fee,
          timestamp,
          data.map(d => Sized.maxUnsafe(Latin1Data.fromData(d.value))),
          minting
        )
      }(transaction =>
        List.from(transaction.inputs.keys) :: transaction.feeOutput
          .fold(transaction.coinOutputs)(feeOutput => transaction.coinOutputs.prepend(feeOutput))
          .map(coin =>
            coin
              .asInstanceOf[Transaction.PolyOutput]
              .dionAddress -> (SimpleValue.valueTypePrefix :: coin.asInstanceOf[Transaction.PolyOutput].value :: HNil)
          )
          .toNonEmptyList
          .toList :: ListMap.from(
          List.from(transaction.inputs.values).distinct
        ) :: transaction.fee :: transaction.timestamp :: transaction.data.map(data =>
          StringDataTypes.Latin1Data.fromData(data.data.bytes)
        ) :: transaction.minting :: HNil
      )

  private def mergeBoxesWithAttestation(
    boxes:       List[BoxReference],
    attestation: ListMap[Proposition, Proof]
  ): ListMap[BoxReference, (Proposition, Proof)] =
    attestation.foldLeft(ListMap.empty[BoxReference, (Proposition, Proof)]) { case (result, pair) =>
      result ++ boxes.filter(_._1.typedEvidence == pair._1.typedEvidence).map(_ -> pair)
    }

//  private val polyTransferEdTransactionCodec: Codec[Transaction] = ???
//
//  private val polyTransferThresholdTransactionCodec: Codec[Transaction] = ???

  private val polyTransferTransactionCodec: Codec[Transaction] =
    discriminated[Transaction]
      .by(byteCodec)
      .|(PublicKeyPropositionCurve25519.typePrefix) {
        case tx @ Transaction(inputs, _, _, _, _, _, _)
            if inputs.exists(p => p._2._1.isInstanceOf[Propositions.Knowledge.Curve25519]) =>
          tx
      }(tx => tx)(polyTransferCurveTransactionCodec)
//      .|(PublicKeyPropositionEd25519.typePrefix) {
//        case tx @ Transaction(inputs, _, _, _, _, _, _)
//            if inputs.exists(p => p._2._1.isInstanceOf[Propositions.Knowledge.Ed25519]) =>
//          tx
//      }(tx => tx)(polyTransferEdTransactionCodec)
//      .|(ThresholdPropositionCurve25519.typePrefix) {
//        case tx @ Transaction(inputs, _, _, _, _, _, _)
//            if inputs.exists(p => p._2._1.isInstanceOf[Propositions.Compositional.Threshold]) =>
//          tx
//      }(tx => tx)(polyTransferThresholdTransactionCodec)

//  private val arbitTransferTransactionCodec: Codec[Transaction] = ???
//
//  private val assetTransferTransactionCodec: Codec[Transaction] = ???

  implicit val transactionCodec: Codec[Transaction] =
    discriminated[Transaction]
      .by(byteCodec)
      .|(PolyTransfer.typePrefix) {
        case tx @ Transaction(_, _, outputs, _, _, _, _) if outputs.head.isInstanceOf[Transaction.PolyOutput] =>
          tx
      }(tx => tx)(polyTransferTransactionCodec)
//      .|(ArbitTransfer.typePrefix) {
//        case tx @ Transaction(_, _, outputs, _, _, _, _) if outputs.head.isInstanceOf[Transaction.ArbitOutput] =>
//          tx
//      }(tx => tx)(arbitTransferTransactionCodec)
//      .|(AssetTransfer.typePrefix) {
//        case tx @ Transaction(_, _, outputs, _, _, _, _) if outputs.head.isInstanceOf[Transaction.AssetOutput] =>
//          tx
//      }(tx => tx)(assetTransferTransactionCodec)

}
