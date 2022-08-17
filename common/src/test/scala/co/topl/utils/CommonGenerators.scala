package co.topl.utils

import co.topl.attestation.PublicKeyPropositionCurve25519.evProducer
import co.topl.attestation._
import co.topl.attestation.keyManagement._
import co.topl.crypto.hash.blake2b256
import co.topl.crypto.hash.digest.Digest32
import co.topl.crypto.mnemonic.Entropy
import co.topl.crypto.signing.{Curve25519, Ed25519, EntropyToSeed, Password}
import co.topl.crypto.{PrivateKey, PublicKey, Signature}
import co.topl.models.Bytes
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.{Lengths, Sized}
import co.topl.modifier.{ModifierId, ProgramId}
import co.topl.modifier.block.{Block, BloomFilter}
import co.topl.modifier.block.PersistentNodeViewModifier.PNVMVersion
import co.topl.modifier.box.Box.Nonce
import co.topl.modifier.box._
import co.topl.modifier.transaction._
import co.topl.utils.StringDataTypes.Latin1Data
import io.circe.Json
import io.circe.syntax._
import org.scalacheck.rng.Seed
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.Suite

import java.nio.charset.StandardCharsets
import scala.collection.SortedSet
import scala.collection.immutable.ListMap
import scala.util.Random

/**
 * Created by cykoz on 4/12/17.
 */
trait CommonGenerators extends Logging with NetworkPrefixTestHelper {

  type P = Proposition
  type S = Secret

  def sampleUntilNonEmpty[T](generator: Gen[T]): T =
    generator.pureApply(Gen.Parameters.default, Seed.random())

  lazy val stringGen: Gen[String] = Gen.alphaNumStr.suchThat(_.nonEmpty)

  lazy val dataStringGen: Gen[String] = Gen.alphaNumStr.suchThat(data => data.length <= 127 && data.nonEmpty)

  lazy val latin1DataGen: Gen[Latin1Data] = dataStringGen.map(Latin1Data.unsafe)

  lazy val shortNameGen: Gen[Latin1Data] = for {
    n   <- Gen.choose(0, AssetCode.shortNameLimit)
    str <- Gen.listOfN(n, Gen.alphaNumChar).map(_.mkString)
  } yield Latin1Data.unsafe(str)

  val jsonTypes: Seq[String] = Seq("Object", "Array", "Boolean", "String", "Number")

  lazy val jsonTypeGen: Gen[String] = Gen.oneOf(jsonTypes)

  private val booleanGen: Gen[Boolean] = Gen.oneOf(Seq(true, false))

  def jsonGen(depth: Int = 0): Gen[Json] = for {
    numFields <- positiveTinyIntGen
  } yield (0 until numFields)
    .map { _ =>
      sampleUntilNonEmpty(stringGen) -> (sampleUntilNonEmpty(jsonTypeGen) match {
        case "Object" if depth < 2 => sampleUntilNonEmpty(jsonGen(depth + 1))
        case "Array" if depth < 3  => sampleUntilNonEmpty(jsonArrayGen(depth + 1))
        case "Boolean"             => sampleUntilNonEmpty(booleanGen).asJson
        case "String"              => sampleUntilNonEmpty(stringGen).asJson
        case "Number"              => sampleUntilNonEmpty(positiveDoubleGen).asJson
        case _                     => sampleUntilNonEmpty(stringGen).asJson
      })
    }
    .toMap
    .asJson

  def jsonArrayGen(depth: Int = 0): Gen[Json] = for {
    numFields <- positiveTinyIntGen
  } yield ((0 until numFields) map { _ =>
    sampleUntilNonEmpty(jsonTypeGen) match {
      case "Object" if depth < 2 => sampleUntilNonEmpty(jsonGen(depth + 1))
      case "Array" if depth < 3  => sampleUntilNonEmpty(jsonArrayGen(depth + 1))
      case "Boolean"             => sampleUntilNonEmpty(booleanGen).asJson
      case "String"              => sampleUntilNonEmpty(stringGen).asJson
      case "Number"              => sampleUntilNonEmpty(positiveDoubleGen).asJson
      case _                     => sampleUntilNonEmpty(stringGen).asJson
    }
  }).asJson

  private lazy val intMin = 1
  private lazy val tinyIntMax = 10
  private lazy val medIntMax = 100

  private lazy val int128Min: Int128 = Int128.MinValue
  private lazy val int128Max: Int128 = Int128.MaxValue

  implicit lazy val int128Chooser: Gen.Choose[Int128] =
    (min, max) => Gen.Choose.chooseBigInt.choose(min.bigInt, max.bigInt).map(Int128(_))

  lazy val positiveTinyIntGen: Gen[Int] = Gen.choose(intMin, tinyIntMax)
  lazy val positiveMediumIntGen: Gen[Int] = Gen.choose(intMin, medIntMax)
  lazy val positiveThresholdIntGen: Gen[Int] = Gen.choose(4, 20) // need numKeys/2 greater than 1 for tests

  lazy val positiveDoubleGen: Gen[Double] = Gen.choose(0, Double.MaxValue)

  lazy val positiveInt128Gen: Gen[Int128] = Gen.choose[Int128](0, int128Max)
  lazy val smallInt128Gen: Gen[Int128] = Gen.choose[Int128](0, Int.MaxValue)
  lazy val largeInt128Gen: Gen[Int128] = Gen.choose[Int128](Long.MaxValue, int128Max)

  def samplePositiveDouble: Double = Random.nextFloat()

  lazy val tokenBoxesGen: Gen[Seq[TokenBox[TokenValueHolder]]] = for {
    tx <- Gen.someOf(polyBoxGen, arbitBoxGen, assetBoxGen)
  } yield tx

  lazy val polyBoxCurve25519Gen: Gen[PolyBox] = for {
    evidence <- evidenceCurve25519Gen
    nonce    <- positiveLongGen
    value    <- positiveLongGen
  } yield PolyBox(evidence, nonce, SimpleValue(value))

  lazy val polyBoxEd25519Gen: Gen[PolyBox] = for {
    evidence <- evidenceEd25519Gen
    nonce    <- positiveLongGen
    value    <- positiveLongGen
  } yield PolyBox(evidence, nonce, SimpleValue(value))

  lazy val polyBoxGen: Gen[PolyBox] = Gen.oneOf(polyBoxCurve25519Gen, polyBoxEd25519Gen)

  lazy val arbitBoxCurve25519Gen: Gen[ArbitBox] = for {
    evidence <- evidenceCurve25519Gen
    nonce    <- positiveLongGen
    value    <- positiveLongGen
  } yield ArbitBox(evidence, nonce, SimpleValue(value))

  lazy val arbitBoxEd25519Gen: Gen[ArbitBox] = for {
    evidence <- evidenceEd25519Gen
    nonce    <- positiveLongGen
    value    <- positiveLongGen
  } yield ArbitBox(evidence, nonce, SimpleValue(value))

  lazy val arbitBoxGen: Gen[ArbitBox] = Gen.oneOf(arbitBoxCurve25519Gen, arbitBoxEd25519Gen)

  lazy val assetBoxCurve25519Gen: Gen[AssetBox] = for {
    evidence <- evidenceCurve25519Gen
    nonce    <- positiveLongGen
    quantity <- positiveLongGen
    // TODO: Hard coded as 1, but change this to arbitrary in the future
    // assetVersion <- Arbitrary.arbitrary[Byte]
    shortName <- shortNameGen
    issuer    <- addressGen
    data      <- latin1DataGen
  } yield {
    // TODO: Hard coded as 1, but change this to arbitrary in the future
    val assetVersion = 1: Byte
    val assetCode = AssetCode(assetVersion, issuer, shortName)
    val value = AssetValue(quantity, assetCode, metadata = Some(data))
    AssetBox(evidence, nonce, value)
  }

  lazy val assetBoxEd25519Gen: Gen[AssetBox] = for {
    evidence <- evidenceEd25519Gen
    nonce    <- positiveLongGen
    quantity <- positiveLongGen
    // TODO: Hard coded as 1, but change this to arbitrary in the future
    // assetVersion <- Arbitrary.arbitrary[Byte]
    shortName <- shortNameGen
    issuer    <- addressGen
    data      <- latin1DataGen
  } yield {
    // TODO: Hard coded as 1, but change this to arbitrary in the future
    val assetVersion = 1: Byte
    val assetCode = AssetCode(assetVersion, issuer, shortName)
    val value = AssetValue(quantity, assetCode, metadata = Some(data))
    AssetBox(evidence, nonce, value)
  }

  lazy val assetBoxGen: Gen[AssetBox] = Gen.oneOf(assetBoxCurve25519Gen, assetBoxEd25519Gen)

  lazy val programIdGen: Gen[ProgramId] = for {
    seed <- specificLengthBytesGen(ProgramId.size)
  } yield ProgramId.create(seed)

  def preFeeBoxGen(minFee: Long = 0, maxFee: Long = Long.MaxValue): Gen[(Nonce, Long)] = for {
    nonce  <- Gen.choose(Long.MinValue, Long.MaxValue)
    amount <- Gen.choose(minFee, maxFee)
  } yield (nonce, amount)

  lazy val fromCurve25519Gen: Gen[(Address, Nonce)] = for {
    address <- addressCurve25519Gen
    nonce   <- positiveLongGen
  } yield (address, nonce)

  lazy val fromEd25519Gen: Gen[(Address, Nonce)] = for {
    address <- addressEd25519Gen
    nonce   <- positiveLongGen
  } yield (address, nonce)

  lazy val fromGen: Gen[(Address, Nonce)] = Gen.oneOf(fromCurve25519Gen, fromEd25519Gen)

  lazy val fromSeqCurve25519Gen: Gen[IndexedSeq[(Address, Nonce)]] = for {
    seqLen <- positiveTinyIntGen
  } yield (0 until seqLen) map { _ => sampleUntilNonEmpty(fromCurve25519Gen) }

  lazy val fromSeqEd25519Gen: Gen[IndexedSeq[(Address, Nonce)]] = for {
    seqLen <- positiveTinyIntGen
  } yield (0 until seqLen) map { _ => sampleUntilNonEmpty(fromEd25519Gen) }

  lazy val fromSeqGen: Gen[IndexedSeq[(Address, Nonce)]] = Gen.oneOf(fromSeqCurve25519Gen, fromSeqEd25519Gen)

  lazy val simpleValueGen: Gen[SimpleValue] = for {
    value <- positiveLongGen
  } yield SimpleValue(value)

  lazy val toCurve25519Gen: Gen[(Address, SimpleValue)] = for {
    address <- addressCurve25519Gen
    value   <- positiveLongGen
  } yield (address, SimpleValue(value))

  lazy val toEd25519Gen: Gen[(Address, SimpleValue)] = for {
    address <- addressEd25519Gen
    value   <- positiveLongGen
  } yield (address, SimpleValue(value))

  lazy val toGen: Gen[(Address, SimpleValue)] = Gen.oneOf(toCurve25519Gen, toEd25519Gen)

  lazy val toSeqCurve25519Gen: Gen[IndexedSeq[(Address, SimpleValue)]] = for {
    seqLen <- positiveTinyIntGen
  } yield (0 until seqLen) map { _ => sampleUntilNonEmpty(toCurve25519Gen) }

  lazy val toSeqEd25519Gen: Gen[IndexedSeq[(Address, SimpleValue)]] = for {
    seqLen <- positiveTinyIntGen
  } yield (0 until seqLen) map { _ => sampleUntilNonEmpty(toEd25519Gen) }

  lazy val toSeqGen: Gen[IndexedSeq[(Address, SimpleValue)]] = Gen.oneOf(toSeqCurve25519Gen, toSeqEd25519Gen)

  lazy val assetCodeCurve25519Gen: Gen[AssetCode] = for {
    // TODO: Hard coded as 1, but change this to arbitrary in the future
    // assetVersion <- Arbitrary.arbitrary[Byte]
    issuer    <- addressCurve25519Gen
    shortName <- shortNameGen
  } yield AssetCode(1: Byte, issuer, shortName)

  lazy val assetCodeEd25519Gen: Gen[AssetCode] = for {
    // TODO: Hard coded as 1, but change this to arbitrary in the future
    // assetVersion <- Arbitrary.arbitrary[Byte]
    issuer    <- addressEd25519Gen
    shortName <- shortNameGen
  } yield AssetCode(1: Byte, issuer, shortName)

  lazy val assetCodeGen: Gen[AssetCode] = Gen.oneOf(assetCodeCurve25519Gen, assetCodeEd25519Gen)

  lazy val assetValueCurve25519Gen: Gen[AssetValue] = for {
    quantity  <- positiveLongGen
    assetCode <- assetCodeCurve25519Gen
    data      <- latin1DataGen
  } yield AssetValue(quantity, assetCode, metadata = Some(data))

  lazy val assetValueEd25519Gen: Gen[AssetValue] = for {
    quantity  <- positiveLongGen
    assetCode <- assetCodeEd25519Gen
    data      <- latin1DataGen
  } yield AssetValue(quantity, assetCode, metadata = Some(data))

  lazy val assetValueGen: Gen[AssetValue] = Gen.oneOf(assetValueCurve25519Gen, assetValueEd25519Gen)

  lazy val assetToCurve25519Gen: Gen[(Address, AssetValue)] = for {
    assetValue <- assetValueCurve25519Gen
  } yield (assetValue.assetCode.issuer, assetValue)

  lazy val assetToEd25519Gen: Gen[(Address, AssetValue)] = for {
    assetValue <- assetValueEd25519Gen
  } yield (assetValue.assetCode.issuer, assetValue)

  lazy val assetToGen: Gen[(Address, AssetValue)] = Gen.oneOf(assetToCurve25519Gen, assetToEd25519Gen)

  lazy val assetToSeqCurve25519Gen: Gen[IndexedSeq[(Address, TokenValueHolder)]] = for {
    seqLen <- positiveTinyIntGen
  } yield (0 until seqLen) map { _ => sampleUntilNonEmpty(assetToCurve25519Gen) }

  lazy val assetToSeqEd25519Gen: Gen[IndexedSeq[(Address, TokenValueHolder)]] = for {
    seqLen <- positiveTinyIntGen
  } yield (0 until seqLen) map { _ => sampleUntilNonEmpty(assetToEd25519Gen) }

  lazy val assetToSeqGen: Gen[IndexedSeq[(Address, TokenValueHolder)]] =
    Gen.oneOf(assetToSeqCurve25519Gen, assetToSeqEd25519Gen)

  lazy val securityRootGen: Gen[SecurityRoot] = for {
    root <- specificLengthBytesGen(Digest32.size)
  } yield SecurityRoot(root)

  lazy val sigSeqCurve25519Gen: Gen[IndexedSeq[SignatureCurve25519]] = for {
    seqLen <- positiveTinyIntGen
  } yield (0 until seqLen) map { _ => sampleUntilNonEmpty(signatureCurve25519Gen) }

  lazy val sigSeqEd25519Gen: Gen[IndexedSeq[SignatureEd25519]] = for {
    seqLen <- positiveTinyIntGen
  } yield (0 until seqLen) map { _ => sampleUntilNonEmpty(signatureEd25519Gen) }

  lazy val sigSeqGen: Gen[IndexedSeq[_ <: Proof[_]]] = Gen.oneOf(sigSeqCurve25519Gen, sigSeqEd25519Gen)

  lazy val polyTransferCurve25519Gen: Gen[PolyTransfer[PublicKeyPropositionCurve25519]] = for {
    from        <- fromSeqCurve25519Gen
    to          <- toSeqGen
    attestation <- attestationCurve25519Gen
    fee         <- positiveLongGen
    timestamp   <- positiveLongGen
    data        <- latin1DataGen
  } yield PolyTransfer(from, to, attestation, fee, timestamp, Some(data), minting = false)

  lazy val polyTransferThresholdCurve25519Gen: Gen[PolyTransfer[ThresholdPropositionCurve25519]] = for {
    from        <- fromSeqCurve25519Gen
    to          <- toSeqGen
    attestation <- attestationThresholdCurve25519Gen
    fee         <- positiveLongGen
    timestamp   <- positiveLongGen
    data        <- latin1DataGen
  } yield PolyTransfer(from, to, attestation, fee, timestamp, Some(data), minting = false)

  def signedPolyTransferGen(
    from:    Gen[IndexedSeq[(Address, Nonce)]],
    keyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519]
  ): Gen[PolyTransfer[keyRing.PK]] = for {
    from      <- from
    to        <- toSeqGen
    fee       <- positiveLongGen
    timestamp <- positiveLongGen
    data      <- latin1DataGen
  } yield {
    val base =
      PolyTransfer[PublicKeyPropositionCurve25519](from, to, ListMap.empty, fee, timestamp, Some(data), minting = false)

    base.copy(attestation = keyRing.generateAttestation(from.map(_._1).toSet)(base.messageToSign))
  }

  lazy val polyTransferEd25519Gen: Gen[PolyTransfer[PublicKeyPropositionEd25519]] = for {
    from        <- fromSeqEd25519Gen
    to          <- toSeqGen
    attestation <- attestationEd25519Gen
    fee         <- positiveLongGen
    timestamp   <- positiveLongGen
    data        <- latin1DataGen
  } yield PolyTransfer(from, to, attestation, fee, timestamp, Some(data), minting = false)

  lazy val polyTransferGen: Gen[PolyTransfer[_ <: Proposition]] =
    Gen.oneOf(polyTransferCurve25519Gen, polyTransferThresholdCurve25519Gen, polyTransferEd25519Gen)

  lazy val arbitTransferCurve25519Gen: Gen[ArbitTransfer[PublicKeyPropositionCurve25519]] = for {
    from        <- fromSeqCurve25519Gen
    to          <- toSeqGen
    attestation <- attestationCurve25519Gen
    fee         <- positiveLongGen
    timestamp   <- positiveLongGen
    data        <- latin1DataGen
  } yield ArbitTransfer(from, to, attestation, fee, timestamp, Some(data), minting = false)

  lazy val arbitTransferThresholdCurve25519Gen: Gen[ArbitTransfer[ThresholdPropositionCurve25519]] = for {
    from        <- fromSeqCurve25519Gen
    to          <- toSeqGen
    attestation <- attestationThresholdCurve25519Gen
    fee         <- positiveLongGen
    timestamp   <- positiveLongGen
    data        <- latin1DataGen
  } yield ArbitTransfer(from, to, attestation, fee, timestamp, Some(data), minting = false)

  lazy val arbitTransferEd25519Gen: Gen[ArbitTransfer[PublicKeyPropositionEd25519]] = for {
    from        <- fromSeqEd25519Gen
    to          <- toSeqGen
    attestation <- attestationEd25519Gen
    fee         <- positiveLongGen
    timestamp   <- positiveLongGen
    data        <- latin1DataGen
  } yield ArbitTransfer(from, to, attestation, fee, timestamp, Some(data), minting = false)

  lazy val arbitTransferGen: Gen[ArbitTransfer[_ <: Proposition]] =
    Gen.oneOf(arbitTransferCurve25519Gen, arbitTransferThresholdCurve25519Gen, arbitTransferEd25519Gen)

  lazy val assetTransferCurve25519Gen: Gen[AssetTransfer[PublicKeyPropositionCurve25519]] = for {
    from        <- fromSeqCurve25519Gen
    to          <- assetToSeqGen
    attestation <- attestationCurve25519Gen
    fee         <- positiveLongGen
    timestamp   <- positiveLongGen
    data        <- latin1DataGen
  } yield AssetTransfer(from, to, attestation, fee, timestamp, Some(data), minting = true)

  lazy val assetTransferThresholdCurve25519Gen: Gen[AssetTransfer[ThresholdPropositionCurve25519]] = for {
    from        <- fromSeqCurve25519Gen
    to          <- assetToSeqGen // TODO: Jing - Does this need to use specific signature scheme?
    attestation <- attestationThresholdCurve25519Gen
    fee         <- positiveLongGen
    timestamp   <- positiveLongGen
    data        <- latin1DataGen
  } yield AssetTransfer(from, to, attestation, fee, timestamp, Some(data), minting = true)

  lazy val assetTransferEd25519Gen: Gen[AssetTransfer[PublicKeyPropositionEd25519]] = for {
    from        <- fromSeqEd25519Gen
    to          <- assetToSeqGen
    attestation <- attestationEd25519Gen
    fee         <- positiveLongGen
    timestamp   <- positiveLongGen
    data        <- latin1DataGen
  } yield AssetTransfer(from, to, attestation, fee, timestamp, Some(data), minting = true)

  lazy val assetTransferGen: Gen[AssetTransfer[_ <: Proposition]] =
    Gen.oneOf(assetTransferCurve25519Gen, assetTransferThresholdCurve25519Gen, assetTransferEd25519Gen)

  lazy val transferGen: Gen[Transaction.TX] =
    Gen.oneOf(
      polyTransferGen.map { t: Transaction.TX => t },
      arbitTransferGen.map { t: Transaction.TX => t },
      assetTransferGen.map { t: Transaction.TX => t }
    )

  lazy val propTypes: Gen[String] = sampleUntilNonEmpty(
    Gen.oneOf(
      PublicKeyPropositionCurve25519.typeString,
      ThresholdPropositionCurve25519.typeString,
      PublicKeyPropositionEd25519.typeString
    )
  )

  lazy val keyPairGen: Gen[(Set[_ <: Secret], _ <: KnowledgeProposition[_ <: Secret])] = for {
    propType <- propTypes
  } yield propType match {
    case PublicKeyPropositionCurve25519.typeString =>
      val key = sampleUntilNonEmpty(publicKeyPropositionCurve25519Gen)
      Set(key._1) -> key._2
    case ThresholdPropositionCurve25519.typeString =>
      sampleUntilNonEmpty(thresholdPropositionCurve25519Gen)
    case PublicKeyPropositionEd25519.typeString =>
      val key = sampleUntilNonEmpty(publicKeyPropositionEd25519Gen)
      Set(key._1) -> key._2
  }

  lazy val keyPairSetCurve25519Gen: Gen[Set[(PrivateKeyCurve25519, PublicKeyPropositionCurve25519)]] = for {
    seqLen <- positiveTinyIntGen
  } yield ((0 until seqLen) map { _ => sampleUntilNonEmpty(keyCurve25519FastGen) }).toSet

  lazy val attestationCurve25519Gen
    : Gen[ListMap[PublicKeyPropositionCurve25519, Proof[PublicKeyPropositionCurve25519]]] =
    for {
      prop <- propositionCurve25519Gen
      sig  <- signatureCurve25519Gen
    } yield ListMap(prop -> sig)

  lazy val attestationThresholdCurve25519Gen
    : Gen[ListMap[ThresholdPropositionCurve25519, Proof[ThresholdPropositionCurve25519]]] =
    for {
      prop <- thresholdPropositionCurve25519Gen
      sig  <- thresholdSignatureCurve25519Gen
    } yield ListMap(prop._2 -> sig)

  lazy val attestationEd25519Gen: Gen[ListMap[PublicKeyPropositionEd25519, Proof[PublicKeyPropositionEd25519]]] = for {
    prop <- propositionEd25519Gen
    sig  <- signatureEd25519Gen
  } yield ListMap(prop -> sig)

  lazy val attestationGen: Gen[Map[_ <: Proposition, Proof[_ <: Proposition]]] =
    Gen.oneOf(attestationCurve25519Gen, attestationThresholdCurve25519Gen, attestationEd25519Gen)

  lazy val transactionTypes: Seq[Gen[TransferTransaction[_ <: TokenValueHolder, _ <: Proposition]]] =
    Seq(
      polyTransferGen.map { t: TransferTransaction[_ <: TokenValueHolder, _ <: Proposition] => t },
      arbitTransferGen.map { t: TransferTransaction[_ <: TokenValueHolder, _ <: Proposition] => t },
      assetTransferGen.map { t: TransferTransaction[_ <: TokenValueHolder, _ <: Proposition] => t }
    )

  lazy val bifrostTransactionSeqGen: Gen[Seq[Transaction[_ <: TokenValueHolder, _ <: Proposition]]] = for {
    seqLen <- positiveMediumIntGen
  } yield 0 until seqLen map { _ =>
    sampleUntilNonEmpty(sampleUntilNonEmpty(Gen.oneOf(transactionTypes)))
  }

  lazy val intSeqGen: Gen[Seq[Int]] = for {
    seqLen <- positiveMediumIntGen
  } yield 0 until seqLen map { _ =>
    sampleUntilNonEmpty(Gen.choose(0, 255))
  }

  lazy val nonEmptyBytesGen: Gen[Array[Byte]] = Gen
    .nonEmptyListOf(Arbitrary.arbitrary[Byte])
    .map(_.toArray)
    .retryUntil(_.length > 0)
  lazy val positiveLongGen: Gen[Long] = Gen.choose(1, Long.MaxValue)

  lazy val modifierIdGen: Gen[ModifierId] =
    Gen.listOfN(ModifierId.size, Arbitrary.arbitrary[Byte]).map(li => ModifierId(li.toArray))

  private val fastEntropyToSeed: EntropyToSeed[Lengths.`32`.type] =
    (entropy: Entropy, password: Option[Password]) =>
      Sized.strictUnsafe(
        Bytes(
          blake2b256
            .hash(entropy.value ++ password.map(_.getBytes(StandardCharsets.UTF_8)).getOrElse(Array.emptyByteArray))
            .value
        )
      )

  lazy val keyCurve25519FastGen: Gen[(PrivateKeyCurve25519, PublicKeyPropositionCurve25519)] = {
    implicit val entropyToSeed: EntropyToSeed[Lengths.`32`.type] = fastEntropyToSeed

    Gen.uuid.map { uuid =>
      val (sk, pk) = Curve25519.instance.createKeyPair(Entropy.fromUuid(uuid), None)
      val secret: PrivateKeyCurve25519 =
        new PrivateKeyCurve25519(PrivateKey(sk.bytes.data.toArray), PublicKey(pk.bytes.data.toArray))
      secret -> secret.publicImage
    }
  }

  lazy val keyEd25519FastGen: Gen[(PrivateKeyEd25519, PublicKeyPropositionEd25519)] = {
    implicit val entropyToSeed: EntropyToSeed[Lengths.`32`.type] = fastEntropyToSeed

    Gen.uuid.map { uuid =>
      val (sk, pk) = Ed25519.instance.createKeyPair(Entropy.fromUuid(uuid), None)
      val secret =
        new PrivateKeyEd25519(PrivateKey(sk.bytes.data.toArray), PublicKey(pk.bytes.data.toArray))
      secret -> secret.publicImage
    }
  }

  lazy val keyGen: Gen[(_ <: Secret, _ <: Proposition)] = Gen.oneOf(keyCurve25519FastGen, keyEd25519FastGen)

  lazy val publicKeyPropositionCurve25519Gen: Gen[(PrivateKeyCurve25519, PublicKeyPropositionCurve25519)] =
    keyCurve25519FastGen.map(key => key._1 -> key._2)

  lazy val publicKeyPropositionEd25519Gen: Gen[(PrivateKeyEd25519, PublicKeyPropositionEd25519)] =
    keyEd25519FastGen.map(key => key._1 -> key._2)

  lazy val oneOfNPropositionCurve25519Gen: Gen[(Set[PrivateKeyCurve25519], ThresholdPropositionCurve25519)] = for {
    n <- positiveTinyIntGen
  } yield {
    val setOfKeys = (0 until n)
      .map { _ =>
        val key = sampleUntilNonEmpty(keyCurve25519FastGen)
        (key._1, key._2)
      }
      .foldLeft((Set[PrivateKeyCurve25519](), Set[PublicKeyPropositionCurve25519]())) { (set, cur) =>
        (set._1 + cur._1, set._2 + cur._2)
      }
    val pubKeyProps = SortedSet[PublicKeyPropositionCurve25519]() ++ setOfKeys._2
    val prop = ThresholdPropositionCurve25519(1, pubKeyProps)

    (setOfKeys._1, prop)
  }

  lazy val thresholdPropositionCurve25519Gen: Gen[(Set[PrivateKeyCurve25519], ThresholdPropositionCurve25519)] = for {
    numKeys <- positiveThresholdIntGen
  } yield {
    val setOfKeys = (0 until numKeys)
      .map { _ =>
        val key = sampleUntilNonEmpty(keyCurve25519FastGen)
        (key._1, key._2)
      }
      .foldLeft((Set[PrivateKeyCurve25519](), Set[PublicKeyPropositionCurve25519]())) { (set, cur) =>
        (set._1 + cur._1, set._2 + cur._2)
      }
    val props = SortedSet[PublicKeyPropositionCurve25519]() ++ setOfKeys._2
    val threshold = numKeys / 2
    val thresholdProp = ThresholdPropositionCurve25519(threshold, props)

    (setOfKeys._1, thresholdProp)
  }

  // TODO: Jing - add threshold proposition
  lazy val publicKeyPropositionGen: Gen[(_ <: Secret, _ <: Proposition)] =
    Gen.oneOf(publicKeyPropositionCurve25519Gen, publicKeyPropositionEd25519Gen)

  lazy val propositionCurve25519Gen: Gen[PublicKeyPropositionCurve25519] = keyCurve25519FastGen.map(_._2)
  lazy val propositionEd25519Gen: Gen[PublicKeyPropositionEd25519] = keyEd25519FastGen.map(_._2)
  lazy val propositionGen: Gen[_ <: Proposition] = Gen.oneOf(propositionCurve25519Gen, propositionEd25519Gen)

  lazy val evidenceCurve25519Gen: Gen[Evidence] = for { address <- addressCurve25519Gen } yield address.evidence
  lazy val evidenceEd25519Gen: Gen[Evidence] = for { address <- addressEd25519Gen } yield address.evidence
  lazy val evidenceGen: Gen[Evidence] = Gen.oneOf(evidenceCurve25519Gen, evidenceEd25519Gen)

  lazy val addressCurve25519Gen: Gen[Address] = for { key <- propositionCurve25519Gen } yield key.address
  lazy val addressEd25519Gen: Gen[Address] = for { key <- propositionEd25519Gen } yield key.address
  lazy val addressGen: Gen[Address] = Gen.oneOf(addressCurve25519Gen, addressEd25519Gen)
  lazy val setAddressGen: Gen[Set[Address]] = Gen.listOf(addressGen).map(_.toSet)
  lazy val nonEmptySetAddressGen: Gen[Set[Address]] = Gen.nonEmptyListOf(addressGen).map(_.toSet)

  lazy val signatureCurve25519Gen: Gen[SignatureCurve25519] =
    genBytesList(SignatureCurve25519.signatureSize).map(bytes => SignatureCurve25519(Signature(bytes)))

  lazy val signatureEd25519Gen: Gen[SignatureEd25519] =
    genBytesList(SignatureEd25519.signatureSize).map(bytes => SignatureEd25519(Signature(bytes)))

  lazy val thresholdSignatureCurve25519Gen: Gen[ThresholdSignatureCurve25519] = for {
    numKeys <- positiveThresholdIntGen
    message <- nonEmptyBytesGen
  } yield {
    val sigs = (0 until numKeys).map { _ =>
      val key = sampleUntilNonEmpty(keyCurve25519FastGen)
      key._1.sign(message)
    }.toSet
    ThresholdSignatureCurve25519(sigs)
  }

  // TODO: Jing - add threshold signature
  lazy val signatureGen: Gen[_ <: Proof[_]] = Gen.oneOf(signatureCurve25519Gen, signatureEd25519Gen)

  def genBytesList(size: Int): Gen[Array[Byte]] = genBoundedBytes(size, size)

  def genBoundedBytes(minSize: Int, maxSize: Int): Gen[Array[Byte]] =
    Gen.choose(minSize, maxSize) flatMap { sz => Gen.listOfN(sz, Arbitrary.arbitrary[Byte]).map(_.toArray) }

  def specificLengthBytesGen(length: Int): Gen[Array[Byte]] = Gen
    .listOfN(length, Arbitrary.arbitrary[Byte])
    .map(_.toArray)

  lazy val blockCurve25519Gen: Gen[Block] =
    for {
      parentId     <- modifierIdGen
      timestamp    <- positiveLongGen
      generatorBox <- arbitBoxCurve25519Gen
      publicKey    <- propositionCurve25519Gen
      signature    <- signatureCurve25519Gen
      txs          <- bifrostTransactionSeqGen
      height       <- positiveLongGen
      difficulty   <- positiveLongGen
      version      <- Gen.choose[Byte](1, 127)
    } yield Block(parentId, timestamp, generatorBox, publicKey, signature, height, difficulty, txs, version)

  def blockCurve25519Gen(
    transactions: Option[Seq[Transaction[_ <: TokenValueHolder, _ <: Proposition]]] = None
  ): Gen[Block] =
    for {
      parentId     <- modifierIdGen
      timestamp    <- positiveLongGen
      generatorBox <- arbitBoxCurve25519Gen
      publicKey    <- propositionCurve25519Gen
      signature    <- signatureCurve25519Gen
      txs = transactions.fold(bifrostTransactionSeqGen.sample.get)(identity)
      height     <- positiveLongGen
      difficulty <- positiveLongGen
      version    <- Gen.choose[Byte](1, 127)
    } yield Block(parentId, timestamp, generatorBox, publicKey, signature, height, difficulty, txs, version)

  lazy val bloomFilterGen: Gen[BloomFilter] =
    Gen.listOfN(BloomFilter.numLongs, Gen.long).map(listT => BloomFilter(listT.toArray))

}
