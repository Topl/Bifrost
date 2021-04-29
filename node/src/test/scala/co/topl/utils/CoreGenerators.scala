package co.topl.utils

import java.io.File
import java.time.Instant
import co.topl.attestation.PublicKeyPropositionCurve25519.evProducer
import co.topl.attestation._
import co.topl.attestation.keyManagement.{
  KeyRing,
  KeyfileCurve25519,
  KeyfileCurve25519Companion,
  PrivateKeyCurve25519,
  Secret
}
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.block.PersistentNodeViewModifier.PNVMVersion
import co.topl.modifier.box.Box.Nonce
import co.topl.modifier.box.{ProgramId, _}
import co.topl.modifier.transaction._
import co.topl.nodeView.history.{BlockProcessor, History, Storage}
import co.topl.settings.{AppSettings, StartupOpts, Version}
import co.topl.utils.NetworkType.{NetworkPrefix, PrivateTestnet}
import io.circe.syntax._
import io.circe.Json
import io.iohk.iodb.LSMStore
import org.scalacheck.{Arbitrary, Gen}
import scorex.crypto.hash.Blake2b256
import scorex.crypto.signatures.{Curve25519, Signature}
import scorex.util.encode.Base58

import scala.collection.SortedSet
import scala.util.{Random, Try}

/**
 * Created by cykoz on 4/12/17.
 */
trait CoreGenerators extends Logging {

  type P = Proposition
  type S = Secret

  implicit val networkPrefix: NetworkPrefix = PrivateTestnet.netPrefix
  implicit val keyfileCurve25519Companion: KeyfileCurve25519Companion.type = KeyfileCurve25519Companion

  private val settingsFilename = "node/src/test/resources/test.conf"
  val settings: AppSettings = AppSettings.read(StartupOpts(Some(settingsFilename), None))._1

  private val keyFileDir =
    settings.application.keyFileDir.ensuring(_.isDefined, "A keyfile directory must be specified").get
  private val keyRing = KeyRing.empty[PrivateKeyCurve25519, KeyfileCurve25519](Some(keyFileDir))

  def sampleUntilNonEmpty[T](generator: Gen[T]): T = {
    var sampled = generator.sample

    while (sampled.isEmpty)
      sampled = generator.sample

    sampled.get
  }

  lazy val stringGen: Gen[String] = Gen.alphaNumStr.suchThat(_.nonEmpty)

  lazy val shortNameGen: Gen[String] = for {
    n   <- Gen.choose(0, AssetCode.shortNameLimit)
    str <- Gen.listOfN(n, Gen.alphaNumChar).map(_.mkString)
  } yield str

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

  private lazy val int128Min: Int128 = Int128.MaxValue
  private lazy val int128Max: Int128 = Int128.MaxValue

  implicit lazy val int128Chooser: Gen.Choose[Int128] =
    (min, max) => Gen.Choose.chooseBigInt.choose(min.bigInt, max.bigInt).map(Int128(_))

  lazy val positiveTinyIntGen: Gen[Int] = Gen.choose(intMin, tinyIntMax)
  lazy val positiveMediumIntGen: Gen[Int] = Gen.choose(intMin, medIntMax)

  lazy val positiveDoubleGen: Gen[Double] = Gen.choose(0, Double.MaxValue)

  lazy val positiveInt128Gen: Gen[Int128] = Gen.choose[Int128](0, int128Max)
  lazy val smallInt128Gen: Gen[Int128] = Gen.choose[Int128](0, Int.MaxValue)
  lazy val largeInt128Gen: Gen[Int128] = Gen.choose[Int128](Long.MaxValue, int128Max)

  def samplePositiveDouble: Double = Random.nextFloat()

  lazy val tokenBoxesGen: Gen[Seq[TokenBox[TokenValueHolder]]] = for {
    tx <- Gen.someOf(polyBoxGen, arbitBoxGen, assetBoxGen)
  } yield tx

  lazy val polyBoxGen: Gen[PolyBox] = for {
    evidence <- evidenceGen
    nonce    <- positiveLongGen
    value    <- positiveLongGen
  } yield PolyBox(evidence, nonce, SimpleValue(value))

  lazy val arbitBoxGen: Gen[ArbitBox] = for {
    evidence <- evidenceGen
    nonce    <- positiveLongGen
    value    <- positiveLongGen
  } yield ArbitBox(evidence, nonce, SimpleValue(value))

  lazy val assetBoxGen: Gen[AssetBox] = for {
    evidence <- evidenceGen
    nonce    <- positiveLongGen
    quantity <- positiveLongGen
    // TODO: Hard coded as 1, but change this to arbitrary in the future
    //assetVersion <- Arbitrary.arbitrary[Byte]
    shortName <- shortNameGen
    issuer    <- addressGen
    data      <- stringGen
  } yield {
    // TODO: Hard coded as 1, but change this to arbitrary in the future
    val assetVersion = 1: Byte
    val assetCode = AssetCode(assetVersion, issuer, shortName)
    val value = AssetValue(quantity, assetCode, metadata = Some(data))
    AssetBox(evidence, nonce, value)
  }

  lazy val stateBoxGen: Gen[StateBox] = for {
    evidence  <- evidenceGen
    state     <- stringGen
    nonce     <- positiveLongGen
    programId <- programIdGen
  } yield StateBox(evidence, nonce, programId, state.asJson)

  lazy val codeBoxGen: Gen[CodeBox] = for {
    evidence  <- evidenceGen
    nonce     <- positiveLongGen
    methodLen <- positiveTinyIntGen
    methods   <- Gen.containerOfN[Seq, String](methodLen, stringGen)
    paramLen  <- positiveTinyIntGen
    programId <- programIdGen
  } yield {

    val interface: Map[String, Seq[String]] = methods.map {
      _ -> Gen.containerOfN[Seq, String](paramLen, Gen.oneOf(jsonTypes)).sample.get
    }.toMap

    CodeBox(evidence, nonce, programId, methods, interface)
  }

  lazy val executionBoxGen: Gen[ExecutionBox] = for {
    evidence   <- evidenceGen
    codeBox_1  <- codeBoxGen
    codeBox_2  <- codeBoxGen
    nonce      <- positiveLongGen
    stateBox_1 <- stateBoxGen
    stateBox_2 <- stateBoxGen
    programId  <- programIdGen
  } yield ExecutionBox(
    evidence,
    nonce,
    programId,
    Seq(stateBox_1.value, stateBox_2.value),
    Seq(codeBox_1.value, codeBox_2.value)
  )

  lazy val signatureGen: Gen[SignatureCurve25519] =
    genBytesList(SignatureCurve25519.signatureSize).map(bytes => SignatureCurve25519(Signature @@ bytes))

  lazy val programIdGen: Gen[ProgramId] = for {
    seed <- specificLengthBytesGen(ProgramId.size)
  } yield ProgramId.create(seed)

  def preFeeBoxGen(minFee: Long = 0, maxFee: Long = Long.MaxValue): Gen[(Nonce, Long)] = for {
    nonce  <- Gen.choose(Long.MinValue, Long.MaxValue)
    amount <- Gen.choose(minFee, maxFee)
  } yield (nonce, amount)

  lazy val fromGen: Gen[(Address, Nonce)] = for {
    address <- addressGen
    nonce   <- positiveLongGen
  } yield (address, nonce)

  lazy val fromSeqGen: Gen[IndexedSeq[(Address, Nonce)]] = for {
    seqLen <- positiveTinyIntGen
  } yield (0 until seqLen) map { _ => sampleUntilNonEmpty(fromGen) }

  lazy val simpleValueGen: Gen[SimpleValue] = for {
    value <- positiveLongGen
  } yield SimpleValue(value)

  lazy val toGen: Gen[(Address, SimpleValue)] = for {
    address <- addressGen
    value   <- positiveLongGen
  } yield (address, SimpleValue(value))

  lazy val assetCodeGen: Gen[AssetCode] = for {
    // TODO: Hard coded as 1, but change this to arbitrary in the future
    // assetVersion <- Arbitrary.arbitrary[Byte]
    issuer    <- addressGen
    shortName <- shortNameGen
  } yield AssetCode(1: Byte, issuer, shortName)

  lazy val assetValueGen: Gen[AssetValue] = for {
    quantity  <- positiveLongGen
    assetCode <- assetCodeGen
    data      <- stringGen
  } yield AssetValue(quantity, assetCode, metadata = Some(data))

  lazy val assetToGen: Gen[(Address, AssetValue)] = for {
    assetValue <- assetValueGen
  } yield (assetValue.assetCode.issuer, assetValue)

  lazy val securityRootGen: Gen[SecurityRoot] = for {
    root <- specificLengthBytesGen(Blake2b256.DigestSize)
  } yield SecurityRoot(Base58.encode(root))

  lazy val toSeqGen: Gen[IndexedSeq[(Address, SimpleValue)]] = for {
    seqLen <- positiveTinyIntGen
  } yield (0 until seqLen) map { _ => sampleUntilNonEmpty(toGen) }

  lazy val assetToSeqGen: Gen[IndexedSeq[(Address, TokenValueHolder)]] = for {
    seqLen <- positiveTinyIntGen
  } yield (0 until seqLen) map { _ => sampleUntilNonEmpty(assetToGen) }

  lazy val sigSeqGen: Gen[IndexedSeq[SignatureCurve25519]] = for {
    seqLen <- positiveTinyIntGen
  } yield (0 until seqLen) map { _ => sampleUntilNonEmpty(signatureGen) }

  lazy val polyTransferGen: Gen[PolyTransfer[PublicKeyPropositionCurve25519]] = for {
    from        <- fromSeqGen
    to          <- toSeqGen
    attestation <- attestationGen
    fee         <- positiveLongGen
    timestamp   <- positiveLongGen
    data        <- stringGen
  } yield PolyTransfer(from, to, attestation, fee, timestamp, Some(data), minting = false)

  lazy val arbitTransferGen: Gen[ArbitTransfer[PublicKeyPropositionCurve25519]] = for {
    from        <- fromSeqGen
    to          <- toSeqGen
    attestation <- attestationGen
    fee         <- positiveLongGen
    timestamp   <- positiveLongGen
    data        <- stringGen
  } yield ArbitTransfer(from, to, attestation, fee, timestamp, Some(data), minting = false)

  lazy val assetTransferGen: Gen[AssetTransfer[PublicKeyPropositionCurve25519]] = for {
    from        <- fromSeqGen
    to          <- assetToSeqGen
    attestation <- attestationGen
    fee         <- positiveLongGen
    timestamp   <- positiveLongGen
    data        <- stringGen
  } yield AssetTransfer(from, to, attestation, fee, timestamp, Some(data), minting = true)

  lazy val publicKeyPropositionCurve25519Gen: Gen[(PrivateKeyCurve25519, PublicKeyPropositionCurve25519)] =
    key25519Gen.map(key => key._1 -> key._2)

  lazy val thresholdPropositionCurve25519Gen: Gen[(Set[PrivateKeyCurve25519], ThresholdPropositionCurve25519)] = for {
    numKeys   <- positiveMediumIntGen
    threshold <- positiveTinyIntGen
  } yield {
    val setOfKeys = (0 until numKeys)
      .map { _ =>
        val key = sampleUntilNonEmpty(key25519Gen)
        (key._1, key._2)
      }
      .foldLeft((Set[PrivateKeyCurve25519](), Set[PublicKeyPropositionCurve25519]())) { (set, cur) =>
        (set._1 + cur._1, set._2 + cur._2)
      }
    val props = SortedSet[PublicKeyPropositionCurve25519]() ++ setOfKeys._2
    val thresholdProp = ThresholdPropositionCurve25519(threshold, props)

    (setOfKeys._1, thresholdProp)
  }

  lazy val thresholdSignatureCurve25519Gen: Gen[ThresholdSignatureCurve25519] = for {
    numKeys <- positiveMediumIntGen
    message <- nonEmptyBytesGen
  } yield {
    val sigs = (0 until numKeys).map { _ =>
      val key = sampleUntilNonEmpty(key25519Gen)
      key._1.sign(message)
    }.toSet
    ThresholdSignatureCurve25519(sigs)
  }

  lazy val propTypes: Gen[String] = sampleUntilNonEmpty(
    Gen.oneOf(PublicKeyPropositionCurve25519.typeString, ThresholdPropositionCurve25519.typeString)
  )

  lazy val keyPairGen: Gen[(Set[_ <: Secret], _ <: KnowledgeProposition[_ <: Secret])] = for {
    propType <- propTypes
  } yield propType match {
    case PublicKeyPropositionCurve25519.typeString =>
      val key = publicKeyPropositionCurve25519Gen.sample.get
      Set(key._1) -> key._2
    case ThresholdPropositionCurve25519.typeString =>
      thresholdPropositionCurve25519Gen.sample.get
  }

  lazy val attestationGen: Gen[Map[PublicKeyPropositionCurve25519, Proof[PublicKeyPropositionCurve25519]]] = for {
    prop <- propositionGen
    sig  <- signatureGen
  } yield Map(prop -> sig)

  lazy val oneOfNPropositionGen: Gen[(Set[PrivateKeyCurve25519], ThresholdPropositionCurve25519)] = for {
    n <- positiveTinyIntGen
  } yield {
    val setOfKeys = (0 until n)
      .map { _ =>
        val key = sampleUntilNonEmpty(key25519Gen)
        (key._1, key._2)
      }
      .foldLeft((Set[PrivateKeyCurve25519](), Set[PublicKeyPropositionCurve25519]())) { (set, cur) =>
        (set._1 + cur._1, set._2 + cur._2)
      }
    val pubKeyProps = SortedSet[PublicKeyPropositionCurve25519]() ++ setOfKeys._2
    val prop = ThresholdPropositionCurve25519(1, pubKeyProps)

    (setOfKeys._1, prop)
  }

  lazy val keyPairSetGen: Gen[Set[(PrivateKeyCurve25519, PublicKeyPropositionCurve25519)]] = for {
    seqLen <- positiveTinyIntGen
  } yield ((0 until seqLen) map { _ => sampleUntilNonEmpty(key25519Gen) }).toSet

  val transactionTypes: Seq[Gen[Transaction.TX]] =
    Seq(polyTransferGen, arbitTransferGen, assetTransferGen)

  lazy val bifrostTransactionSeqGen: Gen[Seq[Transaction.TX]] = for {
    seqLen <- positiveMediumIntGen
  } yield 0 until seqLen map { _ =>
    val g = sampleUntilNonEmpty(Gen.oneOf(transactionTypes))

    var sampled = g.sample

    while (sampled.isEmpty) sampled = g.sample

    sampled.get
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
    Gen.listOfN(ModifierId.size, Arbitrary.arbitrary[Byte]).map(li => ModifierId.parseBytes(li.toArray).get)

  lazy val key25519Gen: Gen[(PrivateKeyCurve25519, PublicKeyPropositionCurve25519)] = genBytesList(Curve25519.KeyLength)
    .map(s => PrivateKeyCurve25519.secretGenerator.generateSecret(s))
  lazy val propositionGen: Gen[PublicKeyPropositionCurve25519] = key25519Gen.map(_._2)
  lazy val evidenceGen: Gen[Evidence] = for { address <- addressGen } yield address.evidence
  lazy val addressGen: Gen[Address] = for { key <- propositionGen } yield key.address

  lazy val versionGen: Gen[Version] = for {
    first  <- Gen.choose(0: Byte, Byte.MaxValue)
    second <- Gen.choose(0: Byte, Byte.MaxValue)
    third  <- Gen.choose(0: Byte, Byte.MaxValue)
  } yield new Version(first, second, third)

  def genBytesList(size: Int): Gen[Array[Byte]] = genBoundedBytes(size, size)

  def genBoundedBytes(minSize: Int, maxSize: Int): Gen[Array[Byte]] =
    Gen.choose(minSize, maxSize) flatMap { sz => Gen.listOfN(sz, Arbitrary.arbitrary[Byte]).map(_.toArray) }

  def specificLengthBytesGen(length: Int): Gen[Array[Byte]] = Gen
    .listOfN(length, Arbitrary.arbitrary[Byte])
    .map(_.toArray)

  lazy val blockGen: Gen[Block] = for {
    parentIdBytes <- specificLengthBytesGen(ModifierId.size)
    timestamp     <- positiveLongGen
    generatorBox  <- arbitBoxGen
    publicKey     <- propositionGen
    signature     <- signatureGen
    txs           <- bifrostTransactionSeqGen
  } yield {
    val parentId = ModifierId(Base58.encode(parentIdBytes))
    val height: Long = 1L
    val difficulty = settings.forging.privateTestnet.map(_.initialDifficulty).get
    val version: PNVMVersion = settings.application.version.firstDigit

    Block(parentId, timestamp, generatorBox, publicKey, signature, height, difficulty, txs, version)
  }

  lazy val genesisBlockGen: Gen[Block] = {
    val keyPair = keyRing.generateNewKeyPairs().get.head
    val matchingAddr = keyPair.publicImage.address
    val height: Long = 1L
    val difficulty = settings.forging.privateTestnet.map(_.initialDifficulty).get
    val version: PNVMVersion = settings.application.version.firstDigit
    val signingFunction: Array[Byte] => Try[SignatureCurve25519] =
      (messageToSign: Array[Byte]) => keyRing.signWithAddress(matchingAddr)(messageToSign)

    Block
      .createAndSign(
        History.GenesisParentId,
        Instant.now().toEpochMilli,
        Seq(),
        ArbitBox(matchingAddr.evidence, 0L, SimpleValue(0)),
        keyPair.publicImage,
        height,
        difficulty,
        version
      )(signingFunction)
      .get
  }

  def generateHistory(genesisBlock: Block = genesisBlockGen.sample.get): History = {
    val dataDir = s"/tmp/bifrost/test-data/test-${Random.nextInt(10000000)}"

    val iFile = new File(s"$dataDir/blocks")
    iFile.mkdirs()
    val blockStorage = new LSMStore(iFile)

    val storage = new Storage(blockStorage, settings.application.cacheExpire, settings.application.cacheSize)
    //we don't care about validation here
    val validators = Seq()

    var history = new History(storage, BlockProcessor(1024), validators)

    history = history.append(genesisBlock).get._1
    assert(history.modifierById(genesisBlock.id).isDefined)
    history
  }

}
