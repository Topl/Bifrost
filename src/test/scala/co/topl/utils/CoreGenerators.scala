package co.topl.utils

import java.io.File
import java.time.Instant

import co.topl.attestation.{PrivateKeyCurve25519, PublicKeyPropositionCurve25519, ThresholdPropositionCurve25519}
import co.topl.attestation.proposition.ThresholdPropositionCurve25519
import co.topl.attestation.proof.SignatureCurve25519
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.Transaction.{Nonce, Value}
import co.topl.modifier.transaction._
import co.topl.nodeView.history.{BlockProcessor, History, Storage}
import co.topl.nodeView.state.box.{ProgramId, _}
import co.topl.program.{Program, ProgramPreprocessor, _}
import co.topl.settings.{AppSettings, StartupOpts}
import io.circe.syntax._
import io.circe.{Json, JsonObject}
import io.iohk.iodb.LSMStore
import org.scalacheck.{Arbitrary, Gen}
import scorex.crypto.hash.Blake2b256
import scorex.crypto.signatures.Signature
import scorex.util.encode.Base58

import scala.util.{Random, Try}

/**
  * Created by cykoz on 4/12/17.
  */
trait CoreGenerators extends Logging {

  private val settingsFilename = "src/test/resources/test.conf"
  val settings: AppSettings = AppSettings.read(StartupOpts(Some(settingsFilename), None))

  def sampleUntilNonEmpty[T](generator: Gen[T]): T = {
    var sampled = generator.sample

    while (sampled.isEmpty) {
      sampled = generator.sample
    }

    sampled.get
  }

  def unfoldLeft[A, B](seed: B)(f: B => Option[(A, B)]): Seq[A] = {
    f(seed) match {
      case Some((a, b)) => a +: unfoldLeft(b)(f)
      case None => Nil
    }
  }

  def splitAmongN(toSplit: Long,
                  n: Int,
                  minShareSize: Long = Long.MinValue,
                  maxShareSize: Long = Long.MaxValue): Try[Seq[Long]] = Try {
    unfoldLeft((toSplit, n)) { case (amountLeft: Long, shares: Int) =>
      if (shares > 0) {

        val longRange = BigInt(Long.MaxValue) - BigInt(Long.MinValue)
        val canOverflowOrUnderflowFully = BigInt(shares - 1) * (BigInt(maxShareSize) - BigInt(minShareSize)) >= longRange

        var noMoreThan: Long = ((BigInt(amountLeft) - BigInt(shares - 1) * BigInt(minShareSize)) % longRange).toLong
        var noLessThan: Long = ((BigInt(amountLeft) - BigInt(shares - 1) * BigInt(maxShareSize)) % longRange).toLong

        if (canOverflowOrUnderflowFully) {
          noMoreThan = maxShareSize
          noLessThan = minShareSize
        }

        var thisPortion: Long = 0L

        if (noLessThan <= maxShareSize && noMoreThan >= minShareSize && noLessThan <= noMoreThan) {
          val boundedSample = sampleUntilNonEmpty(Gen.choose(noLessThan, noMoreThan))
          thisPortion = Math.min(Math.max(boundedSample, minShareSize), maxShareSize)
        } else if (noLessThan <= maxShareSize) {
          val boundedSample = sampleUntilNonEmpty(Gen.choose(noLessThan, maxShareSize))
          thisPortion = Math.max(boundedSample, minShareSize)
        } else if (noMoreThan >= minShareSize) {
          val boundedSample = sampleUntilNonEmpty(Gen.choose(minShareSize, noMoreThan))
          thisPortion = Math.min(boundedSample, maxShareSize)
        } else {
          throw new Exception("Cannot split")
        }

        Some((thisPortion, (amountLeft - thisPortion, shares - 1)))
      } else {
        None
      }
    }
  }

  lazy val stringGen: Gen[String] = Gen.alphaNumStr.suchThat(!_.isEmpty)

  val jsonTypes: Seq[String] = Seq("Object", "Array", "Boolean", "String", "Number")

  lazy val jsonTypeGen: Gen[String] = Gen.oneOf(jsonTypes)

  private val booleanGen: Gen[Boolean] = Gen.oneOf(Seq(true, false))

  def jsonGen(depth: Int = 0): Gen[Json] = for {
    numFields <- positiveTinyIntGen
  } yield {
    (0 until numFields).map { _ =>
      sampleUntilNonEmpty(stringGen) -> (
        sampleUntilNonEmpty(jsonTypeGen) match {
          case "Object" if depth < 2 => sampleUntilNonEmpty(jsonGen(depth + 1))
          case "Array" if depth < 3 => sampleUntilNonEmpty(jsonArrayGen(depth + 1))
          case "Boolean" => sampleUntilNonEmpty(booleanGen).asJson
          case "String" => sampleUntilNonEmpty(stringGen).asJson
          case "Number" => sampleUntilNonEmpty(positiveDoubleGen).asJson
          case _ => sampleUntilNonEmpty(stringGen).asJson
        })
    }.toMap.asJson
  }

  def jsonArrayGen(depth: Int = 0): Gen[Json] = for {
    numFields <- positiveTinyIntGen
  } yield {
    ((0 until numFields) map { _ =>
      sampleUntilNonEmpty(jsonTypeGen) match {
        case "Object" if depth < 2 => sampleUntilNonEmpty(jsonGen(depth + 1))
        case "Array" if depth < 3 => sampleUntilNonEmpty(jsonArrayGen(depth + 1))
        case "Boolean" => sampleUntilNonEmpty(booleanGen).asJson
        case "String" => sampleUntilNonEmpty(stringGen).asJson
        case "Number" => sampleUntilNonEmpty(positiveDoubleGen).asJson
        case _ => sampleUntilNonEmpty(stringGen).asJson
      }
    }).asJson
  }

  private lazy val intMin = 1
  private lazy val tinyIntMax = 10
  private lazy val medIntMax = 100

  lazy val positiveTinyIntGen: Gen[Int] = Gen.choose(intMin, tinyIntMax)
  lazy val positiveMediumIntGen: Gen[Int] = Gen.choose(intMin, medIntMax)

  lazy val positiveDoubleGen: Gen[Double] = Gen.choose(0, Double.MaxValue)

  def samplePositiveDouble: Double = Random.nextFloat()

  lazy val tokenBoxesGen: Gen[Seq[TokenBox]] = for {
    tx <- Gen.someOf(polyBoxGen, arbitBoxGen, assetBoxGen)
  } yield {
    tx
  }

  lazy val polyBoxGen: Gen[PolyBox] = for {
    proposition <- propositionGen
    nonce <- positiveLongGen
    value <- positiveLongGen
  } yield {
    PolyBox(proposition, nonce, value)
  }

  lazy val arbitBoxGen: Gen[ArbitBox] = for {
    proposition <- propositionGen
    nonce <- positiveLongGen
    value <- positiveLongGen
  } yield {
    ArbitBox(proposition, nonce, value)
  }

  lazy val assetBoxGen: Gen[AssetBox] = for {
    proposition <- propositionGen
    nonce <- positiveLongGen
    value <- positiveLongGen
    asset <- stringGen
    hub <- propositionGen
    data <- stringGen
  } yield {
    AssetBox(proposition, nonce, value, asset, hub, data)
  }

  lazy val stateBoxGen: Gen[StateBox] = for {
    proposition <- propositionGen
    state <- stringGen
    nonce <- positiveLongGen
    programId <- programIdGen
  } yield {
    StateBox(proposition, nonce, programId, state.asJson)
  }

  lazy val codeBoxGen: Gen[CodeBox] = for {
    proposition <- propositionGen
    nonce <- positiveLongGen
    methodLen <- positiveTinyIntGen
    methods <- Gen.containerOfN[Seq, String](methodLen, stringGen)
    paramLen <- positiveTinyIntGen
    programId <- programIdGen
  } yield {

    val interface: Map[String, Seq[String]] = methods.map {
      _ -> Gen.containerOfN[Seq, String](paramLen, Gen.oneOf(jsonTypes)).sample.get
    }.toMap

    CodeBox(proposition, nonce, programId, methods, interface)
  }

  lazy val executionBoxGen: Gen[ExecutionBox] = for {
    proposition <- propositionGen
    codeBox_1 <- codeBoxGen
    codeBox_2 <- codeBoxGen
    nonce <- positiveLongGen
    stateBox_1 <- stateBoxGen
    stateBox_2 <- stateBoxGen
    programId <- programIdGen
  } yield {

    ExecutionBox(proposition, nonce, programId, Seq(stateBox_1.value, stateBox_2.value), Seq(codeBox_1.value, codeBox_2.value))
  }

  lazy val validExecutionBuilderTermsGen: Gen[ExecutionBuilderTerms] = for {
    size <- Gen.choose(1, 1024-1)
  } yield {
    ExecutionBuilderTerms(Random.alphanumeric.take(size).mkString)
  }

  def validInitJsGen(): Gen[String] = for {
    _ <- stringGen
  } yield {
    s"""
       |var a = 0
       |
       |add = function() {
       |  a += 1
       |}
     """.stripMargin
  }

  // TODO: This results in an empty generator far too often. Fix needed
  def validExecutionBuilderGen(): Gen[ExecutionBuilder] = for {
    assetCode <- stringGen
    terms <- validExecutionBuilderTermsGen
    name <- stringGen.suchThat(str => !Character.isDigit(str.charAt(0)))
    initjs <- validInitJsGen()
  } yield {
    ExecutionBuilder(terms, assetCode, ProgramPreprocessor(name, initjs)(JsonObject.empty))
  }

  lazy val signatureGen: Gen[SignatureCurve25519] =
    genBytesList(SignatureCurve25519.SignatureSize).map(bytes => SignatureCurve25519(Signature @@ bytes))

  lazy val programIdGen: Gen[ProgramId] = for {
    seed <- specificLengthBytesGen(ProgramId.size)
  } yield {
    ProgramId.create(seed)
  }

  lazy val programGen: Gen[Program] = for {
    producer <- propositionGen
    investor <- propositionGen
    hub <- propositionGen
    storage <- jsonGen()
    status <- jsonGen()
    executionBuilder <- validExecutionBuilderGen().map(_.json)
    id <- genBytesList(Blake2b256.DigestSize)
  } yield {
    Program(Map(
      "parties" -> Map(
        Base58.encode(producer.pubKeyBytes) -> "producer",
        Base58.encode(investor.pubKeyBytes) -> "investor",
        Base58.encode(hub.pubKeyBytes) -> "hub"
      ).asJson,
      "storage" -> Map("status" -> status, "other" -> storage).asJson,
      "executionBuilder" -> executionBuilder,
      "lastUpdated" -> System.currentTimeMillis().asJson
    ).asJson, id)
  }

  def preFeeBoxGen(minFee: Long = 0, maxFee: Long = Long.MaxValue): Gen[(Nonce, Long)] = for {
    nonce <- Gen.choose(Long.MinValue, Long.MaxValue)
    amount <- Gen.choose(minFee, maxFee)
  } yield {
    (nonce, amount)
  }

  lazy val programCreationGen: Gen[ProgramCreation] = for {
    executionBuilder <- validExecutionBuilderGen()
    readOnlyStateBoxes <- stateBoxGen
    numInvestmentBoxes <- positiveTinyIntGen
    owner <- propositionGen
    numFeeBoxes <- positiveTinyIntGen
    timestamp <- positiveLongGen
    data <- stringGen
  } yield {
    ProgramCreation(
      executionBuilder,
      Seq(readOnlyStateBoxes.value),
      (0 until numInvestmentBoxes)
        .map { _ => sampleUntilNonEmpty(positiveLongGen) -> sampleUntilNonEmpty(positiveLongGen) },
      owner,
      Map(owner -> signatureGen.sample.get),
      Map(owner -> (0 until numFeeBoxes).map { _ => sampleUntilNonEmpty(preFeeBoxGen()) }),
      Map(owner -> sampleUntilNonEmpty(positiveTinyIntGen).toLong),
      timestamp,
      data)
  }

  lazy val programMethodExecutionGen: Gen[ProgramMethodExecution] = for {
    executionBox <- executionBoxGen
    sig <- signatureGen
    numFeeBoxes <- positiveTinyIntGen
    stateNonce <- positiveLongGen
    codeNonce <- positiveLongGen
    timestamp <- positiveLongGen
    party <- propositionGen
    data <- stringGen
    sbProgramId <- programIdGen
    cbProgramId <- programIdGen
  } yield {
    val methodName = "inc"
    val parameters = JsonObject.empty.asJson
    val state = StateBox(party, stateNonce, sbProgramId, Map("a" -> 0).asJson)
    val code = CodeBox(party, codeNonce, cbProgramId,
      Seq("inc = function() { a += 1; }"), Map("inc" -> Seq()))

    ProgramMethodExecution(
      executionBox,
      Seq(state),
      Seq(code),
      methodName,
      parameters,
      party,
      Map(party -> sig),
      Map(party -> (0 until numFeeBoxes).map { _ => sampleUntilNonEmpty(preFeeBoxGen()) }),
      Map(party -> sampleUntilNonEmpty(positiveTinyIntGen).toLong),
      timestamp,
      data)
  }

  lazy val codeBoxCreationGen: Gen[CodeCreation] = for {
    to <- propositionGen
    signature <- signatureGen
    fee <- positiveLongGen
    timestamp <- positiveLongGen
    data <- stringGen
  } yield {

    val code: String =
      """
        |/**
        |* @param {Number}
        |* @param {Number}
        |**/
        |add = function(a,b) {
        | return a + b
        |}
      """.stripMargin

    CodeCreation(to, signature, code, fee, timestamp, data)
  }

  lazy val programTransferGen: Gen[ProgramTransfer] = for {
    from <- propositionGen
    to <- propositionGen
    signature <- signatureGen
    executionBox <- executionBoxGen
    fee <- positiveLongGen
    timestamp <- positiveLongGen
    data <- stringGen
  } yield {

    ProgramTransfer(from, to, signature, executionBox, fee, timestamp, data)
  }

  lazy val fromGen: Gen[(PublicKey25519Proposition, Nonce)] = for {
    proposition <- propositionGen
    nonce <- positiveLongGen
  } yield {
    (proposition, nonce)
  }

  lazy val fromSeqGen: Gen[IndexedSeq[(PublicKeyPropositionCurve25519, Nonce)]] = for {
    seqLen <- positiveTinyIntGen
  } yield {
    (0 until seqLen) map { _ => sampleUntilNonEmpty(fromGen) }
  }

  lazy val toGen: Gen[(PublicKeyPropositionCurve25519, Long)] = for {
    proposition <- propositionGen
    value <- positiveLongGen
  } yield {
    (proposition, value)
  }

  lazy val toSeqGen: Gen[IndexedSeq[(PublicKeyPropositionCurve25519, Value)]] = for {
    seqLen <- positiveTinyIntGen
  } yield {
    (0 until seqLen) map { _ => sampleUntilNonEmpty(toGen) }
  }

  lazy val sigSeqGen: Gen[IndexedSeq[SignatureCurve25519]] = for {
    seqLen <- positiveTinyIntGen
  } yield {
    (0 until seqLen) map { _ => sampleUntilNonEmpty(signatureGen) }
  }

  lazy val polyTransferGen: Gen[PolyTransfer] = for {
    from <- fromSeqGen
    to <- toSeqGen
    signatures <- sigSeqGen
    fee <- positiveLongGen
    timestamp <- positiveLongGen
    data <- stringGen
  } yield {
    PolyTransfer(from, to, from.map(a => a._1).zip(signatures).toMap, fee, timestamp, data)
  }

  lazy val arbitTransferGen: Gen[ArbitTransfer] = for {
    from <- fromSeqGen
    to <- toSeqGen
    signatures <- sigSeqGen
    fee <- positiveLongGen
    timestamp <- positiveLongGen
    data <- stringGen
  } yield {
    ArbitTransfer(from, to, from.map(a => a._1).zip(signatures).toMap, fee, timestamp, data)
  }

  lazy val assetTransferGen: Gen[AssetTransfer] = for {
    from <- fromSeqGen
    to <- toSeqGen
    signatures <- sigSeqGen
    fee <- positiveLongGen
    timestamp <- positiveLongGen
    hub <- propositionGen
    assetCode <- stringGen
    data <- stringGen
  } yield {
    AssetTransfer(from, to, from.map(a => a._1).zip(signatures).toMap, hub, assetCode, fee, timestamp, data)
  }

  lazy val assetCreationGen: Gen[AssetCreation] = for {
    to <- toSeqGen
    fee <- positiveLongGen
    timestamp <- positiveLongGen
    issuer <- key25519Gen
    assetCode <- stringGen
    data <- stringGen
  } yield {
    val rawTx = AssetCreation.createRaw(to, fee, issuer._2, assetCode, data).get
    val sig = issuer._1.sign(rawTx.messageToSign)
    AssetCreation(to, Map(issuer._2 -> sig), assetCode, issuer._2, fee, timestamp, data)
  }

  lazy val oneOfNPropositionGen: Gen[(Set[PrivateKeyCurve25519], ThresholdPropositionCurve25519)] = for {
    n <- positiveTinyIntGen
  } yield {
    val setOfKeys = (0 until n)
      .map(_ => {
        val key = sampleUntilNonEmpty(key25519Gen)
        (key._1, key._2)
      })
      .foldLeft((Set[PrivateKeyCurve25519](), Set[PublicKeyPropositionCurve25519]())) { ( set, cur) =>
          (set._1 + cur._1, set._2 + cur._2)
      }

    val prop = ThresholdCurve25519Proposition(1, setOfKeys._2.map(img => img.pubKeyBytes))

    (setOfKeys._1, prop)
  }

  lazy val keyPairSetGen: Gen[Set[(PrivateKeyCurve25519, PublicKeyPropositionCurve25519)]] = for {
    seqLen <- positiveTinyIntGen
  } yield {
    ((0 until seqLen) map { _ => sampleUntilNonEmpty(key25519Gen) }).toSet
  }

  val transactionTypes: Seq[Gen[Transaction]] =
    Seq(polyTransferGen, arbitTransferGen, assetTransferGen, assetCreationGen,
        programMethodExecutionGen, programCreationGen, programTransferGen)

  lazy val bifrostTransactionSeqGen: Gen[Seq[Transaction]] = for {
    seqLen <- positiveMediumIntGen
  } yield {
    0 until seqLen map {
      _ => {
        val g = sampleUntilNonEmpty(Gen.oneOf(transactionTypes))

        var sampled = g.sample

        while (sampled.isEmpty) sampled = g.sample

        sampled.get
      }
    }
  }

  lazy val intSeqGen: Gen[Seq[Int]] = for {
    seqLen <- positiveMediumIntGen
  } yield {
    0 until seqLen map { _ =>
      sampleUntilNonEmpty(Gen.choose(0, 255))
    }
  }


  lazy val nonEmptyBytesGen: Gen[Array[Byte]] = Gen.nonEmptyListOf(Arbitrary.arbitrary[Byte])
    .map(_.toArray).suchThat(_.length > 0)
  lazy val positiveLongGen: Gen[Long] = Gen.choose(1, Long.MaxValue)
  lazy val modifierIdGen: Gen[ModifierId] =
    Gen.listOfN(NodeViewModifier.ModifierIdSize, Arbitrary.arbitrary[Byte]).map(li => ModifierId(li.toArray))
  lazy val key25519Gen: Gen[(PrivateKey25519, PublicKey25519Proposition)] = genBytesList(Curve25519.KeyLength)
    .map(s => PrivateKey25519.generateKeys(s))
  lazy val propositionGen: Gen[PublicKey25519Proposition] = key25519Gen.map(_._2)

  def genBytesList(size: Int): Gen[Array[Byte]] = genBoundedBytes(size, size)

  def genBoundedBytes(minSize: Int, maxSize: Int): Gen[Array[Byte]] = {
    Gen.choose(minSize, maxSize) flatMap { sz => Gen.listOfN(sz, Arbitrary.arbitrary[Byte]).map(_.toArray) }
  }

  def specificLengthBytesGen(length: Int): Gen[Array[Byte]] = Gen
    .listOfN(length, Arbitrary.arbitrary[Byte])
    .map(_.toArray)

  lazy val BlockGen: Gen[Block] = for {
    parentId <- specificLengthBytesGen(Block.blockIdLength)
    timestamp <- positiveLongGen
    generatorBox <- arbitBoxGen
    signature <- signatureGen
    txs <- bifrostTransactionSeqGen
  } yield {
    Block(ModifierId(parentId), timestamp, generatorBox, signature, txs, settings.application.version.firstDigit)
  }

  lazy val genesisBlockGen: Gen[Block] = for {
    keyPair ← key25519Gen
  } yield {
    Block.createAndSign(
      History.GenesisParentId,
      Instant.now().toEpochMilli,
      Seq(),
      ArbitBox(keyPair._2, 0L, 0L),
      keyPair._1,
      settings.application.version.firstDigit)
  }

  def generateHistory(genesisBlockVersion: Byte): History = {
    val dataDir = s"/tmp/bifrost/test-data/test-${Random.nextInt(10000000)}"

    val iFile = new File(s"$dataDir/blocks")
    iFile.mkdirs()
    val blockStorage = new LSMStore(iFile)

    val storage = new Storage(blockStorage, settings.application.cacheExpire, settings.application.cacheSize)
    //we don't care about validation here
    val validators = Seq()

    var history = new History(storage, BlockProcessor(1024), validators)

    val genesisBlock = genesisBlockGen.sample.get.copy(version = genesisBlockVersion)

    history = history.append(genesisBlock).get._1
    assert(history.modifierById(genesisBlock.id).isDefined)
    history
  }

}
