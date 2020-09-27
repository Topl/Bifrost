package bifrost

import java.io.File
import java.time.Instant
import java.util.UUID

import bifrost.crypto.{ FastCryptographicHash, PrivateKey25519, Signature25519 }
import bifrost.history.{ BlockProcessor, History, Storage }
import bifrost.modifier.ModifierId
import bifrost.modifier.block.Block
import bifrost.modifier.box._
import bifrost.modifier.box.proposition.{ MofNProposition, PublicKey25519Proposition }
import bifrost.modifier.transaction.bifrostTransaction._
import bifrost.modifier.transaction.bifrostTransaction.Transaction.{ Nonce, Value }
import bifrost.network.message.BifrostSyncInfo
import bifrost.program.{ Program, ProgramPreprocessor, _ }
import bifrost.settings.{ AppSettings, StartupOpts }
import bifrost.state.ProgramId
import io.circe.{ Json, JsonObject }
import io.circe.syntax._
import io.iohk.iodb.LSMStore
import org.scalacheck.{ Arbitrary, Gen }
import scorex.crypto.encode.Base58

import scala.util.{ Random, Try }
import bifrost.utils.Logging

/**
  * Created by cykoz on 4/12/17.
  */
trait BifrostGenerators extends CoreGenerators with Logging {

  def sampleUntilNonEmpty[T](generator: Gen[T]): T = {
    var sampled = generator.sample

    while (sampled.isEmpty) {
      sampled = generator.sample
    }

    sampled.get
  }

  private val settingsFilename = "src/test/resources/test.conf"
  val settings: AppSettings = AppSettings.read(StartupOpts(Some(settingsFilename), None))
  val settings_version0: AppSettings = AppSettings.read(StartupOpts(Some(settingsFilename), None)).copy(version = "0.0.0")

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

  lazy val stringGen: Gen[String] = Gen.alphaNumStr.suchThat(!_.isEmpty) //nonEmptyBytesGen.map(new String(_))

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

  //noinspection ScalaStyle
  lazy val positiveTinyIntGen: Gen[Int] = Gen.choose(1, 10)
  lazy val positiveMediumIntGen: Gen[Int] = Gen.choose(1, 100)
  lazy val digitGen: Gen[Int] = Gen.choose(1, 9)

  //noinspection ScalaStyle
  lazy val numStringGen: Gen[String] = for {
    numDigits <- Gen.choose(0, 78)
  } yield {
    (0 until numDigits)
      .map {
        _ => sampleUntilNonEmpty(digitGen)
      }
      .foldLeft(sampleUntilNonEmpty(digitGen).toString)((a, b) => a + b) +
      sampleUntilNonEmpty(digitGen).toString
  }

  lazy val positiveDoubleGen: Gen[Double] = Gen.choose(0, Double.MaxValue)

  def samplePositiveDouble: Double = Random.nextFloat()

  lazy val smallBigDecimalGen: Gen[BigDecimal] = for {
    decimalPortion <- numStringGen
  } yield {
    BigDecimal("0." + decimalPortion)
  }

  lazy val bigDecimalGen: Gen[BigDecimal] = for {
    wholeNumber <- numStringGen
    decimalPortion <- numStringGen
  } yield {
    BigDecimal(wholeNumber + "." + decimalPortion)
  }

  //generate a num from smallInt for len of seq, map that many tuples, concatenate together into seq
  def seqDoubleGen(minLength: Int): Gen[Seq[(Double, (Double, Double, Double))]] = for {
    seqLen <- Gen.choose(minLength, minLength + sampleUntilNonEmpty(positiveTinyIntGen))
  } yield {
    (0 until seqLen) map {
      val first = samplePositiveDouble / 2
      val second = samplePositiveDouble / 2
      _ => (samplePositiveDouble, (first, second, 1 - first - second))
    }
  }

  def seqLongDoubleGen(minLength: Int): Gen[Seq[(Long, Double)]] = for {
    seqLen <- Gen.choose(minLength, minLength + sampleUntilNonEmpty(positiveTinyIntGen))
  } yield {
    (0 until seqLen) map { _ => (sampleUntilNonEmpty(positiveLongGen), samplePositiveDouble) }
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

  val doubleGen: Gen[Double] = Gen.choose(Double.MinValue, Double.MaxValue)

  lazy val stateBoxGen: Gen[StateBox] = for {
    proposition <- propositionGen
    value <- stringGen
    nonce <- positiveLongGen
  } yield {
    StateBox(proposition, nonce, ProgramId.create(), value.asJson)
  }

  lazy val codeBoxGen: Gen[CodeBox] = for {
    proposition <- propositionGen
    nonce <- positiveLongGen
    methodLen <- positiveTinyIntGen
    methods <- Gen.containerOfN[Seq, String](methodLen, stringGen)
    paramLen <- positiveTinyIntGen
  } yield {

    val interface: Map[String, Seq[String]] = methods.map {
      _ -> Gen.containerOfN[Seq, String](paramLen, Gen.oneOf(jsonTypes)).sample.get
    }.toMap

    CodeBox(proposition, nonce, ProgramId.create(), methods, interface)
  }

  lazy val executionBoxGen: Gen[ExecutionBox] = for {
    proposition <- propositionGen
    codeBox_1 <- codeBoxGen
    codeBox_2 <- codeBoxGen
    nonce <- positiveLongGen
    stateBox_1 <- stateBoxGen
    stateBox_2 <- stateBoxGen
  } yield {

    ExecutionBox(proposition, nonce, ProgramId.create(), Seq(stateBox_1.value, stateBox_2.value), Seq(codeBox_1.value, codeBox_2.value))
  }

  // TODO refactor out partiesGen and replace with proposition
  lazy val partiesGen: Gen[PublicKey25519Proposition] = for {
    a <- propositionGen
  } yield {
    a
  }

  lazy val validExecutionBuilderTermsGen: Gen[ExecutionBuilderTerms] = for {
    size <- Gen.choose(1, 1024-1)
  } yield {
    ExecutionBuilderTerms(Random.alphanumeric.take(size).mkString)
  }

  def validInitJsGen(name: String,
                     assetCode: String): Gen[String] = for {
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

  /*def validInitJsGen(name: String,
                     assetCode: String,
                     effectiveTimestamp: Long,
                     expirationTimestamp: Long): Gen[String] = for {
    _ <- stringGen
  } yield {
    s"""
       |this.$name = function(){
       |    this.programEffectiveTime = $effectiveTimestamp;
       |    this.programExpirationTime = $expirationTimestamp;
       |    this.status = "initialized";
       |    this.assetCode = "$assetCode";
       |    this.initialCapital = "0";
       |    _this = this;
       |
       |    this.changeStatus = function(newStatus) {
       |      this.status = newStatus;
       |      return this;
       |    }
       |
       |    this.newAsset = function(publicKey, asset, amount) {
       |      this.createAssets(publicKey, asset, amount);
       |      return this;
       |    }
       |
       |    this.newAssetTransfer = function(publicKey, asset, amount, data) {
       |      this.transferAssets(publicKey, asset, amount, data);
       |      return this;
       |    }
       |
       |}
       |
       |this.$name.fromJSON = function(str) {
       |    return new $name();
       |}
       |
       |this.$name.toJSON = function(o) {
       |    return JSON.stringify(o);
       |}
     """.stripMargin
  }*/

  val alphanumeric: Gen[String] = for {
    size <- positiveMediumIntGen
  } yield {
    Random.alphanumeric.take(size).mkString
  }

  // TODO: This results in an empty generator far too often. Fix needed
  def validExecutionBuilderGen(): Gen[ExecutionBuilder] = for {
    assetCode <- alphanumeric
    terms <- validExecutionBuilderTermsGen
    name <- alphanumeric.suchThat(str => !Character.isDigit(str.charAt(0)))
    initjs <- validInitJsGen(name, assetCode)
  } yield {
    ExecutionBuilder(terms, assetCode, ProgramPreprocessor(name, initjs)(JsonObject.empty))
  }

  lazy val signatureGen: Gen[Signature25519] = genBytesList(Signature25519.SignatureSize).map(Signature25519(_))

  lazy val programGen: Gen[Program] = for {
    producer <- propositionGen
    investor <- propositionGen
    hub <- propositionGen
    storage <- jsonGen()
    status <- jsonGen()
    executionBuilder <- validExecutionBuilderGen().map(_.json)
    id <- genBytesList(FastCryptographicHash.DigestSize)
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
    owner <- partiesGen
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
    //methodName <- stringGen
    stateBoxes <- positiveTinyIntGen
    executionBox <- executionBoxGen
    codeBoxes <- positiveTinyIntGen
    //parameters <- jsonArrayGen()
    sig <- signatureGen
    numFeeBoxes <- positiveTinyIntGen
    stateNonce <- positiveLongGen
    codeNonce <- positiveLongGen
    timestamp <- positiveLongGen
    party <- propositionGen
    data <- stringGen
  } yield {

    /*val state = (0 until stateBoxes).map { _ =>
      sampleUntilNonEmpty(stateBoxGen)
    }*/

    /*val code = (0 until codeBoxes).map { _ =>
      sampleUntilNonEmpty(codeBoxGen)
    }*/

    val methodName = "inc"

    val parameters = JsonObject.empty.asJson

    val state = StateBox(party, stateNonce, ProgramId.create(), Map("a" -> 0).asJson)

    val code = CodeBox(party, codeNonce, ProgramId.create(),
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

  lazy val assetHubGen: Gen[(String, PublicKey25519Proposition)] = for {
    asset <- stringGen
    hub <- propositionGen
  } yield {
    (asset, hub)
  }

  lazy val ctFromGen: Gen[(PublicKey25519Proposition, Nonce)] = for {
    proposition <- propositionGen
    nonce <- positiveLongGen
  } yield {
    (proposition, nonce)
  }

  lazy val ctToGen: Gen[(PublicKey25519Proposition, Long)] = for {
    proposition <- propositionGen
    amount <- positiveLongGen
  } yield {
    (proposition, amount)
  }

  lazy val fromGen: Gen[(PublicKey25519Proposition, Nonce)] = for {
    proposition <- propositionGen
    nonce <- positiveLongGen
  } yield {
    (proposition, nonce)
  }

  lazy val fromSeqGen: Gen[IndexedSeq[(PublicKey25519Proposition, Nonce)]] = for {
    seqLen <- positiveTinyIntGen
  } yield {
    (0 until seqLen) map { _ => sampleUntilNonEmpty(fromGen) }
  }

  lazy val toGen: Gen[(PublicKey25519Proposition, Long)] = for {
    proposition <- propositionGen
    value <- positiveLongGen
  } yield {
    (proposition, value)
  }

  lazy val toSeqGen: Gen[IndexedSeq[(PublicKey25519Proposition, Value)]] = for {
    seqLen <- positiveTinyIntGen
  } yield {
    (0 until seqLen) map { _ => sampleUntilNonEmpty(toGen) }
  }

  lazy val sigSeqGen: Gen[IndexedSeq[Signature25519]] = for {
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
    signatures <- sigSeqGen
    fee <- positiveLongGen
    timestamp <- positiveLongGen
    hub <- propositionGen
    assetCode <- stringGen
    data <- stringGen
  } yield {
    AssetCreation(to, Map(hub -> signatures.head), assetCode, hub, fee, timestamp, data)
  }

  lazy val oneOfNPropositionGen: Gen[(Set[PrivateKey25519], MofNProposition)] = for {
    n <- positiveTinyIntGen
  } yield {
    val setOfKeys = (0 until n)
      .map(i => {
        val key = sampleUntilNonEmpty(key25519Gen)
        (key._1, key._2.pubKeyBytes)
      })
      .foldLeft((Set[PrivateKey25519](), Set[Array[Byte]]())) {
        case (set: (Set[PrivateKey25519], Set[Array[Byte]]), cur: (PrivateKey25519, Array[Byte])) =>
          (set._1 + cur._1, set._2 + cur._2)
      }
    val prop = MofNProposition(1, setOfKeys._2)

    (setOfKeys._1, prop)
  }

  lazy val keyPairSetGen: Gen[Set[(PrivateKey25519, PublicKey25519Proposition)]] = for {
    seqLen <- positiveTinyIntGen
  } yield {
    ((0 until seqLen) map { _ => sampleUntilNonEmpty(key25519Gen) }).toSet
  }

  //TODO Add programCreationGen after fixing serialization
  val transactionTypes: Seq[Gen[Transaction]] =
    Seq(polyTransferGen, arbitTransferGen, assetTransferGen,
      assetCreationGen, programCreationGen, programMethodExecutionGen, programTransferGen)

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
    Block(ModifierId(parentId), timestamp, generatorBox, signature, txs, settings.forgingSettings.version)
  }

  lazy val bifrostSyncInfoGen: Gen[BifrostSyncInfo] = for {
    numLastBlocks <- Gen.choose(1, 10)
  } yield {
    val lastBlockIds = (0 until numLastBlocks).map { _ => sampleUntilNonEmpty(modifierIdGen) }
    BifrostSyncInfo(lastBlockIds)
  }

  lazy val genesisBlockGen: Gen[Block] = for {
    keyPair ← key25519Gen
  } yield {
    Block.create(
      ModifierId(History.GenesisParentId),
      Instant.now().toEpochMilli,
      Seq(),
      ArbitBox(keyPair._2, 0L, 0L),
      keyPair._1,
      settings.forgingSettings.version)
  }

  def generateHistory: History = {
    val dataDir = s"/tmp/bifrost/test-data/test-${Random.nextInt(10000000)}"

    val iFile = new File(s"$dataDir/blocks")
    iFile.mkdirs()
    val blockStorage = new LSMStore(iFile)

    val storage = new Storage(blockStorage, settings)
    //we don't care about validation here
    val validators = Seq()

    var history = new History(storage, BlockProcessor(1024), settings, validators)

    val genesisBlock = genesisBlockGen.sample.get

    history = history.append(genesisBlock).get._1
    assert(history.modifierById(genesisBlock.id).isDefined)
    history
  }

}
