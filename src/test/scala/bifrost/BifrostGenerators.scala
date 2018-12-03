package bifrost

import java.io.File
import java.time.Instant

import bifrost.blocks.BifrostBlock
import bifrost.contract.modules.BaseModuleWrapper
import bifrost.contract.{Contract, _}
import bifrost.forging.ForgingSettings
import bifrost.history.{BifrostHistory, BifrostStorage, BifrostSyncInfo}
import bifrost.transaction.BifrostTransaction.{Nonce, Value}
import bifrost.transaction.Role.Role
import bifrost.transaction._
import bifrost.transaction.box._
import bifrost.transaction.box.proposition.MofNProposition
import io.circe
import io.circe.syntax._
import io.circe.{Json, JsonObject}
import io.iohk.iodb.LSMStore
import org.scalacheck.{Arbitrary, Gen}
import scorex.core.block.Block
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.core.transaction.state.PrivateKey25519
import scorex.crypto.encode.Base58
import scorex.testkit.CoreGenerators

import scala.util.{Random, Try}

/**
  * Created by cykoz on 4/12/17.
  */
trait BifrostGenerators extends CoreGenerators {

  def sampleUntilNonEmpty[T](generator: Gen[T]): T = {
    var sampled = generator.sample

    while (sampled.isEmpty) {
      sampled = generator.sample
    }

    sampled.get
  }

  val settings: ForgingSettings = new ForgingSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile("testSettings.json")

    override lazy val Difficulty: BigInt = 1
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

        Some(thisPortion, (amountLeft - thisPortion, shares - 1))
      } else {
        None
      }
    }
  }

  lazy val stringGen: Gen[String] = Gen.alphaStr suchThat (!_.isEmpty) //nonEmptyBytesGen.map(new String(_))

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
  val digitGen = Gen.choose(1, 9)

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

  lazy val shareFuncGen: Gen[ShareFunction] = seqDoubleGen(2).map(PiecewiseLinearMultiple)

  def seqLongDoubleGen(minLength: Int): Gen[Seq[(Long, Double)]] = for {
    seqLen <- Gen.choose(minLength, minLength + sampleUntilNonEmpty(positiveTinyIntGen))
  } yield {
    (0 until seqLen) map { _ => (sampleUntilNonEmpty(positiveLongGen), samplePositiveDouble) }
  }

  lazy val fulfilFuncGen: Gen[FulfilmentFunction] = seqLongDoubleGen(2).map(PiecewiseLinearSingle)

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

  lazy val reputationBoxGen: Gen[ReputationBox] = for {
    proposition <- propositionGen
    nonce <- positiveLongGen
  } yield {
    ReputationBox(proposition, nonce, (sampleUntilNonEmpty(doubleGen), sampleUntilNonEmpty(doubleGen)))
  }

  lazy val profileBoxGen: Gen[ProfileBox] = for {
    proposition <- propositionGen
    value <- stringGen
    field <- stringGen
  } yield {
    ProfileBox(proposition, 0L, value, field)
  }

  lazy val partiesGen: Gen[Map[PublicKey25519Proposition, Role]] = for {
    a <- propositionGen
    b <- propositionGen
    c <- propositionGen
  } yield {
    Map(a -> Role.Producer, b -> Role.Hub, c -> Role.Investor)
  }

  lazy val validShareFuncGen: Gen[ShareFunction] = seqDoubleGen(sampleUntilNonEmpty(positiveTinyIntGen)).map(seq => {
    val first: Double = samplePositiveDouble / 2
    val second: Double = samplePositiveDouble / 2
    PiecewiseLinearMultiple((0.0, (first, second, 1 - first - second)) +: seq)
  })

  lazy val validFulfilFuncGen: Gen[FulfilmentFunction] = seqLongDoubleGen(sampleUntilNonEmpty(positiveTinyIntGen))
    .map(seq => PiecewiseLinearSingle((0L, samplePositiveDouble) +: seq))

  lazy val validAgreementTermsGen: Gen[AgreementTerms] = for {
    size <- Gen.choose(1, 16 * 1024-1)
  } yield {
    AgreementTerms(Random.alphanumeric.take(size).mkString)
  }

  def validInitJsGen(name: String,
                     assetCode: String,
                     effectiveTimestamp: Long,
                     expirationTimestamp: Long): Gen[String] = for {
    _ <- stringGen
  } yield {
    s"""
       |this.$name = function(){
       |    this.contractEffectiveTime = $effectiveTimestamp;
       |    this.contractExpirationTime = $expirationTimestamp;
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
       |      this.createAsset(publicKey, asset, amount);
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
  }

  val alphanumeric: Gen[String] = for {
    size <- positiveMediumIntGen
  } yield {
    Random.alphanumeric.take(size).mkString
  }

  // TODO: This results in an empty generator far too often. Fix needed
  def validAgreementGen(effectiveTimestamp: Long = Instant.now.toEpochMilli,
                        expirationTimestamp: Long = Instant.now.toEpochMilli + 10000L): Gen[Agreement] = for {
    assetCode <- alphanumeric
    terms <- validAgreementTermsGen
    name <- alphanumeric.suchThat(str => !Character.isDigit(str.charAt(0)))
    initjs <- validInitJsGen(name, assetCode, effectiveTimestamp, expirationTimestamp)
  } yield {
    Agreement(terms, assetCode, BaseModuleWrapper(name, initjs)(JsonObject.empty))
  }

  lazy val signatureGen: Gen[Signature25519] = genBytesList(Signature25519.SignatureSize).map(Signature25519(_))

  lazy val contractGen: Gen[Contract] = for {
    producer <- propositionGen
    investor <- propositionGen
    hub <- propositionGen
    storage <- jsonGen()
    status <- jsonGen()
    agreement <- validAgreementGen().map(_.json)
    id <- genBytesList(FastCryptographicHash.DigestSize)
  } yield {
    Contract(Map(
      "parties" -> Map(
        Base58.encode(producer.pubKeyBytes) -> "producer",
        Base58.encode(investor.pubKeyBytes) -> "investor",
        Base58.encode(hub.pubKeyBytes) -> "hub"
      ).asJson,
      "storage" -> Map("status" -> status, "other" -> storage).asJson,
      "agreement" -> agreement,
      "lastUpdated" -> System.currentTimeMillis().asJson
    ).asJson, id)
  }

  lazy val contractBoxGen: Gen[ContractBox] = for {
    proposition <- oneOfNPropositionGen
    nonce <- positiveLongGen
    value <- contractGen.map(_.json)
  } yield {
    ContractBox(proposition._2, nonce, value)
  }

  def preFeeBoxGen(minFee: Long = 0, maxFee: Long = Long.MaxValue): Gen[(Nonce, Long)] = for {
    nonce <- Gen.choose(Long.MinValue, Long.MaxValue)
    amount <- Gen.choose(minFee, maxFee)
  } yield {
    (nonce, amount)
  }

  lazy val contractCreationGen: Gen[ContractCreation] = for {
    agreement <- validAgreementGen()
    numInvestmentBoxes <- positiveTinyIntGen
    parties <- partiesGen
    numFeeBoxes <- positiveTinyIntGen
    timestamp <- positiveLongGen
    data <- stringGen
  } yield {
    ContractCreation(
      agreement,
      (0 until numInvestmentBoxes)
        .map { _ => sampleUntilNonEmpty(positiveLongGen) -> sampleUntilNonEmpty(positiveLongGen) },
      parties,
      parties.map({ case (k, _) => (k, sampleUntilNonEmpty(signatureGen)) }),
      parties.map({ case (k, _) => k -> (0 until numFeeBoxes).map { _ => sampleUntilNonEmpty(preFeeBoxGen()) } }),
      parties.map({ case (k, _) => k -> sampleUntilNonEmpty(positiveTinyIntGen).toLong }),
      timestamp,
      data)
  }

  lazy val contractMethodExecutionGen: Gen[ContractMethodExecution] = for {
    contract <- contractBoxGen
    methodName <- stringGen
    parameters <- jsonArrayGen()
    sig <- signatureGen
    numFeeBoxes <- positiveTinyIntGen
    fee <- positiveLongGen
    timestamp <- positiveLongGen
    party <- propositionGen
    data <- stringGen
  } yield {
    ContractMethodExecution(
      contract,
      methodName,
      parameters,
      Map(sampleUntilNonEmpty(party) -> Gen.oneOf(Role.values.toSeq).sample.get),
      Map(party -> sig),
      Map(party -> (0 until numFeeBoxes).map { _ => sampleUntilNonEmpty(preFeeBoxGen()) }),
      Map(party -> sampleUntilNonEmpty(positiveTinyIntGen).toLong),
      timestamp,
      data)
  }

  lazy val contractCompletionGen: Gen[ContractCompletion] = for {
    contract <- contractBoxGen
    reputation <- reputationBoxGen
    parties <- partiesGen
    signature <- signatureGen
    fee <- positiveLongGen
    numFeeBoxes <- positiveTinyIntGen
    timestamp <- positiveLongGen
    data <- stringGen
  } yield {
    ContractCompletion(
      contract,
      IndexedSeq(reputation),
      parties,
      parties.map({ case (k, _) => (k, sampleUntilNonEmpty(signatureGen)) }),
      parties.map({ case (k, _) => k -> (0 until numFeeBoxes).map { _ => sampleUntilNonEmpty(preFeeBoxGen()) } }),
      parties.map({ case (k, _) => k -> sampleUntilNonEmpty(positiveTinyIntGen).toLong }),
      timestamp,
      data)
  }

  lazy val profileTxGen: Gen[ProfileTransaction] = for {
    from <- propositionGen
    numKeys <- positiveMediumIntGen
    fee <- positiveLongGen
    timestamp <- positiveLongGen
  } yield {
    val signature = sampleUntilNonEmpty(signatureGen)
    val keyValues = (0 until numKeys).map { _ => (sampleUntilNonEmpty(stringGen), sampleUntilNonEmpty(stringGen)) }
      .foldLeft[Map[String, String]](Map())((a, b) => a + b)
    ProfileTransaction(from, signature, keyValues, fee, timestamp)
  }

  lazy val assetRedemptionGen: Gen[AssetRedemption] = for {
    assetLength <- positiveTinyIntGen
    hub <- propositionGen
    fee <- positiveLongGen
    timestamp <- positiveLongGen
    data <- stringGen
  } yield {

    val assets = (0 until assetLength).map { _ =>
      sampleUntilNonEmpty(stringGen)
    }

    val availableToRedeem = assets.map(_ -> sampleUntilNonEmpty(fromSeqGen)).toMap
    val remainderAllocations = assets.map(_ -> sampleUntilNonEmpty(toSeqGen)).toMap

    val signatures = availableToRedeem.map { case (assetId, boxes) =>
      assetId -> boxes.map(_ => sampleUntilNonEmpty(signatureGen))
    }

    AssetRedemption(availableToRedeem, remainderAllocations, signatures, hub, fee, timestamp, data)
  }

  /*lazy val conversionTxGen: Gen[ConversionTransaction] = for {
    assetLength <- positiveTinyIntGen
    fee <- positiveLongGen
    timestamp <- positiveLongGen
    data <- stringGen
  } yield {
    val assetHub = (0 until assetLength).map { _ => sampleUntilNonEmpty(assetHubGen) }
    val totalAssetBoxes = assetHub.map(_ -> IndexedSeq(sampleUntilNonEmpty(ctFromGen))).toMap
    val assetsToReturn = assetHub.map(_ -> IndexedSeq(sampleUntilNonEmpty(ctToGen))).toMap
    val assetTokensToRedeem = assetHub.map(_ -> IndexedSeq(sampleUntilNonEmpty(ctToGen))).toMap
    val conversionSignatures = assetHub.map(_ -> IndexedSeq(sampleUntilNonEmpty(signatureGen))).toMap

    ConversionTransaction(totalAssetBoxes, assetsToReturn, assetTokensToRedeem, conversionSignatures, fee, timestamp, data)
  }*/

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
    PolyTransfer(from, to, signatures, fee, timestamp, data)
  }

  lazy val arbitTransferGen: Gen[ArbitTransfer] = for {
    from <- fromSeqGen
    to <- toSeqGen
    signatures <- sigSeqGen
    fee <- positiveLongGen
    timestamp <- positiveLongGen
    data <- stringGen
  } yield {
    ArbitTransfer(from, to, signatures, fee, timestamp, data)
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
    AssetTransfer(from, to, signatures, hub, assetCode, fee, timestamp, data)
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
    AssetCreation(to, signatures, assetCode, hub, fee, timestamp, data)
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

  val transactionTypes: Seq[Gen[BifrostTransaction]] =
    Seq(contractCreationGen, polyTransferGen, arbitTransferGen, profileTxGen)

  lazy val bifrostTransactionSeqGen: Gen[Seq[BifrostTransaction]] = for {
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

  lazy val bifrostBlockGen: Gen[BifrostBlock] = for {
    parentId <- specificLengthBytesGen(Block.BlockIdLength)
    timestamp <- positiveLongGen
    generatorBox <- arbitBoxGen
    signature <- signatureGen
    txs <- bifrostTransactionSeqGen
  } yield {
    BifrostBlock(parentId, timestamp, generatorBox, signature, txs)
  }

  lazy val bifrostSyncInfoGen: Gen[BifrostSyncInfo] = for {
    answer <- booleanGen
    score <- positiveLongGen
    numLastBlocks <- Gen.choose(1, 10)
  } yield {
    val lastBlockIds = (0 until numLastBlocks).map { _ => sampleUntilNonEmpty(modifierIdGen) }
    BifrostSyncInfo(answer, lastBlockIds, BigInt(score))
  }

  def generateHistory: BifrostHistory = {
    val dataDir = s"/tmp/scorex/scorextest-${Random.nextInt(10000000)}"

    val iFile = new File(s"$dataDir/blocks")
    iFile.mkdirs()
    val blockStorage = new LSMStore(iFile)

    val storage = new BifrostStorage(blockStorage, settings)
    //we don't care about validation here
    val validators = Seq()

    var history = new BifrostHistory(storage, settings, validators)

    val keyPair = sampleUntilNonEmpty(key25519Gen)
    val genesisBlock = BifrostBlock.create(
      settings.GenesisParentId,
      1478164225796L,
      Seq(),
      ArbitBox(keyPair._2, 0L, 0L),
      keyPair._1)

    history = history.append(genesisBlock).get._1
    assert(history.modifierById(genesisBlock.id).isDefined)
    history
  }

}
