package bifrost

import java.io.File
import java.time.Instant

import bifrost.blocks.BifrostBlock
import bifrost.contract.{Contract, _}
import bifrost.forging.ForgingSettings
import bifrost.history.{BifrostHistory, BifrostStorage, BifrostSyncInfo}
import bifrost.transaction.BifrostTransaction.{Nonce, Value}
import bifrost.transaction.Role.Role
import bifrost.transaction.box.proposition.MofNProposition
import bifrost.transaction._
import bifrost.transaction.box._
import io.circe
import io.circe.Json
import io.circe.syntax._
import io.iohk.iodb.LSMStore
import org.scalacheck.{Arbitrary, Gen}
import scorex.core.block.Block
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.crypto.encode.Base58
import scorex.testkit.CoreGenerators

import scala.util.{Random, Try}

/**
  * Created by cykoz on 4/12/17.
  */
trait BifrostGenerators extends CoreGenerators {

  val settings = new ForgingSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile("settings.json")

    override lazy val Difficulty: BigInt = 1
  }

  def unfoldLeft[A,B](seed: B)(f: B => Option[(A, B)]): Seq[A] = {
    f(seed) match {
      case Some((a, b)) => a +: unfoldLeft(b)(f)
      case None => Nil
    }
  }

  def splitAmongN(toSplit: Long, n: Int, minShareSize: Long = Long.MinValue, maxShareSize: Long = Long.MaxValue): Try[Seq[Long]] = Try {
    unfoldLeft((toSplit, n)) { case (amountLeft: Long, shares: Int) =>
      if(shares > 0) {

        val longRange = BigInt(Long.MaxValue) - BigInt(Long.MinValue)
        val canOverflowOrUnderflowFully = BigInt(shares - 1)*(BigInt(maxShareSize) - BigInt(minShareSize)) >= longRange

        var noMoreThan: Long = ((BigInt(amountLeft) - BigInt(shares - 1)*BigInt(minShareSize)) % longRange).toLong
        var noLessThan: Long = ((BigInt(amountLeft) - BigInt(shares - 1)*BigInt(maxShareSize)) % longRange).toLong

        if(canOverflowOrUnderflowFully) {
          noMoreThan = maxShareSize
          noLessThan = minShareSize
        }

        var thisPortion: Long = 0L

        if(noLessThan <= maxShareSize && noMoreThan >= minShareSize && noLessThan <= noMoreThan)
          thisPortion = Math.min(Math.max(Gen.choose(noLessThan, noMoreThan).sample.get, minShareSize), maxShareSize)
        else if(noLessThan <= maxShareSize)
          thisPortion = Math.max(Gen.choose(noLessThan, maxShareSize).sample.get, minShareSize)
        else if(noMoreThan >= minShareSize)
          thisPortion = Math.min(Gen.choose(minShareSize, noMoreThan).sample.get, maxShareSize)
        else
          throw new Exception("Cannot split")

        Some(thisPortion, (amountLeft - thisPortion, shares - 1))
      } else None
    }
  }

  lazy val stringGen: Gen[String] = nonEmptyBytesGen.map(new String(_))

  //noinspection ScalaStyle
  lazy val base10gen: Gen[Int] = Gen.choose(0,10)

  val jsonTypes: Seq[String] = Seq("Object", "Array", "Boolean", "String", "Number")

  lazy val jsonTypeGen: Gen[String] = Gen.oneOf(jsonTypes)

  def jsonGen(depth: Int = 0): Gen[Json] = for {
    numFields <- positiveTinyIntGen
  } yield ((0 until numFields) map { _ => stringGen.sample.get -> (
    jsonTypeGen.sample.get match {
      case "Object" if depth < 2 => jsonGen(depth + 1).sample.get
      case "Array" if depth < 3 => jsonArrayGen(depth + 1).sample.get
      case "Boolean" => Gen.oneOf(Seq(true, false)).sample.get.asJson
      case "String" => stringGen.sample.get.asJson
      case "Number" => positiveDoubleGen.sample.get.asJson
      case _ => stringGen.sample.get.asJson
    })
  } toMap).asJson

  def jsonArrayGen(depth: Int = 0): Gen[Json] = for {
    numFields <- positiveTinyIntGen
  } yield ((0 until numFields) map { _ =>
    jsonTypeGen.sample.get match {
      case "Object" if depth < 2 => jsonGen(depth + 1).sample.get
      case "Array" if depth < 3 => jsonArrayGen(depth + 1).sample.get
      case "Boolean" => Gen.oneOf(Seq(true, false)).sample.get.asJson
      case "String" => stringGen.sample.get.asJson
      case "Number" => positiveDoubleGen.sample.get.asJson
      case _ => stringGen.sample.get.asJson
    }
  }).asJson

  //noinspection ScalaStyle
  lazy val positiveTinyIntGen: Gen[Int] = Gen.choose(1,10)
  lazy val positiveMediumIntGen: Gen[Int] = Gen.choose(1,100)
  lazy val booleanGen: Gen[Boolean] = Random.nextBoolean()

  //noinspection ScalaStyle
  lazy val numStringGen: Gen[String] = for {
    numDigits <- Gen.choose(0, 78)
  } yield (0 until numDigits).map {
    _ => base10gen.sample.get
  }.foldLeft(Gen.choose(1,9).sample.get.toString)((a,b) => a + b) + Gen.choose(1,9).sample.get.toString

  lazy val positiveDoubleGen: Gen[Double] = Gen.choose(0, Double.MaxValue)

  def samplePositiveDouble: Double = Random.nextFloat()

  lazy val smallBigDecimalGen: Gen[BigDecimal] = for {
    decimalPortion <- numStringGen
  } yield BigDecimal("0." + decimalPortion)

  lazy val bigDecimalGen: Gen[BigDecimal] = for {
    wholeNumber <- numStringGen
    decimalPortion <- numStringGen
  } yield BigDecimal(wholeNumber + "." + decimalPortion)

  //generate a num from smallInt for len of seq, map that many tuples, concatenate together into seq
  def seqDoubleGen(minLength: Int): Gen[Seq[(Double, (Double, Double, Double))]] = for {
    seqLen <- Gen.choose(minLength, minLength + positiveTinyIntGen.sample.get)
  } yield (0 until seqLen) map {
    val first = samplePositiveDouble / 2; val second = samplePositiveDouble / 2
    _ => (samplePositiveDouble, (first, second, 1 - first - second))
  }

  lazy val shareFuncGen: Gen[ShareFunction] = seqDoubleGen(2).map(PiecewiseLinearMultiple)

  def seqLongDoubleGen(minLength: Int): Gen[Seq[(Long, Double)]] = for {
    seqLen <- Gen.choose(minLength, minLength + positiveTinyIntGen.sample.get)
  } yield (0 until seqLen) map { _ => (positiveLongGen.sample.get, samplePositiveDouble) }

  lazy val fulfilFuncGen: Gen[FulfilmentFunction] = seqLongDoubleGen(2).map(PiecewiseLinearSingle)

  lazy val polyBoxGen: Gen[PolyBox] = for {
    proposition <- propositionGen
    nonce <- positiveLongGen
    value <- positiveLongGen
  } yield PolyBox(proposition, nonce, value)

  lazy val arbitBoxGen: Gen[ArbitBox] = for {
    proposition <- propositionGen
    nonce <- positiveLongGen
    value <- positiveLongGen
  } yield ArbitBox(proposition, nonce, value)

  lazy val assetBoxGen: Gen[AssetBox] = for {
    proposition <- propositionGen
    nonce <- positiveLongGen
    value <- positiveLongGen
    asset <- stringGen
    hub <- propositionGen
  } yield AssetBox(proposition, nonce, value, asset, hub)

  val doubleGen: Gen[Double] = Gen.choose(Double.MinValue, Double.MaxValue)

  lazy val reputationBoxGen: Gen[ReputationBox] = for {
    proposition <- propositionGen
    nonce <- positiveLongGen
  } yield ReputationBox(proposition, nonce, (doubleGen.sample.get, doubleGen.sample.get))

  lazy val profileBoxGen: Gen[ProfileBox] = for {
    proposition <- propositionGen
    value <- stringGen
    field <- stringGen
  } yield ProfileBox(proposition, 0L, value, field)

  lazy val partiesGen: Gen[Map[Role, PublicKey25519Proposition]] = for {
    a <- propositionGen
    b <- propositionGen
    c <- propositionGen
  } yield Map(Role.Producer -> a, Role.Hub -> b, Role.Investor -> c)

  lazy val validShareFuncGen: Gen[ShareFunction] = seqDoubleGen(positiveTinyIntGen.sample.get).map(seq => {
    val first: Double = samplePositiveDouble / 2
    val second: Double = samplePositiveDouble / 2
    PiecewiseLinearMultiple((0.0, (first, second, 1 - first - second)) +: seq)
  })

  lazy val validFulfilFuncGen: Gen[FulfilmentFunction] = seqLongDoubleGen(positiveTinyIntGen.sample.get).map(seq =>
    PiecewiseLinearSingle((0L, samplePositiveDouble) +: seq)
  )

  lazy val validAgreementTermsGen: Gen[AgreementTerms] = for {
    pledge <- positiveLongGen.map(_/1e10.toLong + 1L)
    xrate <- smallBigDecimalGen
    share <- validShareFuncGen
    fulfilment <- validFulfilFuncGen
  } yield new AgreementTerms(pledge, xrate, share, fulfilment)

  lazy val validAgreementGen: Gen[Agreement] = for {
    assetCode <- stringGen
    terms <- validAgreementTermsGen
    delta <- positiveLongGen
  } yield Agreement(terms, assetCode, Instant.now.toEpochMilli + 10000L, Instant.now.toEpochMilli + 10000L + delta)

  lazy val signatureGen: Gen[Signature25519] = genBytesList(Signature25519.SignatureSize).map(Signature25519(_))

  lazy val contractGen: Gen[Contract] = for {
    producer <- propositionGen
    investor <- propositionGen
    hub <- propositionGen
    storage <- jsonGen()
    status <- jsonGen()
    agreement <- validAgreementGen.map(_.json)
    id <- genBytesList(FastCryptographicHash.DigestSize)
  } yield Contract(Map(
    "producer" -> Base58.encode(producer.pubKeyBytes).asJson,
    "investor" -> Base58.encode(investor.pubKeyBytes).asJson,
    "hub" -> Base58.encode(hub.pubKeyBytes).asJson,
    "storage" -> Map("status" -> status, "other" -> storage).asJson,
    "agreement" -> agreement,
    "lastUpdated" -> System.currentTimeMillis().asJson
  ).asJson, id)

  lazy val contractBoxGen: Gen[ContractBox] = for {
    proposition <- oneOfNPropositionGen
    nonce <- positiveLongGen
    value <- contractGen.map(_.json)
  } yield ContractBox(proposition._2, nonce, value)

  def preFeeBoxGen(minFee: Long = 0, maxFee: Long = Long.MaxValue): Gen[(Nonce, Long)] = for {
    nonce <- Gen.choose(Long.MinValue, Long.MaxValue)
    amount <- Gen.choose(minFee, maxFee)
  } yield (nonce, amount)

  lazy val contractCreationGen: Gen[ContractCreation] = for {
    agreement <- validAgreementGen
    numInvestmentBoxes <- positiveTinyIntGen
    parties <- partiesGen
    signature <- signatureGen
    numFeeBoxes <- positiveTinyIntGen
    timestamp <- positiveLongGen
  } yield ContractCreation(
    agreement,
    (0 until numInvestmentBoxes).map { _ => positiveLongGen.sample.get -> positiveLongGen.sample.get },
    parties,
    parties.map { case (_, v) => (v, signatureGen.sample.get) },
    parties.map { case (_, v) => v -> (0 until numFeeBoxes).map { _ => preFeeBoxGen().sample.get} },
    parties.map { case (_, v) => v -> positiveTinyIntGen.sample.get.toLong },
    timestamp
  )

  lazy val contractMethodExecutionGen: Gen[ContractMethodExecution] = for {
    contract <- contractBoxGen
    methodName <- stringGen
    parameters <- jsonArrayGen()
    sig <- signatureGen
    numFeeBoxes <- positiveTinyIntGen
    fee <- positiveLongGen
    timestamp <- positiveLongGen
    party <- propositionGen
  } yield ContractMethodExecution(
    contract,
    methodName,
    parameters,
    Map(Gen.oneOf(Role.values.toSeq).sample.get -> party),
    Map(party -> sig),
    Map(party -> (0 until numFeeBoxes).map { _ => preFeeBoxGen().sample.get}),
    Map(party -> positiveTinyIntGen.sample.get.toLong),
    timestamp
  )

  lazy val contractCompletionGen: Gen[ContractCompletion] = for {
    contract <- contractBoxGen
    parties <- partiesGen
    signature <- signatureGen
    fee <- positiveLongGen
    numFeeBoxes <- positiveTinyIntGen
    timestamp <- positiveLongGen
  } yield ContractCompletion(
    contract,
    IndexedSeq(),
    parties,
    parties.map { case (_, v) => (v, signatureGen.sample.get) },
    parties.map { case (_, v) => v -> (0 until numFeeBoxes).map { _ => preFeeBoxGen().sample.get} },
    parties.map { case (_, v) => v -> positiveTinyIntGen.sample.get.toLong },
    timestamp
  )

  lazy val profileTxGen: Gen[ProfileTransaction] = for {
    from <- propositionGen
    numKeys <- positiveMediumIntGen
    fee <- positiveLongGen
    timestamp <- positiveLongGen
  } yield {
    val signature = signatureGen.sample.get
    val keyValues = (0 until numKeys).map { _ => (stringGen.sample.get, stringGen.sample.get)}.foldLeft[Map[String, String]](Map())((a, b) => a + b )
    ProfileTransaction(from, signature, keyValues, fee, timestamp)
  }

  lazy val assetRedemptionGen: Gen[AssetRedemption] = for {
    assetLength <- positiveTinyIntGen
    hub <- propositionGen
    fee <- positiveLongGen
    timestamp <- positiveLongGen
  } yield {

    val assets = (0 until assetLength).map { _ =>
      stringGen.sample.get
    }

    val availableToRedeem = assets.map(_ -> fromSeqGen.sample.get).toMap
    val remainderAllocations = assets.map(_ -> toSeqGen.sample.get).toMap

    val signatures = availableToRedeem.map { case (assetId, boxes) =>
      assetId -> boxes.map(_ => signatureGen.sample.get)
    }

    AssetRedemption(availableToRedeem, remainderAllocations, signatures, hub, fee, timestamp)
  }


  lazy val fromGen: Gen[(PublicKey25519Proposition, Nonce)] = for {
    proposition <- propositionGen
    nonce <- positiveLongGen
  } yield (proposition, nonce)

  lazy val fromSeqGen: Gen[IndexedSeq[(PublicKey25519Proposition, Nonce)]] = for {
    seqLen <- positiveTinyIntGen
  } yield (0 until seqLen) map { _ => fromGen.sample.get }

  lazy val toGen: Gen[(PublicKey25519Proposition, Long)] = for {
    proposition <- propositionGen
    value <- positiveLongGen
  } yield (proposition, value)

  lazy val toSeqGen: Gen[IndexedSeq[(PublicKey25519Proposition, Value)]] = for {
    seqLen <- positiveTinyIntGen
  } yield (0 until seqLen) map { _ => toGen.sample.get }

  lazy val sigSeqGen: Gen[IndexedSeq[Signature25519]] = for {
    seqLen <- positiveTinyIntGen
  } yield (0 until seqLen) map { _ => signatureGen.sample.get }

  lazy val polyTransferGen: Gen[PolyTransfer] = for {
    from <- fromSeqGen
    to <- toSeqGen
    signatures <- sigSeqGen
    fee <- positiveLongGen
    timestamp <- positiveLongGen
  } yield PolyTransfer(from, to, signatures, fee, timestamp)

  lazy val arbitTransferGen: Gen[ArbitTransfer] = for {
    from <- fromSeqGen
    to <- toSeqGen
    signatures <- sigSeqGen
    fee <- positiveLongGen
    timestamp <- positiveLongGen
  } yield ArbitTransfer(from, to, signatures, fee, timestamp)

  lazy val assetTransferGen: Gen[AssetTransfer] = for {
    from <- fromSeqGen
    to <- toSeqGen
    signatures <- sigSeqGen
    fee <- positiveLongGen
    timestamp <- positiveLongGen
    hub <- propositionGen
    assetCode <- stringGen
  } yield AssetTransfer(from, to, signatures, hub, assetCode, fee, timestamp)

  lazy val oneOfNPropositionGen: Gen[(Set[PrivateKey25519], MofNProposition)] = for {
    n <- positiveTinyIntGen
  } yield {
    val setOfKeys = (0 until n).map(i => {
      val key = key25519Gen.sample.get
      (key._1, key._2.pubKeyBytes)
    }).foldLeft((Set[PrivateKey25519](), Set[Array[Byte]]())) {
      case (set: (Set[PrivateKey25519], Set[Array[Byte]]), cur: (PrivateKey25519, Array[Byte])) =>
        (set._1 + cur._1, set._2 + cur._2)
    }
    val prop = MofNProposition(1, setOfKeys._2)

    (setOfKeys._1, prop)
  }

  lazy val keyPairSetGen: Gen[Set[(PrivateKey25519, PublicKey25519Proposition)]] = for {
    seqLen <- positiveTinyIntGen
  } yield ((0 until seqLen) map { _ => key25519Gen.sample.get }).toSet

  val transactionTypes: Seq[String] = Seq("ContractCreation", "PolyTransfer", "ArbitTransfer","ProfileTransaction")

  lazy val bifrostTransactionSeqGen: Gen[Seq[BifrostTransaction]] = for {
    seqLen <- positiveMediumIntGen
  } yield 0 until seqLen map {
    _ => Gen.oneOf(transactionTypes).sample.get match {
      case "ContractCreation" => contractCreationGen.sample.get
      case "PolyTransfer" => polyTransferGen.sample.get
      case "ArbitTransfer" => arbitTransferGen.sample.get
      case "ProfileTransaction" => profileTxGen.sample.get
    }
  }

  def specificLengthBytesGen(length: Int): Gen[Array[Byte]] = Gen.listOfN(length, Arbitrary.arbitrary[Byte]).map(_.toArray)

  lazy val bifrostBlockGen: Gen[BifrostBlock] = for {
    parentId <- specificLengthBytesGen(Block.BlockIdLength)
    timestamp <- positiveLongGen
    generatorBox <- arbitBoxGen
    signature <- signatureGen
    txs <- bifrostTransactionSeqGen
  } yield BifrostBlock(parentId, timestamp, generatorBox, signature, txs)

  lazy val bifrostSyncInfoGen: Gen[BifrostSyncInfo] = for {
    answer <- booleanGen
    score <- positiveLongGen
    numLastBlocks <- Gen.choose(1,10)
  } yield {
    val lastBlockIds = (0 until numLastBlocks).map{ _ => modifierIdGen.sample.get}
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

    val keyPair = key25519Gen.sample.get
    val genesisBlock = BifrostBlock.create(settings.GenesisParentId, 1478164225796L, Seq(), ArbitBox(keyPair._2, 0L, 0L), keyPair._1)

    history = history.append(genesisBlock).get._1
    assert(history.modifierById(genesisBlock.id).isDefined)
    history
  }

}
