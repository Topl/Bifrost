package bifrost

import java.io.File

import com.google.common.primitives.{Bytes, Longs}
import examples.bifrost.blocks.BifrostBlock
import examples.bifrost.contract._
import examples.bifrost.forging.ForgingSettings
import examples.bifrost.history.{BifrostHistory, BifrostStorage}
import examples.bifrost.transaction.box.proposition.MofNProposition
import examples.bifrost.transaction._
import examples.bifrost.transaction.box.{ContractBox, PolyBox}
import io.circe
import io.iohk.iodb.LSMStore
import org.scalacheck.{Arbitrary, Gen}
import scorex.core.block.Block
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.testkit.CoreGenerators

import scala.concurrent.duration._
import scala.util.Random

/**
  * Created by cykoz on 4/12/17.
  */
trait BifrostGenerators extends CoreGenerators {

  val settings = new ForgingSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile("settings.json")

    override lazy val Difficulty: BigInt = 1
  }


  lazy val stringGen: Gen[String] = nonEmptyBytesGen.map(new String(_))

  //noinspection ScalaStyle
  lazy val base10gen: Gen[Int] = Gen.choose(0,10)

  //noinspection ScalaStyle
  lazy val positiveTinyIntGen: Gen[Int] = Gen.choose(1,10)
  lazy val positiveMediumIntGen: Gen[Int] = Gen.choose(1,100)

  //noinspection ScalaStyle
  lazy val numStringGen: Gen[String] = for {
    numDigits <- Gen.choose(0, 100)
  } yield (0 until numDigits).map {
    _ => base10gen.sample.get
  }.foldLeft("")((a,b) => a + b)

  lazy val positiveDoubleGen: Gen[Double] = Gen.choose(0, Double.MaxValue)

  def samplePositiveDouble: Double = positiveDoubleGen.sample.get

  lazy val bigDecimalGen: Gen[BigDecimal] = for {
    wholeNumber <- numStringGen
    decimalPortion <- numStringGen
  } yield BigDecimal(wholeNumber + "." + decimalPortion)

  //generate a num from smallInt for len of seq, map that many tuples, concatenate together into seq
  lazy val seqDoubleGen: Gen[Seq[(Double, (Double, Double, Double))]] = for {
    seqLen <- positiveTinyIntGen
  } yield (0 until seqLen) map {
    _ => (samplePositiveDouble, (samplePositiveDouble, samplePositiveDouble, samplePositiveDouble))
  }

  lazy val shareFuncGen: Gen[ShareFunction] = seqDoubleGen.map(new PiecewiseLinearMultiple(_))

  lazy val seqLongDoubleGen: Gen[Seq[(Long, Double)]] = for {
    seqLen <- positiveTinyIntGen
  } yield (0 until seqLen) map { i => (positiveLongGen.sample.get, samplePositiveDouble) }

  lazy val fulfilFuncGen: Gen[FulfilmentFunction] = seqLongDoubleGen.map(new PiecewiseLinearSingle(_))

  lazy val contractBoxGen: Gen[ContractBox] = for {
    proposition <- oneOfNPropositionGen
    nonce <- positiveLongGen
    value <- stringGen
  } yield ContractBox(proposition._2, nonce, value)

  lazy val polyBoxGen: Gen[PolyBox] = for {
    proposition <- propositionGen
    nonce <- positiveLongGen
    value <- positiveLongGen
  } yield PolyBox(proposition, nonce, value)

  lazy val agreementTermsGen: Gen[AgreementTerms] = for {
    pledge <- positiveLongGen
    xrate <- bigDecimalGen
    share <- shareFuncGen
    fulfilment <- fulfilFuncGen
  } yield new AgreementTerms(pledge, xrate, share, fulfilment)

  lazy val partiesGen: Gen[IndexedSeq[PublicKey25519Proposition]] = for {
    a <- propositionGen
    b <- propositionGen
    c <- propositionGen
  } yield IndexedSeq(a, b, c)

  lazy val agreementGen: Gen[Agreement] = for {
    terms <- agreementTermsGen
    contractEndTime <- positiveLongGen
  } yield Agreement(terms, contractEndTime)

  lazy val validAgreementGen: Gen[Agreement] = for {
    terms <- agreementTermsGen
    timestamp <- positiveLongGen
  } yield Agreement(terms, timestamp)

  lazy val signatureGen: Gen[Signature25519] = genBytesList(Signature25519.SignatureSize).map(Signature25519(_))

  lazy val contractCreationGen: Gen[ContractCreation] = for {
    agreement <- agreementGen
    parties <- partiesGen
    signature <- signatureGen
    fee <- positiveLongGen
    timestamp <- positiveLongGen
  } yield ContractCreation(agreement, parties, parties.map { _ => signatureGen.sample.get }, fee, timestamp)


  lazy val validContractCreationGen: Gen[ContractCreation] = for {
    agreement <- validAgreementGen
    fee <- positiveLongGen
    timestamp <- positiveLongGen
  } yield {
    val allKeyPairs = (0 until 3).map(_ => keyPairSetGen.sample.get.head)
    val parties = allKeyPairs.map(_._2)
    val messageToSign = Bytes.concat(
      Longs.toByteArray(timestamp),
      AgreementCompanion.toBytes(agreement),
      parties.foldLeft(Array[Byte]())((a, b) => a ++ b.pubKeyBytes)
    )
    val signatures = allKeyPairs.map(
      keypair =>
        PrivateKey25519Companion.sign(keypair._1, messageToSign)
    )
    ContractCreation(agreement, parties, signatures, fee, timestamp)
  }

  lazy val fromGen: Gen[(PublicKey25519Proposition, PolyTransfer.Nonce)] = for {
    proposition <- propositionGen
    nonce <- positiveLongGen
  } yield (proposition, nonce)

  lazy val fromSeqGen: Gen[IndexedSeq[(PublicKey25519Proposition, PolyTransfer.Nonce)]] = for {
    seqLen <- positiveTinyIntGen
  } yield (0 until seqLen) map { _ => fromGen.sample.get }

  lazy val toGen: Gen[(PublicKey25519Proposition, PolyTransfer.Value)] = for {
    proposition <- propositionGen
    value <- positiveLongGen
  } yield (proposition, value)

  lazy val toSeqGen: Gen[IndexedSeq[(PublicKey25519Proposition, PolyTransfer.Value)]] = for {
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

  lazy val validPolyTransferGen: Gen[PolyTransfer] = for {
    from <- fromSeqGen
    to <- toSeqGen
    fee <- positiveLongGen
    timestamp <- positiveLongGen
  } yield {
    val fromKeyPairs = keyPairSetGen.sample.get.head
    val from = IndexedSeq((fromKeyPairs._1, Longs.fromByteArray(FastCryptographicHash("Testing").take(8))))
    val toKeyPairs = keyPairSetGen.sample.get.head
    val to = IndexedSeq((toKeyPairs._2, 4L))

    PolyTransfer(from, to, fee, timestamp)
  }

  lazy val oneOfNPropositionGen: Gen[(Set[PrivateKey25519], MofNProposition)] = for {
    n <- positiveTinyIntGen
  } yield {
    var keySet = Set[PrivateKey25519]()
    val prop = MofNProposition(
      1, (0 until n).map(i =>{
        val key = key25519Gen.sample.get
        keySet += key._1
        key._2.pubKeyBytes
      }).foldLeft(Set[Array[Byte]]())((set, cur) => set + cur)
    )

    (keySet, prop)
  }

  lazy val keyPairSetGen: Gen[Set[(PrivateKey25519, PublicKey25519Proposition)]] = for {
    seqLen <- positiveTinyIntGen
  } yield ((0 until seqLen) map { _ => key25519Gen.sample.get }).toSet

  val transactionTypes: Seq[String] = Seq() :+ "ContractCreation" :+ "PolyTransfer"

  lazy val bifrostTransactionSeqGen: Gen[Seq[BifrostTransaction]] = for {
    seqLen <- positiveMediumIntGen
  } yield 0 until seqLen map {
    _ => Gen.oneOf(transactionTypes).sample.get match {
      case "ContractCreation" => contractCreationGen.sample.get
      case "PolyTransfer" => polyTransferGen.sample.get
    }
  }

  def specificLengthBytesGen(length: Int): Gen[Array[Byte]] = Gen.listOfN(length, Arbitrary.arbitrary[Byte]).map(_.toArray)

  lazy val bifrostBlockGen: Gen[BifrostBlock] = for {
    parentId <- specificLengthBytesGen(Block.BlockIdLength)
    timestamp <- positiveLongGen
    generatorBox <- polyBoxGen
    signature <- signatureGen
    txs <- bifrostTransactionSeqGen
  } yield BifrostBlock(parentId, timestamp, generatorBox, signature, txs)

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
    val genesisBlock = BifrostBlock.create(settings.GenesisParentId, 1478164225796L, Seq(), PolyBox(keyPair._2, 0L, 0L), keyPair._1)

    history = history.append(genesisBlock).get._1
    assert(history.modifierById(genesisBlock.id).isDefined)
    history
  }

}
