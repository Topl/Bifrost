package bifrost

import examples.bifrost.contract._
import examples.bifrost.transaction.ContractCreation.Nonce
import examples.bifrost.transaction.{ContractCreation, StableCoinTransfer}
import examples.bifrost.transaction.box.ContractBox
import org.scalacheck.Gen
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.testkit.CoreGenerators

/**
  * Created by cykoz on 4/12/17.
  */
trait BifrostGenerators extends CoreGenerators {
  lazy val stringGen: Gen[String] = nonEmptyBytesGen.map(new String(_))

  lazy val base10gen: Gen[Int] = Gen.choose(0, 10)
  lazy val positiveTinyIntGen: Gen[Int] = Gen.choose(1,10)

  lazy val numStringGen = for {
    numDigits <- Gen.choose(0, 100)
  } yield (0 until numDigits).map {
    i => base10gen.sample.get
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
    i => (samplePositiveDouble, (samplePositiveDouble, samplePositiveDouble, samplePositiveDouble))
  }

  lazy val shareFuncGen: Gen[ShareFunction] = seqDoubleGen.map(new PiecewiseLinearMultiple(_))

  lazy val seqLongDoubleGen: Gen[Seq[(Long, Double)]] = for {
    seqLen <- positiveTinyIntGen
  } yield (0 until seqLen) map { i => (positiveLongGen.sample.get, samplePositiveDouble) }

  lazy val fulfilFuncGen: Gen[FulfilmentFunction] = seqLongDoubleGen.map(new PiecewiseLinearSingle(_))

  lazy val bifrostBoxGen: Gen[ContractBox] = for {
    proposition <- propositionGen
    nonce <- positiveLongGen
    value <- stringGen
  } yield ContractBox(proposition, nonce, value, "This is an Agreement")

  lazy val agreementTermsGen: Gen[AgreementTerms] = for {
    pledge <- bigDecimalGen
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
    parties <- partiesGen
    terms <- agreementTermsGen
    nonce <- positiveLongGen
    timestamp <- positiveLongGen
    expirationTimestamp <- positiveLongGen
  } yield Agreement(parties, terms, nonce, timestamp, expirationTimestamp)

  lazy val signatureGen: Gen[Signature25519] = genBytesList(Signature25519.SignatureSize).map(Signature25519(_))

  lazy val contractCreationGen: Gen[ContractCreation] = for {
    agreement <- agreementGen
    parties <- partiesGen
    numSigs <- smallInt
    signature <- signatureGen
    fee <- positiveLongGen
    timestamp <- positiveLongGen
  } yield ContractCreation(agreement, parties, (0 until numSigs) map { i => signature }, fee, timestamp)


  lazy val fromGen: Gen[(PublicKey25519Proposition, StableCoinTransfer.Nonce)] = for {
    proposition <- propositionGen
    nonce <- positiveLongGen
  } yield (proposition, nonce)

  lazy val fromSeqGen: Gen[IndexedSeq[(PublicKey25519Proposition, StableCoinTransfer.Nonce)]] = for {
    seqLen <- positiveTinyIntGen
  } yield (0 until seqLen) map { i => fromGen.sample.get }

  lazy val toGen: Gen[(PublicKey25519Proposition, StableCoinTransfer.Value)] = for {
    proposition <- propositionGen
    value <- positiveLongGen
  } yield (proposition, value)

  lazy val toSeqGen: Gen[IndexedSeq[(PublicKey25519Proposition, StableCoinTransfer.Value)]] = for {
    seqLen <- positiveTinyIntGen
  } yield (0 until seqLen) map { i => toGen.sample.get }

  lazy val sigSeqGen: Gen[IndexedSeq[Signature25519]] = for {
    seqLen <- positiveTinyIntGen
  } yield (0 until seqLen) map { i => signatureGen.sample.get }

  lazy val stableCoinTransferGen: Gen[StableCoinTransfer] = for {
    from <- fromSeqGen
    to <- toSeqGen
    signatures <- sigSeqGen
    fee <- positiveLongGen
    timestamp <- positiveLongGen
  } yield StableCoinTransfer(from, to, signatures, fee, timestamp)
}
