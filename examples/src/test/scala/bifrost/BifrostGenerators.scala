package bifrost

import examples.bifrost.contract._
import examples.bifrost.transaction.box.ContractBox
import org.scalacheck.Gen
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.testkit.CoreGenerators

/**
  * Created by cykoz on 4/12/17.
  */
trait BifrostGenerators extends CoreGenerators {
  lazy val stringGen = nonEmptyBytesGen.map(new String(_))

  lazy val doubleGen: Gen[Double] = Gen.choose(0, Double.MaxValue)

  lazy val bigDecimalGen: Gen[BigDecimal] = for {
    a <- doubleGen
  } yield BigDecimal.double2bigDecimal(a)

  //generate a num from smallInt for len of seq, map that many tuples, concatenate together into seq
  lazy val seqDoubleGen: Gen[Seq[(Double, (Double, Double, Double))]] = for {
      a <- doubleGen
      b <- doubleGen
      c <- doubleGen
      d <- doubleGen
    } yield Seq((a, (b,c,d)))

  lazy val shareFuncGen: Gen[ShareFunction] = seqDoubleGen.map(new PiecewiseLinearMultiple(_))

  lazy val seqLongDoubleGen: Gen[Seq[(Long, Double)]] = for {
    a <- positiveLongGen
    b <- doubleGen
  } yield Seq((a, b))

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
  } yield IndexedSeq(a)

  lazy val agreementGen: Gen[Agreement] = for {
    parties <- partiesGen
    terms <- agreementTermsGen
    nonce <- positiveLongGen
    timestamp <- positiveLongGen
    expirationTimestamp <- positiveLongGen
  } yield Agreement(parties, terms, nonce, timestamp, expirationTimestamp)
}
