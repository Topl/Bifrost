package co.topl.nodeView.state

import java.time.Instant

import co.topl.crypto.Signature25519
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.ProgramCreation
import co.topl.nodeView.state.box._
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import scorex.crypto.signatures.Curve25519

import scala.util.Failure

/**
  * Created by Matt Kindy on 6/7/2017.
  */
class ProgramCreationValidationSpec extends ProgramSpec {

//  //noinspection ScalaStyle
//  def arbitraryPartyProgramCreationGen(num: Int): Gen[ProgramCreation] = for {
//    executionBuilder <- validExecutionBuilderGen()
//    timestamp <- positiveLongGen
//    numFeeBoxes <- positiveTinyIntGen
//    numInvestmentBoxes <- positiveTinyIntGen
//    data <- stringGen
//  } yield {
//    val (priv: PrivateKey25519, owner: PublicKey25519Proposition) = keyPairSetGen.sample.get.head
//
//    val preInvestmentBoxes: IndexedSeq[(Nonce, Long)] =
//      (0 until numInvestmentBoxes).map { _ => positiveLongGen.sample.get -> positiveLongGen.sample.get }
//
//    val feePreBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]] =
//      Map(owner -> (0 until numFeeBoxes).map { _ => preFeeBoxGen().sample.get })
//
//    val fees = feePreBoxes.map {
//      case (prop, preBoxes) =>
//        val providedFunds = preBoxes.map(_._2).sum
//        val possibleFeeValue = providedFunds -
//          Gen
//            .choose(0L, boundedBy(providedFunds, 0, Long.MaxValue))
//            .sample
//            .get
//
//        prop -> possibleFeeValue
//    }
//
//    val messageToSign = Bytes.concat(
//      ExecutionBuilderSerializer.toBytes(executionBuilder),
//      owner.pubKeyBytes,
//      data.getBytes)
//      //boxIdsToOpen.foldLeft(Array[Byte]())(_ ++ _))
//
//    val signature = Map(owner -> PrivateKey25519.sign(priv, messageToSign))
//
//    val stateTwo =
//      s"""
//         |{ "b": 0 }
//         """.stripMargin.asJson
//
//    val stateThree =
//      s"""
//         |{ "c": 0 }
//         """.stripMargin.asJson
//
////    val stateBoxTwo = StateBox(owner, 1L, null, stateTwo)
////    val stateBoxThree = StateBox(owner, 2L, null, stateThree)
//
//    val readOnlyIds = Seq(programIdGen.sample.get, programIdGen.sample.get)
//
//    ProgramCreation(
//      executionBuilder,
//      readOnlyIds,
//      preInvestmentBoxes,
//      owner,
//      signature,
//      feePreBoxes,
//      fees,
//      timestamp,
//      data)
//  }
//
//  private def boundedBy(sum: Long, min: Long, max: Long) = Math.max(min, Math.min(max, sum))

/*  property("A block with valid program creation will result " +
             "in a program entry and updated poly boxes in the LSMStore") {
    // Create block with program creation
    forAll(validProgramCreationGen) {
      programCreation: ProgramCreation =>
        val block = Block(
          ModifierId(Array.fill(Block.signatureLength)(-1: Byte)),
          Instant.now.toEpochMilli,
          ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
          Signature25519(Array.fill(Block.signatureLength)(0: Byte)),
          Seq(programCreation),
          settings.forgingSettings.version
        )

        //val preExistingPolyBoxes: Set[Box] = getPreExistingPolyBoxes(programCreation)

        val executionBox = programCreation.newBoxes.head.asInstanceOf[ExecutionBox]
        val stateBox = programCreation.newBoxes.drop(1).head.asInstanceOf[StateBox]
        val codeBox = programCreation.newBoxes.drop(2).head.asInstanceOf[CodeBox]
//        val returnedPolyBox: PolyBox = programCreation.newBoxes.last match {
//          case p: PolyBox => p
//          case _ => throw new Exception("Was expecting PolyBoxes but found something else")
//        }

        val stateBoxBytes = BoxSerializer.toBytes(stateBox)
        val codeBoxBytes = BoxSerializer.toBytes(codeBox)
        val executionBoxBytes = BoxSerializer.toBytes(executionBox)
        //val returnedPolyBoxBytes = BoxSerializer.toBytes(returnedPolyBox)

        //val necessaryBoxesSC = StateChanges(Set(), preExistingPolyBoxes)

//        val preparedState = StateSpec
//          .genesisState
//          .applyChanges(ModifierId(Ints.toByteArray(23)), necessaryBoxesSC)
//          .get

        val newState = StateSpec
          .genesisState
          .applyModifier(block)
          .get

//        require(newState.getBox(returnedPolyBox.id) match {
//                  case Some(box) => box.bytes sameElements returnedPolyBoxBytes
//                  case None => false
//                })

        require(newState.getBox(stateBox.id) match {
          case Some(box) => box.bytes sameElements stateBoxBytes
          case None ⇒ false
        })

        require(newState.getBox(codeBox.id) match {
          case Some(box) => box.bytes sameElements codeBoxBytes
          case None ⇒ false
        })

        require(newState.getBox(executionBox.id) match {
          case Some(box) => box.bytes sameElements executionBoxBytes
          case None ⇒ false
        })

        /* Checks that the total sum of polys returned is total amount submitted minus total fees */
//        returnedPolyBox.value shouldEqual
//          preExistingPolyBoxes
//            .map { case pb: PolyBox => pb.value }
//            .sum - programCreation.fee


        /* Checks that the amount returned in polys is equal to amount sent in less fees */
//        programCreation.fees.foreach { case (prop, fee) =>
//          val output = if (returnedPolyBox.proposition equals prop) returnedPolyBox.value else 0
//          val input = (preExistingPolyBoxes collect { case pb: PolyBox if pb.proposition equals prop => pb.value }).sum
//          val investment = 0
//
//          output shouldEqual (input - fee - investment)
//        }


        /* Expect none of the preexisting boxes to still be around */
//        preExistingPolyBoxes
//          .foreach(pb => newState.getBox(pb.id) shouldBe None)

        StateSpec.genesisState = newState
          .rollbackTo(StateSpec.genesisBlockId)
          .get
    }
  }*/

/*  property("Attempting to validate a program creation tx without valid signatures should error") {
    forAll(validProgramCreationGen) {
      programCreation: ProgramCreation =>

        val wrongSig: Array[Byte] = (programCreation.signatures.head._2.bytes.head + 1).toByte +:
          programCreation.signatures.head._2.bytes.tail

        val wrongSigs: Map[PublicKey25519Proposition, Signature25519] = programCreation.signatures +
          (programCreation.signatures.head._1 -> Signature25519(wrongSig))

        val invalidPC = programCreation.copy(signatures = wrongSigs)

//        val preExistingPolyBoxes: Set[Box] = getPreExistingPolyBoxes(programCreation)
//        val necessaryBoxesSC = StateChanges(Set(), preExistingPolyBoxes)

//        val preparedState = StateSpec
//          .genesisState
//          .applyChanges(ModifierId(Ints.toByteArray(25)), necessaryBoxesSC)
//          .get

        val newState = StateSpec
          .genesisState.validate(invalidPC)

//        StateSpec.genesisState = preparedState
//          .rollbackTo(StateSpec.genesisBlockId)
//          .get

        newState shouldBe a[Failure[_]]

        newState.failed.get.getMessage shouldBe "Incorrect unlocker"
    }
  }*/

  //noinspection ScalaStyle
//  property("Attempting to validate a program creation tx with too many signatures (versus parties) should error") {
//
//    forAll(arbitraryPartyProgramCreationGen(Gen.choose(4, 10).sample.get)) {
//      cc: ProgramCreation =>
//        val roles = Role.Investor +: Random.shuffle(List(Role.Producer, Role.Hub))
//
//        val preExistingPolyBoxes: Set[Box] = getPreExistingPolyBoxes(cc) // TODO(balinskia): Which party is the investor
//      val profileBoxes: Set[ProfileBox] = constructProfileBoxes(cc, roles)
//
//        val necessaryBoxesSC = BifrostStateChanges(
//          Set(),
//          preExistingPolyBoxes ++ profileBoxes,
//          Instant.now.toEpochMilli)
//
//        val preparedState = BifrostStateSpec
//          .genesisState
//          .applyChanges(necessaryBoxesSC, Ints.toByteArray(2))
//          .get
//
//        val newState = preparedState.validate(cc)
//
//        BifrostStateSpec.genesisState = preparedState
//          .rollbackTo(BifrostStateSpec.genesisBlockId)
//          .get
//
//        newState shouldBe a[Failure[_]]
//        newState.failed.get.getMessage shouldBe "Too many signatures for the parties of this transaction"
//    }
//  }

  //TODO Add back in when timestamps will be used for Programs
/*
  property(
    "Attempting to validate a program creation tx with a timestamp that is before the last block timestamp should error")
  {
    forAll(validProgramCreationGen) {
      cc: ProgramCreation =>

        val preExistingPolyBoxes: Set[Box] = getPreExistingPolyBoxes(cc)

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingPolyBoxes,
          cc.timestamp + Gen.choose(1L, Long.MaxValue - cc.timestamp - 1L).sample.get)

        val preparedState = BifrostStateSpec
          .genesisState.
          applyChanges(necessaryBoxesSC, Ints.toByteArray(28))
          .get

        val newState = preparedState.validate(cc)

        BifrostStateSpec.genesisState = preparedState
          .rollbackTo(BifrostStateSpec.genesisBlockId)
          .get

        newState shouldBe a[Failure[_]]
        newState.failed.get.getMessage shouldBe "ProgramCreation attempts to write into the past"
    }
  }
*/

  property("Attempting to validate a program creation tx " +
             "with the same id as an existing program should error") {
    forAll(validProgramCreationGen) {
      cc: ProgramCreation =>


        val firstCCAddBlock = Block(
          ModifierId(Array.fill(Block.signatureLength)(1: Byte)),
          Instant.now.toEpochMilli,
          ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), scala.util.Random.nextLong(), 0L),
          Signature25519(Array.fill(Block.signatureLength)(0: Byte)),
          Seq(cc),
          settings.forgingSettings.version
        )

        val necessaryState = StateSpec
          .genesisState()
          .applyModifier(firstCCAddBlock)
          .get

        val newState = necessaryState.validate(cc)

        newState shouldBe a[Failure[_]]
        newState.failed.get.getMessage shouldBe "ProgramCreation attempts to overwrite existing program"
    }
  }
}
