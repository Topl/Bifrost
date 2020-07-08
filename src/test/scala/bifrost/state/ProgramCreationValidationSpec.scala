package bifrost.state

import java.time.Instant
import java.util.UUID

import bifrost.crypto.{PrivateKey25519, PrivateKey25519Companion, Signature25519}
import bifrost.modifier.block.Block
import bifrost.modifier.box.proposition.PublicKey25519Proposition
import bifrost.modifier.box._
import bifrost.modifier.transaction.bifrostTransaction.ProgramCreation
import bifrost.modifier.transaction.bifrostTransaction.Transaction.Nonce
import bifrost.program.ExecutionBuilderCompanion
import com.google.common.primitives.{Bytes, Ints}
import io.circe.syntax._
import io.iohk.iodb.ByteArrayWrapper
import org.scalacheck.Gen
import scorex.crypto.signatures.Curve25519

import scala.util.Failure

/**
  * Created by Matt Kindy on 6/7/2017.
  */
class ProgramCreationValidationSpec extends ProgramSpec {

  //noinspection ScalaStyle
  def arbitraryPartyProgramCreationGen(num: Int): Gen[ProgramCreation] = for {
    executionBuilder <- validExecutionBuilderGen()
    timestamp <- positiveLongGen
    numFeeBoxes <- positiveTinyIntGen
    numInvestmentBoxes <- positiveTinyIntGen
    data <- stringGen
  } yield {
    val (priv: PrivateKey25519, owner: PublicKey25519Proposition) = keyPairSetGen.sample.get.head

    val preInvestmentBoxes: IndexedSeq[(Nonce, Long)] =
      (0 until numInvestmentBoxes).map { _ => positiveLongGen.sample.get -> positiveLongGen.sample.get }

    val feePreBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]] =
      Map(owner -> (0 until numFeeBoxes).map { _ => preFeeBoxGen().sample.get })

    val fees = feePreBoxes.map {
      case (prop, preBoxes) =>
        val providedFunds = preBoxes.map(_._2).sum
        val possibleFeeValue = providedFunds -
          Gen
            .choose(0L, boundedBy(providedFunds, 0, Long.MaxValue))
            .sample
            .get

        prop -> possibleFeeValue
    }

    val messageToSign = Bytes.concat(
      ExecutionBuilderCompanion.toBytes(executionBuilder),
      owner.pubKeyBytes,
      data.getBytes)
      //boxIdsToOpen.foldLeft(Array[Byte]())(_ ++ _))

    val signature = Map(owner -> PrivateKey25519Companion.sign(priv, messageToSign))

    val stateTwo =
      s"""
         |{ "b": 0 }
         """.stripMargin.asJson

    val stateThree =
      s"""
         |{ "c": 0 }
         """.stripMargin.asJson

    val stateBoxTwo = StateBox(owner, 1L, null, stateTwo)
    val stateBoxThree = StateBox(owner, 2L, null, stateThree)

    val readOnlyUUIDs = Seq(UUID.nameUUIDFromBytes(stateBoxTwo.id), UUID.nameUUIDFromBytes(stateBoxThree.id))

    ProgramCreation(
      executionBuilder,
      readOnlyUUIDs,
      preInvestmentBoxes,
      owner,
      signature,
      feePreBoxes,
      fees,
      timestamp,
      data)
  }

  private def boundedBy(sum: Long, min: Long, max: Long) = Math.max(min, Math.min(max, sum))

  property("A block with valid program creation will result " +
             "in a program entry and updated poly boxes in the LSMStore") {
    // Create block with program creation
    forAll(validProgramCreationGen) {
      programCreation: ProgramCreation =>
        val block = Block(
          Array.fill(Block.SignatureLength)(-1: Byte),
          Instant.now.toEpochMilli,
          ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
          Signature25519(Array.fill(Block.SignatureLength)(0: Byte)),
          Seq(programCreation),
          10L,
          settings.version
        )

        val preExistingPolyBoxes: Set[Box] = getPreExistingPolyBoxes(programCreation)

        val executionBox = programCreation.newBoxes.head.asInstanceOf[ExecutionBox]
        val stateBox = programCreation.newBoxes.drop(1).head.asInstanceOf[StateBox]
        val codeBox = programCreation.newBoxes.drop(2).head.asInstanceOf[CodeBox]
        val returnedPolyBoxes: Traversable[PolyBox] = programCreation.newBoxes.tail.drop(2).map {
          case p: PolyBox => p
          case _ => throw new Exception("Was expecting PolyBoxes but found something else")
        }

        val stateBoxBytes = StateBoxSerializer.toBytes(stateBox)
        val codeBoxBytes = CodeBoxSerializer.toBytes(codeBox)
        val executionBoxBytes = ExecutionBoxSerializer.toBytes(executionBox)

        val necessaryBoxesSC = StateChanges(
          Set(),
          preExistingPolyBoxes,
          Instant.now.toEpochMilli)

        val preparedState = StateSpec
          .genesisState
          .applyChanges(necessaryBoxesSC, Ints.toByteArray(23))
          .get

        val newState = preparedState
          .applyChanges(preparedState.changes(block).get, Ints.toByteArray(24))
          .get

        require(returnedPolyBoxes
                  .forall(pb => newState.storage.get(ByteArrayWrapper(pb.id)) match {
                    case Some(wrapper) => wrapper.data sameElements PolyBoxSerializer.toBytes(pb)
                    case None => false
                  }))

        require(newState.storage.get(ByteArrayWrapper(stateBox.id)) match {
          case Some(wrapper) => wrapper.data sameElements stateBoxBytes
          case None ⇒ false
        })

        require(newState.storage.get(ByteArrayWrapper(codeBox.id)) match {
          case Some(wrapper) => wrapper.data sameElements codeBoxBytes
          case None ⇒ false
        })

        require(newState.storage.get(ByteArrayWrapper(executionBox.id)) match {
          case Some(wrapper) => wrapper.data sameElements executionBoxBytes
          case None ⇒ false
        })

        /* Checks that the total sum of polys returned is total amount submitted minus total fees */
        returnedPolyBoxes.map(_.value).sum shouldEqual
          preExistingPolyBoxes
            .map { case pb: PolyBox => pb.value }
            .sum - programCreation.fee


        /* Checks that the amount returned in polys is equal to amount sent in less fees */
        programCreation.fees.foreach { case (prop, fee) =>

          val output = (returnedPolyBoxes collect { case pb: PolyBox if pb.proposition equals prop => pb.value }).sum

          val input = (preExistingPolyBoxes collect { case pb: PolyBox if pb.proposition equals prop =>
            pb.value }).sum

          val investment = 0

          output shouldEqual (input - fee - investment)
        }


        /* Expect none of the preexisting boxes to still be around */
        preExistingPolyBoxes
          .foreach(pb => newState.storage.get(ByteArrayWrapper(pb.id)) shouldBe empty)

        StateSpec.genesisState = newState
          .rollbackTo(StateSpec.genesisBlockId)
          .get
    }
  }

  property("Attempting to validate a program creation tx without valid signatures should error") {
    forAll(validProgramCreationGen) {
      programCreation: ProgramCreation =>

        val wrongSig: Array[Byte] = (programCreation.signatures.head._2.bytes.head + 1).toByte +:
          programCreation.signatures.head._2.bytes.tail

        val wrongSigs: Map[PublicKey25519Proposition, Signature25519] = programCreation.signatures +
          (programCreation.signatures.head._1 -> Signature25519(wrongSig))

        val invalidPC = programCreation.copy(signatures = wrongSigs)

        val preExistingPolyBoxes: Set[Box] = getPreExistingPolyBoxes(programCreation)

        val necessaryBoxesSC = StateChanges(
          Set(),
          preExistingPolyBoxes,
          Instant.now.toEpochMilli)

        val preparedState = StateSpec
          .genesisState
          .applyChanges(necessaryBoxesSC, Ints.toByteArray(25))
          .get

        val newState = preparedState.validate(invalidPC)

        StateSpec.genesisState = preparedState
          .rollbackTo(StateSpec.genesisBlockId)
          .get

        newState shouldBe a[Failure[_]]

        newState.failed.get.getMessage shouldBe "Incorrect unlocker"
    }
  }

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

        val preExistingPolyBoxes: Set[Box] = getPreExistingPolyBoxes(cc)

        val necessaryBoxesSC = StateChanges(Set(), preExistingPolyBoxes, cc.timestamp)

        val firstCCAddBlock = Block(
          Array.fill(Block.SignatureLength)(1: Byte),
          Instant.now.toEpochMilli,
          ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
          Signature25519(Array.fill(Block.SignatureLength)(0: Byte)),
          Seq(cc),
          10L,
          settings.version
        )

        val necessaryState = StateSpec
          .genesisState
          .applyChanges(necessaryBoxesSC, Ints.toByteArray(29))
          .get

        val preparedChanges = necessaryState.changes(firstCCAddBlock).get
        val preparedState = necessaryState
          .applyChanges(preparedChanges, Ints.toByteArray(30))
          .get
          .applyChanges(necessaryBoxesSC, Ints.toByteArray(31))
          .get

        val newState = preparedState.validate(cc)

        StateSpec.genesisState = preparedState
          .rollbackTo(StateSpec.genesisBlockId)
          .get

        newState shouldBe a[Failure[_]]
        newState.failed.get.getMessage shouldBe "ProgramCreation attempts to overwrite existing program"
    }
  }
}
