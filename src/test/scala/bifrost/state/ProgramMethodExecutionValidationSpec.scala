package bifrost.state

import java.time.Instant
import java.util.UUID

import bifrost.block.Block
import bifrost.crypto.{FastCryptographicHash, PrivateKey25519Companion}
import bifrost.program.Program
import bifrost.exceptions.JsonParsingException
import bifrost.forging.ForgingSettings
import bifrost.programBoxRegistry.ProgramBoxRegistryOld
import bifrost.transaction.box.{PublicKeyNoncedBox, _}
import bifrost.transaction.bifrostTransaction.ProgramMethodExecution
import com.google.common.primitives.{Bytes, Ints, Longs}
import io.iohk.iodb.ByteArrayWrapper
import org.scalacheck.Gen
import bifrost.transaction.box.proposition.{MofNProposition, PublicKey25519Proposition}
import bifrost.transaction.proof.Signature25519
import io.circe.Json
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519

import scala.util.{Failure, Random, Success}

/**
  * Created by Matt Kindy on 6/7/2017.
  */
//noinspection ScalaStyle
class ProgramMethodExecutionValidationSpec extends ProgramSpec {

  val forgingSettings = new ForgingSettings {
    override def settingsJSON: Map[String, Json] = super.settingsFromFile("testSettings.json")
  }

  val pbr = ProgramBoxRegistryOld.readOrGenerate(forgingSettings)

  //noinspection ScalaStyle
  /*def arbitraryPartyProgramMethodExecutionGen(num: Int, numInProgram: Int): Gen[ProgramMethodExecution] = for {
    methodName <- Gen.oneOf(validProgramMethods)
    parameters <- jsonGen()
    timestamp <- positiveLongGen.map(_ / 3)
    data <- stringGen
  } yield {
    println(s">>>>> yield")

    val allKeyPairs = (0 until num + (3 - numInProgram)).map(_ => keyPairSetGen.sample.get.head)

    val roles = Random.shuffle(List(Role.Investor, Role.Producer, Role.Hub))
    val parties = (allKeyPairs zip (Stream continually roles).flatten).map(t => t._2 -> t._1)

    /* TODO: Don't know why this re-sampling is necessary here -- but should figure that out */
    var executionBuilderOpt = validExecutionBuilderGen().sample
    println(s">>>>> executionBuilderOpt: ${executionBuilderOpt.get.core.json}")
    while (executionBuilderOpt.isEmpty) executionBuilderOpt = validExecutionBuilderGen().sample
    val executionBuilder = executionBuilderOpt.get

    val leadParty = parties.head._2._2

    val stateBox: StateBox = StateBox(leadParty, 0L, Seq("a = 0"), true)
    val codeBox: CodeBox = CodeBox(leadParty, 1L, Seq("function add() { a = 2 + 2 }"))

    val stateBoxUUID: UUID = UUID.nameUUIDFromBytes(stateBox.id)

    val senders = parties.slice(3 - numInProgram, 3 - numInProgram + num)

    val proposition = MofNProposition(1, senders.map(_._2._2.pubKeyBytes).toSet)

    val executionBox: ExecutionBox = ExecutionBox(proposition, 2L, Seq(stateBoxUUID), Seq(codeBox.id))

    val feePreBoxes = senders.map(s => s._2._2 -> (0 until positiveTinyIntGen.sample.get).map { _ => preFeeBoxGen().sample.get }).toMap
    val feeBoxIdKeyPairs: Map[ByteArrayWrapper, PublicKey25519Proposition] = feePreBoxes.flatMap { case (prop, v) =>
      v.map {
        case (nonce, amount) => (ByteArrayWrapper(PublicKeyNoncedBox.idFromBox(prop, nonce)), prop)
      }
    }
    val fees = feePreBoxes.map { case (prop, preBoxes) =>
      prop -> (preBoxes.map(_._2).sum - Gen.choose(0L, Math.max(0, Math.min(Long.MaxValue, preBoxes.map(_._2).sum))).sample.get)
    }

    val hashNoNonces = FastCryptographicHash(
      executionBox.id ++
        methodName.getBytes ++
        parties.take(numInProgram).flatMap(_._2._2.pubKeyBytes) ++
        //parameters.noSpaces.getBytes ++
        (executionBox.id ++ feeBoxIdKeyPairs.flatMap(_._1.data)) ++
        Longs.toByteArray(timestamp) ++
        fees.flatMap { case (prop, amount) => prop.pubKeyBytes ++ Longs.toByteArray(amount) }
    )

    val messageToSign = Bytes.concat(FastCryptographicHash(executionBox.bytes ++ hashNoNonces), data.getBytes)
    val sigs: IndexedSeq[(PublicKey25519Proposition, Signature25519)] = allKeyPairs.take(numInProgram).map(t => t._2 -> PrivateKey25519Companion.sign(t._1, messageToSign))
    var extraSigs: IndexedSeq[(PublicKey25519Proposition, Signature25519)] = IndexedSeq()

    if(num - numInProgram > 0) {
      extraSigs = parties.slice(3, parties.length).map(t => t._2._2 -> PrivateKey25519Companion.sign(t._2._1, messageToSign))
    }

    ProgramMethodExecution(
      stateBox,
      codeBox,
      executionBox,
      methodName,
      parameters,
      parties.take(numInProgram).map(t => t._2._2 -> t._1).toMap,
      (sigs ++ extraSigs).toMap,
      feePreBoxes,
      fees,
      timestamp,
      data
    )
  }*/

  /*property("A block with valid CME will result in a correctly updated StateBox entry " +
    "and updated poly boxes in the LSMStore") {

    forAll(semanticallyValidProgramMethodExecutionGen) {
      cme: ProgramMethodExecution =>
        val block = Block(
          Array.fill(Block.SignatureLength)(-1: Byte),
          Instant.now.toEpochMilli,
          ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
          Signature25519(Array.fill(Block.SignatureLength)(0: Byte)),
          Seq(cme),
          10L,
          settings.version
        )

        val preExistingPolyBoxes: Set[BifrostBox] = cme
          .preFeeBoxes
          .flatMap {
            case (prop, preBoxes) => preBoxes.map(b => PolyBox(prop, b._1, b._2))
          }
          .toSet

        val box = cme
          .newBoxes
          .head
          .asInstanceOf[StateBox]

        val deductedFeeBoxes: Traversable[PolyBox] = cme
          .newBoxes
          .tail
          .map {
            case p: PolyBox => p
            case _ => throw new Exception("Was expecting PolyBoxes but found something else")
          }

        val boxBytes = StateBoxSerializer.toBytes(box)
        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingPolyBoxes + cme.executionBox + cme.stateBox + cme.codeBox,
          Instant.now.toEpochMilli)

        val preparedState = BifrostStateSpec
          .genesisState
          .applyChanges(necessaryBoxesSC, Ints.toByteArray(33), cme.executionBox)
          .get

        val newState = preparedState
          .applyChanges(preparedState.changes(block).get, Ints.toByteArray(34))
          .get

        require(newState.storage.get(ByteArrayWrapper(box.id)) match {
          case Some(wrapper) => wrapper.data sameElements boxBytes
          case None => false
        })

        cme.newBoxes.head shouldBe a[StateBox]

        Program
          .execute(cme.program, cme.methodName)(cme.parties.toIndexedSeq(0)._1)(cme.parameters.asObject.get)

        require(deductedFeeBoxes
          .forall(pb => newState.storage.get(ByteArrayWrapper(pb.id)) match {
            case Some(wrapper) => wrapper.data sameElements PolyBoxSerializer.toBytes(pb)
            case None => false
          }))

        /* Checks that the total sum of polys returned is total amount submitted minus total fees */
        deductedFeeBoxes.map(_.value).sum shouldEqual
          preExistingPolyBoxes.map { case pb: PolyBox => pb.value }.sum - cme.fee

        /* Checks that the amount returned in polys is equal to amount sent in less fees */
        cme
          .fees
          .foreach(p => deductedFeeBoxes
            .filter(_.proposition equals p._1)
            .map(_.value)
            .sum shouldEqual
            cme.preFeeBoxes(p._1).map(_._2).sum - cme.fees(p._1))

        /* Expect none of the prexisting boxes to still be around */
        preExistingPolyBoxes
          .foreach(pb => newState
            .storage
            .get(ByteArrayWrapper(pb.id)) shouldBe empty)

        BifrostStateSpec.genesisState = newState
          .rollbackTo(BifrostStateSpec.genesisBlockId)
          .get

    }
  }*/

  /*property("Attempting to validate a CME without valid signatures should error") {
    forAll(semanticallyValidProgramMethodExecutionGen) {
      programMethodExecution: ProgramMethodExecution =>

        val wrongSig: Array[Byte] = (programMethodExecution.signatures.head._2.bytes.head + 1).toByte +:
          programMethodExecution.signatures.head._2.bytes.tail

        val wrongSigs: Map[PublicKey25519Proposition, Signature25519] =
          programMethodExecution.signatures + (programMethodExecution.signatures.head._1 -> Signature25519(wrongSig))

        val invalidCME = programMethodExecution.copy(signatures = wrongSigs)

        val preExistingPolyBoxes: Set[BifrostBox] = programMethodExecution
          .preFeeBoxes
          .flatMap {
            case (prop, preBoxes) => preBoxes.map(b => PolyBox(prop, b._1, b._2))
          }
          .toSet

        val profileBoxes: Set[ProfileBox] = programMethodExecution
          .parties
          .map {
            case (p: PublicKey25519Proposition, r: Role.Role) =>
              ProfileBox(p, positiveLongGen.sample.get, r.toString, "role")
          }
          .toSet

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingPolyBoxes ++ profileBoxes + programMethodExecution.executionBox + programMethodExecution.stateBox + programMethodExecution.codeBox,
          Instant.now.toEpochMilli)

        val preparedState = BifrostStateSpec
          .genesisState
          .applyChanges(necessaryBoxesSC, Ints.toByteArray(35), programMethodExecution.executionBox)
          .get

        val newState = preparedState.validate(invalidCME)

        BifrostStateSpec.genesisState = preparedState
          .rollbackTo(BifrostStateSpec.genesisBlockId)
          .get

        newState shouldBe a[Failure[_]]
        newState.failed.get.getMessage shouldBe "Signature is invalid for ExecutionBox"
    }
  }*/

  //property("Attempting to validate a CME with a party that is not part of the program should error") {
    /*forAll(arbitraryPartyProgramMethodExecutionGen(num = 1, numInProgram = 0)) {
      cme: ProgramMethodExecution =>
        val roles = Random.shuffle(List(Role.Investor, Role.Producer, Role.Hub))
        val preExistingPolyBoxes: Set[BifrostBox] = cme.preFeeBoxes.flatMap { case (prop, preBoxes) => preBoxes.map(b => PolyBox(prop, b._1, b._2)) }.toSet
        val profileBoxes: Set[ProfileBox] = cme.parties.map {
          case (r: Role.Role, p: PublicKey25519Proposition) => ProfileBox(p, positiveLongGen.sample.get, r.toString, "role")
        }.toSet ++
          ((cme.signatures.keySet -- cme.parties.values.toSet) zip (Stream continually roles).flatten).map(t =>
            ProfileBox(t._1, positiveLongGen.sample.get, t._2.toString, "role")
          )

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingPolyBoxes + cme.programBox,
          Instant.now.toEpochMilli
        )

        val preparedState = BifrostStateSpec.genesisState.applyChanges(necessaryBoxesSC, Ints.toByteArray(1)).get
        val newState = preparedState.validate(cme)

        BifrostStateSpec.genesisState = preparedState.rollbackTo(BifrostStateSpec.genesisBlockId).get

        newState shouldBe a[Failure[_]]
        newState.failed.get.getMessage shouldBe s"Signature is invalid for programBox"
    } */
  //}

  //noinspection ScalaStyle
  //property("Attempting to validate a CME with too many signatures (versus parties) should error") {
    /*forAll(arbitraryPartyProgramMethodExecutionGen(num = Gen.choose(2, 10).sample.get, numInProgram = 1)) {
      cme: ProgramMethodExecution =>
        val roles = Random.shuffle(List(Role.Investor, Role.Producer, Role.Hub))
        val preExistingPolyBoxes: Set[BifrostBox] = cme.preFeeBoxes.flatMap { case (prop, preBoxes) => preBoxes.map(b => PolyBox(prop, b._1, b._2)) }.toSet
        val profileBoxes: Set[ProfileBox] = cme.parties.map {
          case (r: Role.Role, p: PublicKey25519Proposition) => ProfileBox(p, positiveLongGen.sample.get, r.toString, "role")
        }.toSet ++
          ((cme.signatures.keySet -- cme.parties.values.toSet) zip (Stream continually roles).flatten).map(t =>
            ProfileBox(t._1, positiveLongGen.sample.get, t._2.toString, "role")
          )

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingPolyBoxes ++ profileBoxes + cme.programBox,
          Instant.now.toEpochMilli
        )

        val preparedState = BifrostStateSpec.genesisState.applyChanges(necessaryBoxesSC, Ints.toByteArray(1)).get
        val newState = preparedState.validate(cme)

        BifrostStateSpec.genesisState = preparedState.rollbackTo(BifrostStateSpec.genesisBlockId).get

        newState shouldBe a[Failure[_]]

        val failMessage = newState.failed.get.getMessage

        /* This can have two outputs, since we can't know exactly how the extra signatures were 'role-d'. Either we get it right, and the signatures
           are not part of the program, or we get it wrong, and the roles are not valid */
        require(failMessage == "Signature is invalid for programBox" || failMessage == "Not all roles are valid for signers")
    }*/
  //}

  /*property("Attempting to validate a CME with a timestamp that is before the last block timestamp should error") {
    forAll(semanticallyValidProgramMethodExecutionGen) {
      programMethodExecution: ProgramMethodExecution =>

        val preExistingPolyBoxes: Set[BifrostBox] = programMethodExecution
          .preFeeBoxes
          .flatMap {
            case (prop, preBoxes) => preBoxes.map(b => PolyBox(prop, b._1, b._2))
          }
          .toSet

        val profileBoxes: Set[ProfileBox] = programMethodExecution
          .parties
          .map {
            case (p: PublicKey25519Proposition, r: Role.Role) =>
              ProfileBox(p, positiveLongGen.sample.get, r.toString, "role")
          }
          .toSet

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingPolyBoxes ++ profileBoxes + programMethodExecution.executionBox + programMethodExecution.stateBox + programMethodExecution.codeBox,
          programMethodExecution.timestamp +
            Gen.choose(1L, Long.MaxValue - programMethodExecution.timestamp - 1L).sample.get)

        val preparedState = BifrostStateSpec
          .genesisState
          .applyChanges(necessaryBoxesSC, Ints.toByteArray(36))
          .get

        val newState = preparedState.validate(programMethodExecution)

        BifrostStateSpec.genesisState = preparedState
          .rollbackTo(BifrostStateSpec.genesisBlockId)
          .get

        newState shouldBe a[Failure[_]]
        newState.failed.get.getMessage shouldBe "ProgramMethodExecution attempts to write into the past"
    }
  }*/

  /*property("Attempting to validate a CME for a program that doesn't exist should error") {
    forAll(semanticallyValidProgramMethodExecutionGen) {
      programMethodExecution: ProgramMethodExecution =>
        val block = Block(
          Array.fill(Block.SignatureLength)(-1: Byte),
          Instant.now.toEpochMilli,
          ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
          Signature25519(Array.fill(Block.SignatureLength)(0: Byte)),
          Seq(programMethodExecution),
          10L,
          settings.version
        )

        val preExistingPolyBoxes: Set[BifrostBox] = programMethodExecution
          .preFeeBoxes
          .flatMap {
            case (prop, preBoxes) => preBoxes.map(b => PolyBox(prop, b._1, b._2))
          }
          .toSet

        val box = programMethodExecution
          .newBoxes
          .head
          .asInstanceOf[StateBox]

        val deductedFeeBoxes: Traversable[PolyBox] = programMethodExecution
          .newBoxes
          .tail
          .map {
            case p: PolyBox => p
            case _ => throw new Exception("Was expecting PolyBoxes but found something else")
          }

        val boxBytes = StateBoxSerializer.toBytes(box)

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingPolyBoxes,
          Instant.now.toEpochMilli)

        val preparedState = BifrostStateSpec
          .genesisState
          .applyChanges(necessaryBoxesSC, Ints.toByteArray(37), programMethodExecution.executionBox)
          .get

        val newState = preparedState.validate(programMethodExecution)

        BifrostStateSpec.genesisState = preparedState
          .rollbackTo(BifrostStateSpec.genesisBlockId)
          .get

        newState shouldBe a[Failure[_]]
        newState.failed.get.getMessage shouldBe s"Program ${
          Base58.encode(programMethodExecution.executionBox
            .id)
        } does not exist"
    }
  }*/

  /*property("Attempting to validate a CME with a timestamp too far in the future should error") {
    forAll(semanticallyValidProgramMethodExecutionGen.suchThat(_.timestamp > Instant.now.toEpochMilli + 50L)) {
      programMethodExecution: ProgramMethodExecution =>
        val preExistingPolyBoxes: Set[BifrostBox] = programMethodExecution
          .preFeeBoxes
          .flatMap {
            case (prop, preBoxes) => preBoxes.map(b => PolyBox(prop, b._1, b._2))
          }.toSet

        val profileBoxes: Set[ProfileBox] = programMethodExecution
          .parties
          .map {
            case (p: PublicKey25519Proposition, r: Role.Role) =>
              ProfileBox(p, positiveLongGen.sample.get, r.toString, "role")
          }
          .toSet

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingPolyBoxes ++ profileBoxes + programMethodExecution.executionBox + programMethodExecution.stateBox + programMethodExecution.codeBox,
          Instant.now.toEpochMilli)

        val preparedState = BifrostStateSpec
          .genesisState
          .applyChanges(necessaryBoxesSC, Ints.toByteArray(38))
          .get

        val newState = preparedState.validate(programMethodExecution)

        BifrostStateSpec.genesisState = preparedState
          .rollbackTo(BifrostStateSpec.genesisBlockId)
          .get

        newState shouldBe a[Failure[_]]
        newState.failed.get.getMessage shouldBe "ProgramMethodExecution timestamp is too far into the future"
    }
  }*/

  /*property("Attempting to validate a CME with nonexistent fee boxes should error") {
    forAll(semanticallyValidProgramMethodExecutionGen) {
      programMethodExecution: ProgramMethodExecution =>
        val block = Block(
          Array.fill(Block.SignatureLength)(-1: Byte),
          Instant.now.toEpochMilli,
          ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
          Signature25519(Array.fill(Block.SignatureLength)(0: Byte)),
          Seq(programMethodExecution),
          10L,
          settings.version
        )

        val profileBoxes: Set[BifrostBox] = programMethodExecution
          .parties
          .map {
            case (p: PublicKey25519Proposition, r: Role.Role) =>
              ProfileBox(p, positiveLongGen.sample.get, r.toString, "role")
          }
          .toSet

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          profileBoxes + programMethodExecution.executionBox + programMethodExecution.stateBox + programMethodExecution.codeBox,
          Instant.now.toEpochMilli)

        val preparedState = BifrostStateSpec
          .genesisState
          .applyChanges(necessaryBoxesSC, Ints.toByteArray(39))
          .get

        val newState = preparedState.validate(programMethodExecution)

        BifrostStateSpec.genesisState = preparedState
          .rollbackTo(BifrostStateSpec.genesisBlockId)
          .get

        newState shouldBe a[Failure[_]]
        newState.failed.get.getMessage shouldBe "Insufficient balances provided for fees"
    }
  }*/
}
