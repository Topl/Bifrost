package bifrost.state

import java.time.Instant

import bifrost.blocks.BifrostBlock
import bifrost.contract.Contract
import bifrost.exceptions.JsonParsingException
import bifrost.transaction.box._
import bifrost.transaction.{ContractMethodExecution, Role}
import com.google.common.primitives.Ints
import io.iohk.iodb.ByteArrayWrapper
import org.scalacheck.Gen
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519

import scala.util.{Failure, Success}

/**
  * Created by Matt Kindy on 6/7/2017.
  */
//noinspection ScalaStyle
class BifrostStateContractMethodExecutionValidationSpec extends ContractSpec {

  //noinspection ScalaStyle
  /* TODO uncomment and fix tests with arbitraryPartyContractMethodExecutionGen
  def arbitraryPartyContractMethodExecutionGen(num: Int, numInContract: Int): Gen[ContractMethodExecution] = for {
    methodName <- Gen.oneOf(validContractMethods)
    parameters <- jsonGen()
    timestamp <- positiveLongGen.map(_ / 3)
    deliveredQuantity <- positiveLongGen
    effDelta <- positiveLongGen.map(_ / 3)
    expDelta <- positiveLongGen.map(_ / 3)
  } yield {
    val allKeyPairs = (0 until num + (3 - numInContract)).map(_ => keyPairSetGen.sample.get.head)

    val roles = Random.shuffle(List(Role.Investor, Role.Producer, Role.Hub))
    val parties = (allKeyPairs zip (Stream continually roles).flatten).map(t => t._2 -> t._1)

    /* TODO: Don't know why this re-sampling is necessary here -- but should figure that out */
    var agreementOpt = validAgreementGen().sample
    while (agreementOpt.isEmpty) agreementOpt = validAgreementGen().sample
    val agreement = agreementOpt.get

    val contractBox = createContractBox(
      agreement,
      parties.take(3).map(t => t._1 -> t._2._2).toMap
    )

    val senders = parties.slice(3 - numInContract, 3 - numInContract + num)

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
      contractBox.id ++
        methodName.getBytes ++
        parties.take(numInContract).flatMap(_._2._2.pubKeyBytes) ++
        parameters.noSpaces.getBytes ++
        (contractBox.id ++ feeBoxIdKeyPairs.flatMap(_._1.data)) ++
        Longs.toByteArray(timestamp) ++
        fees.flatMap { case (prop, amount) => prop.pubKeyBytes ++ Longs.toByteArray(amount) }
    )

    val messageToSign = FastCryptographicHash(contractBox.value.noSpaces.getBytes ++ hashNoNonces)
    val sigs: IndexedSeq[(PublicKey25519Proposition, Signature25519)] = allKeyPairs.take(numInContract).map(t => t._2 -> PrivateKey25519Companion.sign(t._1, messageToSign))
    var extraSigs: IndexedSeq[(PublicKey25519Proposition, Signature25519)] = IndexedSeq()

    if(num - numInContract > 0) {
      extraSigs = parties.slice(3, parties.length).map(t => t._2._2 -> PrivateKey25519Companion.sign(t._2._1, messageToSign))
    }

    ContractMethodExecution(
      contractBox,
      methodName,
      parameters,
      parties.take(numInContract).map(t => t._1 -> t._2._2).toMap,
      (sigs ++ extraSigs).toMap,
      feePreBoxes,
      fees,
      timestamp
    )
  } */

  property("A block with valid CME will result in a correct contract entry and updated poly boxes in the LSMStore") {
    forAll(semanticallyValidContractMethodExecutionGen) {
      cme: ContractMethodExecution =>
        val block = BifrostBlock(
          Array.fill(BifrostBlock.SignatureLength)(-1: Byte),
          Instant.now.toEpochMilli,
          ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
          Signature25519(Array.fill(BifrostBlock.SignatureLength)(0: Byte)),
          Seq(cme)
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
          .asInstanceOf[ContractBox]

        val deductedFeeBoxes: Traversable[PolyBox] = cme
          .newBoxes
          .tail
          .map {
            case p: PolyBox => p
            case _ => throw new Exception("Was expecting PolyBoxes but found something else")
          }

        val boxBytes = ContractBoxSerializer.toBytes(box)
        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingPolyBoxes + cme.contractBox,
          Instant.now.toEpochMilli)

        val preparedState = BifrostStateSpec
          .genesisState
          .applyChanges(necessaryBoxesSC, Ints.toByteArray(1))
          .get

        val newState = preparedState
          .applyChanges(preparedState.changes(block).get, Ints.toByteArray(2))
          .get

        require(newState.storage.get(ByteArrayWrapper(box.id)) match {
                  case Some(wrapper) => wrapper.data sameElements boxBytes
                  case None => false
                })
        
        cme.newBoxes.head shouldBe a[ContractBox]
        val contractJson = cme
          .newBoxes
          .head
          .asInstanceOf[ContractBox]
          .json

        val newContractTimestamp = contractJson
          .asObject
          .flatMap(_.apply("value"))
          .flatMap(_.asObject.get("lastUpdated"))
          .map(_.as[Long] match {
                 case Right(timestamp: Long) => timestamp
                 case Left(_) =>
                   throw new JsonParsingException("Was unable to convert lastUpdated to long in new contract")
               })
          .get

        val oldContractTimestamp = cme
          .contractBox
          .json
          .asObject
          .flatMap(_.apply("value"))
          .flatMap(_.asObject.get("lastUpdated"))
          .map(_.as[Long] match {
                 case Right(timestamp: Long) => timestamp
                 case Left(_) =>
                   throw new JsonParsingException("Was unable to convert lastUpdated to long in old contract")
               })
          .get

        Contract
          .execute(cme.contract, cme.methodName)(cme.parties.toIndexedSeq(0)._2)(cme.parameters.asObject.get) match {
          case Success(res) => res match {
            case Left(_) => newContractTimestamp shouldBe cme.timestamp
            case Right(_) => newContractTimestamp shouldBe oldContractTimestamp
          }
          case Failure(_) => newContractTimestamp shouldBe oldContractTimestamp
        }

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
  }

  property("Attempting to validate a CME without valid signatures should error") {
    forAll(semanticallyValidContractMethodExecutionGen) {
      contractMethodExecution: ContractMethodExecution =>

        val wrongSig: Array[Byte] = (contractMethodExecution.signatures.head._2.bytes.head + 1).toByte +:
          contractMethodExecution.signatures.head._2.bytes.tail

        val wrongSigs: Map[PublicKey25519Proposition, Signature25519] =
          contractMethodExecution.signatures + (contractMethodExecution.signatures.head._1 -> Signature25519(wrongSig))

        val invalidCME = contractMethodExecution.copy(signatures = wrongSigs)

        val preExistingPolyBoxes: Set[BifrostBox] = contractMethodExecution
          .preFeeBoxes
          .flatMap {
            case (prop, preBoxes) => preBoxes.map(b => PolyBox(prop, b._1, b._2))
          }
          .toSet

        val profileBoxes: Set[ProfileBox] = contractMethodExecution
          .parties
          .map {
            case (r: Role.Role, p: PublicKey25519Proposition) =>
              ProfileBox(p, positiveLongGen.sample.get, r.toString, "role")
          }
          .toSet

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingPolyBoxes ++ profileBoxes + contractMethodExecution.contractBox,
          Instant.now.toEpochMilli)

        val preparedState = BifrostStateSpec
          .genesisState
          .applyChanges(necessaryBoxesSC, Ints.toByteArray(1))
          .get

        val newState = preparedState.validate(invalidCME)

        BifrostStateSpec.genesisState = preparedState
          .rollbackTo(BifrostStateSpec.genesisBlockId)
          .get

        newState shouldBe a[Failure[_]]
        newState.failed.get.getMessage shouldBe "Signature is invalid for contractBox"
    }
  }

  property("Attempting to validate a CME with a party that is not part of the contract should error") {
    /*forAll(arbitraryPartyContractMethodExecutionGen(num = 1, numInContract = 0)) {
      cme: ContractMethodExecution =>
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
          preExistingPolyBoxes + cme.contractBox,
          Instant.now.toEpochMilli
        )

        val preparedState = BifrostStateSpec.genesisState.applyChanges(necessaryBoxesSC, Ints.toByteArray(1)).get
        val newState = preparedState.validate(cme)

        BifrostStateSpec.genesisState = preparedState.rollbackTo(BifrostStateSpec.genesisBlockId).get

        newState shouldBe a[Failure[_]]
        newState.failed.get.getMessage shouldBe s"Signature is invalid for contractBox"
    } */
  }

  //noinspection ScalaStyle
  property("Attempting to validate a CME with too many signatures (versus parties) should error") {
    /*forAll(arbitraryPartyContractMethodExecutionGen(num = Gen.choose(2, 10).sample.get, numInContract = 1)) {
      cme: ContractMethodExecution =>
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
          preExistingPolyBoxes ++ profileBoxes + cme.contractBox,
          Instant.now.toEpochMilli
        )

        val preparedState = BifrostStateSpec.genesisState.applyChanges(necessaryBoxesSC, Ints.toByteArray(1)).get
        val newState = preparedState.validate(cme)

        BifrostStateSpec.genesisState = preparedState.rollbackTo(BifrostStateSpec.genesisBlockId).get

        newState shouldBe a[Failure[_]]

        val failMessage = newState.failed.get.getMessage

        /* This can have two outputs, since we can't know exactly how the extra signatures were 'role-d'. Either we get it right, and the signatures
           are not part of the contract, or we get it wrong, and the roles are not valid */
        require(failMessage == "Signature is invalid for contractBox" || failMessage == "Not all roles are valid for signers")
    }*/
  }

  property("Attempting to validate a CME with a timestamp that is before the last block timestamp should error") {
    forAll(semanticallyValidContractMethodExecutionGen) {
      contractMethodExecution: ContractMethodExecution =>

        val preExistingPolyBoxes: Set[BifrostBox] = contractMethodExecution
          .preFeeBoxes
          .flatMap {
            case (prop, preBoxes) => preBoxes.map(b => PolyBox(prop, b._1, b._2))
          }
          .toSet

        val profileBoxes: Set[ProfileBox] = contractMethodExecution
          .parties
          .map {
            case (r: Role.Role, p: PublicKey25519Proposition) =>
              ProfileBox(p, positiveLongGen.sample.get, r.toString, "role")
          }
          .toSet

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingPolyBoxes ++ profileBoxes + contractMethodExecution.contractBox,
          contractMethodExecution.timestamp +
            Gen.choose(1L, Long.MaxValue - contractMethodExecution.timestamp - 1L).sample.get)

        val preparedState = BifrostStateSpec
          .genesisState
          .applyChanges(necessaryBoxesSC, Ints.toByteArray(1))
          .get

        val newState = preparedState.validate(contractMethodExecution)

        BifrostStateSpec.genesisState = preparedState
          .rollbackTo(BifrostStateSpec.genesisBlockId)
          .get

        newState shouldBe a[Failure[_]]
        newState.failed.get.getMessage shouldBe "ContractMethodExecution attempts to write into the past"
    }
  }

  property("Attempting to validate a CME for a contract that doesn't exist should error") {
    forAll(semanticallyValidContractMethodExecutionGen) {
      contractMethodExecution: ContractMethodExecution =>
        val block = BifrostBlock(
          Array.fill(BifrostBlock.SignatureLength)(-1: Byte),
          Instant.now.toEpochMilli,
          ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
          Signature25519(Array.fill(BifrostBlock.SignatureLength)(0: Byte)),
          Seq(contractMethodExecution))

        val preExistingPolyBoxes: Set[BifrostBox] = contractMethodExecution
          .preFeeBoxes
          .flatMap {
            case (prop, preBoxes) => preBoxes.map(b => PolyBox(prop, b._1, b._2))
          }
          .toSet

        val box = contractMethodExecution
          .newBoxes
          .head
          .asInstanceOf[ContractBox]

        val deductedFeeBoxes: Traversable[PolyBox] = contractMethodExecution
          .newBoxes
          .tail
          .map {
            case p: PolyBox => p
            case _ => throw new Exception("Was expecting PolyBoxes but found something else")
          }

        val boxBytes = ContractBoxSerializer.toBytes(box)

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingPolyBoxes,
          Instant.now.toEpochMilli)

        val preparedState = BifrostStateSpec
          .genesisState
          .applyChanges(necessaryBoxesSC, Ints.toByteArray(1))
          .get

        val newState = preparedState.validate(contractMethodExecution)

        BifrostStateSpec.genesisState = preparedState
          .rollbackTo(BifrostStateSpec.genesisBlockId)
          .get

        newState shouldBe a[Failure[_]]
        newState.failed.get.getMessage shouldBe s"Contract ${
          Base58.encode(contractMethodExecution.contractBox
                          .id)
        } does not exist"
    }
  }

  property("Attempting to validate a CME with a timestamp too far in the future should error") {
    forAll(semanticallyValidContractMethodExecutionGen.suchThat(_.timestamp > Instant.now.toEpochMilli + 50L)) {
      contractMethodExecution: ContractMethodExecution =>
        val preExistingPolyBoxes: Set[BifrostBox] = contractMethodExecution
          .preFeeBoxes
          .flatMap {
            case (prop, preBoxes) => preBoxes.map(b => PolyBox(prop, b._1, b._2))
          }.toSet

        val profileBoxes: Set[ProfileBox] = contractMethodExecution
          .parties
          .map {
            case (r: Role.Role, p: PublicKey25519Proposition) =>
              ProfileBox(p, positiveLongGen.sample.get, r.toString, "role")
          }
          .toSet

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingPolyBoxes ++ profileBoxes + contractMethodExecution.contractBox,
          Instant.now.toEpochMilli)

        val preparedState = BifrostStateSpec
          .genesisState
          .applyChanges(necessaryBoxesSC, Ints.toByteArray(1))
          .get

        val newState = preparedState.validate(contractMethodExecution)

        BifrostStateSpec.genesisState = preparedState
          .rollbackTo(BifrostStateSpec.genesisBlockId)
          .get

        newState shouldBe a[Failure[_]]
        newState.failed.get.getMessage shouldBe "ContractMethodExecution timestamp is too far into the future"
    }
  }

  property("Attempting to validate a CME with nonexistent fee boxes should error") {
    forAll(semanticallyValidContractMethodExecutionGen) {
      contractMethodExecution: ContractMethodExecution =>
        val block = BifrostBlock(
          Array.fill(BifrostBlock.SignatureLength)(-1: Byte),
          Instant.now.toEpochMilli,
          ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
          Signature25519(Array.fill(BifrostBlock.SignatureLength)(0: Byte)),
          Seq(contractMethodExecution))

        val profileBoxes: Set[BifrostBox] = contractMethodExecution
          .parties
          .map {
            case (r: Role.Role, p: PublicKey25519Proposition) =>
              ProfileBox(p, positiveLongGen.sample.get, r.toString, "role")
          }
          .toSet

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          profileBoxes + contractMethodExecution.contractBox,
          Instant.now.toEpochMilli)

        val preparedState = BifrostStateSpec
          .genesisState
          .applyChanges(necessaryBoxesSC, Ints.toByteArray(1))
          .get

        val newState = preparedState.validate(contractMethodExecution)

        BifrostStateSpec.genesisState = preparedState
          .rollbackTo(BifrostStateSpec.genesisBlockId)
          .get

        newState shouldBe a[Failure[_]]
        newState.failed.get.getMessage shouldBe "Insufficient balances provided for fees"
    }
  }

}
