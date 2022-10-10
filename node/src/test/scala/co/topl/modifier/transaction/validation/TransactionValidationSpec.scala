package co.topl.modifier.transaction.validation

import cats.data.NonEmptyChain
import cats.scalatest.{ValidatedMatchers, ValidatedNecMatchers}
import co.topl.attestation.{Address, Proposition, PublicKeyPropositionCurve25519, PublicKeyPropositionEd25519}
import co.topl.codecs.json._
import co.topl.consensus.GenesisProvider
import co.topl.modifier.box._
import co.topl.modifier.transaction._
import co.topl.modifier.transaction.validation.implicits._
import co.topl.nodeView.state.BoxState
import co.topl.nodeView.{NodeViewTestHelpers, ValidTransactionGenerators}
import co.topl.utils.GeneratorOps.GeneratorOps
import co.topl.utils.NetworkType.PrivateTestnet
import co.topl.utils.StringDataTypes.Latin1Data
import co.topl.utils.{InMemoryKeyRingTestHelper, Int128, NetworkType}
import io.circe.{parser, ParsingFailure}
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.Random

class TransactionValidationSpec
    extends AnyPropSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with NodeViewTestHelpers
    with InMemoryKeyRingTestHelper
    with ValidTransactionGenerators
    with EitherValues
    with ValidatedMatchers
    with ValidatedNecMatchers {

  type TransferTx = TransferTransaction[TokenValueHolder, _ <: Proposition]

  property("Randomly generated AssetTransfer Tx should be valid") {
    withValidState { boxState =>
      forAll(
        validAssetTransferGen(
          keyRingCurve25519,
          keyRingEd25519,
          propsThresholdCurve25519,
          boxState,
          minting = true
        )
      ) { tx =>
        tx.syntacticValidation should beValid[TransferTx](tx)
      }
    }
  }

  property("AssetTransfer with minting = true should fail if no PolyBoxes are provided") {
    withValidState { boxState =>
      val genCurve25519 =
        validAssetTransferCurve25519Gen(keyRingCurve25519, boxState, fee = 0, minting = true).map { tx =>
          val fromWithoutPolys: IndexedSeq[(Address, Box.Nonce)] = tx.from
            .map { case (address, nonce) =>
              val boxId = BoxId.idFromEviNonce(address.evidence, nonce)
              val box = boxState.getBox(boxId).get
              (address, nonce, box)
            }
            .collect { case (address, nonce, _: AssetBox) =>
              (address, nonce)
            }

          val noPolyRawTx = tx.copy(from = fromWithoutPolys)
          val sender = keyRingCurve25519.addresses.head
          noPolyRawTx.copy(attestation =
            Transaction.updateAttestation(noPolyRawTx)(keyRingCurve25519.generateAttestation(sender))
          )
        }
      forAll(genCurve25519)(tx =>
        tx.semanticValidation(boxState) should haveInvalidC[SemanticValidationFailure](
          SyntacticSemanticValidationFailure(
            NonEmptyChain(NoInputBoxesSpecified)
          )
        )
      )

      val genEd25519 = validAssetTransferEd25519Gen(keyRingEd25519, boxState, fee = 0, minting = true).map { tx =>
        val fromWithoutPolys: IndexedSeq[(Address, Box.Nonce)] = tx.from
          .map { case (address, nonce) =>
            val boxId = BoxId.idFromEviNonce(address.evidence, nonce)
            val box = boxState.getBox(boxId).get
            (address, nonce, box)
          }
          .collect { case (address, nonce, _: AssetBox) =>
            (address, nonce)
          }

        val noPolyRawTx = tx.copy(from = fromWithoutPolys)
        val sender = keyRingEd25519.addresses.head
        noPolyRawTx.copy(attestation =
          Transaction.updateAttestation(noPolyRawTx)(keyRingEd25519.generateAttestation(sender))
        )
      }
      forAll(genEd25519)(tx =>
        tx.semanticValidation(boxState) should haveInvalidC[SemanticValidationFailure](
          SyntacticSemanticValidationFailure(
            NonEmptyChain(NoInputBoxesSpecified)
          )
        )
      )
    }
  }

  property("Attempting to validate a AssetTransfer without valid signature should error") {
    // Create invalid AssetTransfer
    // send tx to state
    forAll(assetTransferGen) { tx =>
      tx.syntacticValidation should haveInvalidC[SyntacticValidationFailure](UnsatisfiedProposition)
      tx.syntacticValidation should haveInvalidC[SyntacticValidationFailure](PropositionEvidenceMismatch)
      tx.syntacticValidation should haveInvalidC[SyntacticValidationFailure](MintingMissingIssuersSignature)
    }
  }

  property("Attempting to validate an AssetTransfer with data of invalid length should error") {
    forAll(stringGen) { data: String =>
      whenever(data.length >= 128) {
        val tx = assetTransferEd25519Gen.sampleFirst()
        val invalidDataTx = tx.copy(data = Some(Latin1Data.unsafe(data)))
        invalidDataTx.syntacticValidation should haveInvalidC[SyntacticValidationFailure](DataTooLong)
      }
    }
  }

  property("Attempting to validate an AssetTransfer with metadata of invalid length should error") {
    forAll(stringGen) { metadata: String =>
      whenever(metadata.length >= 128) {
        val tx = assetTransferEd25519Gen.sampleFirst()
        val assetValue = assetValueEd25519Gen.sampleFirst().copy(metadata = Some(Latin1Data.unsafe(metadata)))
        val invalidDataTx = tx.copy(to = IndexedSeq((assetValue.assetCode.issuer, assetValue)))

        invalidDataTx.syntacticValidation should haveInvalidC[SyntacticValidationFailure](MetadataTooLong)
      }
    }
  }

  property("Randomly generated ArbitTransfer Tx should be valid") {
    withValidState { boxState =>
      forAll(validArbitTransferGen(keyRingCurve25519, keyRingEd25519, propsThresholdCurve25519, boxState)) { tx =>
        tx.syntacticValidation should beValid[TransferTx](tx)
      }
    }
  }

  property("Attempting to validate a ArbitTransfer without valid signature should error") {
    // Create invalid PolyTransfer
    // send tx to state
    forAll(arbitTransferGen) { tx =>
      tx.syntacticValidation should haveInvalidC[SyntacticValidationFailure](UnsatisfiedProposition)
      tx.syntacticValidation should haveInvalidC[SyntacticValidationFailure](PropositionEvidenceMismatch)
    }
  }

  property("ArbitTransfer should fail if no PolyBoxes are provided") {
    withValidState { boxState =>
      val genCurve25519 =
        validArbitTransferCurve25519Gen(keyRingCurve25519, boxState, fee = 0)
          .map { tx =>
            val fromWithoutPolys: IndexedSeq[(Address, Box.Nonce)] = tx.from
              .map { case (address, nonce) =>
                val boxId = BoxId.idFromEviNonce(address.evidence, nonce)
                val box = boxState.getBox(boxId).get
                (address, nonce, box)
              }
              .collect { case (address, nonce, _: ArbitBox) =>
                (address, nonce)
              }

            signTx(tx.copy(from = fromWithoutPolys))

          }
      forAll(genCurve25519) { tx =>
        tx.semanticValidation(boxState) should haveInvalidC[SemanticValidationFailure](
          InputFeeChangeOutputUnequalNonMinting(
            Int128(Int.MaxValue),
            0,
            0
          )
        )
      }

      val genEd25519 =
        validArbitTransferEd25519Gen(keyRingEd25519, boxState, fee = 0)
          .map { tx =>
            val fromWithoutPolys: IndexedSeq[(Address, Box.Nonce)] = tx.from
              .map { case (address, nonce) =>
                val boxId = BoxId.idFromEviNonce(address.evidence, nonce)
                val box = boxState.getBox(boxId).get
                (address, nonce, box)
              }
              .collect { case (address, nonce, _: ArbitBox) =>
                (address, nonce)
              }

            signTx(tx.copy(from = fromWithoutPolys))

          }
      forAll(genEd25519) { tx =>
        tx.semanticValidation(boxState) should haveInvalidC[SemanticValidationFailure](
          InputFeeChangeOutputUnequalNonMinting(
            Int128(Int.MaxValue),
            0,
            0
          )
        )
      }
    }
  }

  property("Transactions created on a specific network should not be accepted on any other network") {
    withValidState { boxState =>
      val otherNetworks = NetworkType.all.filterNot(_ == PrivateTestnet)
      forAll(
        validAssetTransferGen(
          keyRingCurve25519,
          keyRingEd25519,
          propsThresholdCurve25519,
          boxState,
          minting = true
        )
      ) { tx =>
        otherNetworks.foreach { netType =>
          tx.syntacticValidation(netType.netPrefix) should haveInvalidC[SyntacticValidationFailure](
            MintingMissingIssuersSignature
          )
        }
        tx.syntacticValidation(PrivateTestnet.netPrefix) should beValid[TransferTx](tx)
      }
    }
  }

  property("Randomly generated PolyTransfer Tx should be valid") {
    withValidState { boxState =>
      forAll(validPolyTransferGen(keyRingCurve25519, keyRingEd25519, propsThresholdCurve25519, boxState)) { tx =>
        tx.syntacticValidation should beValid[TransferTx](tx)
      }
    }
  }

  property("Attempting to validate a PolyTransfer without valid signature should error") {
    // Create invalid PolyTransfer
    // send tx to state
    forAll(polyTransferCurve25519Gen) { tx =>
      tx.syntacticValidation should haveInvalidC[SyntacticValidationFailure](UnsatisfiedProposition)
    }
  }

  property("Transaction with negative fee should be invalid") {
    withValidState { boxState =>
      forAll(validPolyTransferCurve25519Gen(keyRingCurve25519, boxState).map(tx => signTx(tx.copy(fee = -1)))) { tx =>
        tx.syntacticValidation should haveInvalidC[SyntacticValidationFailure](NegativeFeeFailure)
      }

      forAll(validPolyTransferEd25519Gen(keyRingEd25519, boxState).map(tx => signTx(tx.copy(fee = -1)))) { tx =>
        tx.syntacticValidation should haveInvalidC[SyntacticValidationFailure](NegativeFeeFailure)
      }
    }
  }

  property("Transaction with negative timestamp should be invalid") {
    withValidState { boxState =>
      forAll(
        validPolyTransferCurve25519Gen(keyRingCurve25519, boxState).map(tx => signTx(tx.copy(timestamp = -1)))
      ) { tx =>
        tx.syntacticValidation should haveInvalidC[SyntacticValidationFailure](InvalidTimestamp)
      }

      forAll(validPolyTransferEd25519Gen(keyRingEd25519, boxState).map(tx => signTx(tx.copy(timestamp = -1)))) { tx =>
        tx.syntacticValidation should haveInvalidC[SyntacticValidationFailure](InvalidTimestamp)
      }
    }
  }

  property("Transaction with data length > 128 bytes should be invalid") {
    withValidState { boxState =>
      val data = Random.alphanumeric.take(512).mkString

      forAll(
        validPolyTransferCurve25519Gen(keyRingCurve25519, boxState).map(tx =>
          signTx(tx.copy(data = Some(Latin1Data.unsafe(data))))
        )
      ) { tx =>
        tx.syntacticValidation should haveInvalidC[SyntacticValidationFailure](DataTooLong)
      }

      forAll(
        validPolyTransferEd25519Gen(keyRingEd25519, boxState).map(tx =>
          signTx(tx.copy(data = Some(Latin1Data.unsafe(data))))
        )
      ) { tx =>
        tx.syntacticValidation should haveInvalidC[SyntacticValidationFailure](DataTooLong)
      }
    }
  }

  property("Improperly formatted Json transaction should fail validation") {
    val jsonTx =
      """
        |{
        |	"txType": "ArbitTransfer",
        |	"timestamp": 1618158572915,
        |	"signatures": {},
        |	"newBoxes": [
        |		{
        |			"nonce": "403810454164527817",
        |			"id": "H8prBRHWdpwRGsHa1h3RycBywr3h1tGw1rGh1LWM1kH7",
        |			"evidence": "QpFhbVa8ziDsDgEYN8tMHBici2sdQVeGRwuH2Uyqywgm",
        |			"type": "PolyBox",
        |			"value": {
        |				"type": "Simple",
        |				"quantity": "10000000"
        |			}
        |		}
        |	],
        |	"data": null,
        |	"from": [
        |		[
        |			"AU9dn9YhqL1YWxfemMfS97zjVXR6G9QX74XRq1jVLtP3snQtuuVk",
        |			"632651921866009156"
        |		],
        |		[
        |			"AU9Xs4B5HnsTiYGb7D71CCxg5mYhaQv1WH3ptfiGbV4LUGb87W54",
        |			"-1022347197045362381"
        |		],
        |		[
        |			"AUA3RmKwr39nVQFFTV1BQFELbFhJQVWfFDdS5YDx7r1om5UCbqef",
        |			"-3442597734742762895"
        |		],
        |		[
        |			"AU9avKWiVVPKyU9LoMqDpduS4knoLDMdPEK54qKDNBpdnAMwQZcS",
        |			"-9159076465667901239"
        |		],
        |		[
        |			"AUAvJqLKc8Un3C6bC4aj8WgHZo74vamvX8Kdm6MhtdXgw51cGfix",
        |			"-2738345987285926199"
        |		],
        |		[
        |			"AU9sKKy7MN7U9G6QeasZUMTirD6SeGQx8Sngmb9jmDgNB2EzA3rq",
        |			"6192622283292330446"
        |		],
        |		[
        |			"AU9upSwu8MtmQz6EBMuv34bJ4G8i6Aw64xxRShJ3kpZRec5Ucp9Q",
        |			"5211780637118315406"
        |		],
        |		[
        |			"AUANVY6RqbJtTnQS1AFTQBjXMFYDknhV8NEixHFLmeZynMxVbp64",
        |			"-7028078984054333839"
        |		],
        |		[
        |			"AU9NkZmX5Pch2kUA28GUtv9m4bNaLNtKLoFXphcAAc9PUQXinXRm",
        |			"-2082354150813152515"
        |		],
        |		[
        |			"AUAbSWQxzfoCN4FizrKKf6E1qCSRffHhjrvo2v7L6q8xFZ7pxKqh",
        |			"-1063807144321497201"
        |		],
        |		[
        |			"AU9dn9YhqL1YWxfemMfS97zjVXR6G9QX74XRq1jVLtP3snQtuuVk",
        |			"-276484983974921900"
        |		],
        |		[
        |			"AU9Xs4B5HnsTiYGb7D71CCxg5mYhaQv1WH3ptfiGbV4LUGb87W54",
        |			"7750101586306631723"
        |		],
        |		[
        |			"AUA3RmKwr39nVQFFTV1BQFELbFhJQVWfFDdS5YDx7r1om5UCbqef",
        |			"2630159458189048846"
        |		],
        |		[
        |			"AU9avKWiVVPKyU9LoMqDpduS4knoLDMdPEK54qKDNBpdnAMwQZcS",
        |			"4767595726154610060"
        |		],
        |		[
        |			"AUAvJqLKc8Un3C6bC4aj8WgHZo74vamvX8Kdm6MhtdXgw51cGfix",
        |			"640575933084298873"
        |		],
        |		[
        |			"AU9sKKy7MN7U9G6QeasZUMTirD6SeGQx8Sngmb9jmDgNB2EzA3rq",
        |			"-7183361756457193850"
        |		],
        |		[
        |			"AU9upSwu8MtmQz6EBMuv34bJ4G8i6Aw64xxRShJ3kpZRec5Ucp9Q",
        |			"2554848147682096396"
        |		],
        |		[
        |			"AUANVY6RqbJtTnQS1AFTQBjXMFYDknhV8NEixHFLmeZynMxVbp64",
        |			"4537410223110818381"
        |		],
        |		[
        |			"AU9NkZmX5Pch2kUA28GUtv9m4bNaLNtKLoFXphcAAc9PUQXinXRm",
        |			"7969292424973020400"
        |		],
        |		[
        |			"AUAbSWQxzfoCN4FizrKKf6E1qCSRffHhjrvo2v7L6q8xFZ7pxKqh",
        |			"6247990739483366931"
        |		]
        |	],
        |	"minting": false,
        |	"txId": "riWxuUYrewehokDP2cpdYpEVkLGUeH4NAFq4XTKcJeS9",
        |	"boxesToRemove": [
        |		"HAkHigK5gLyf7h6sXrr6TXeN2e2kGuWn5eLe53muGBrD",
        |		"FsmB4iF7br4QGt36pt5FGgZimh1dmBATu1q5ZMfDsRkW",
        |		"pr64bcWrWDJyjBCJ2JCDj6LzcK3sgmU82UdfNvSQXrE",
        |		"CtLGQcnpqVdoirGQNt6EJhQYPohaeNsc7ZmDcPVP4seV",
        |		"EkQrVxwgAmHJrzgY5kJyAvqH36DjkBFa4cPGGE31tY6y",
        |		"5Wq9cayFkbA2WzMVugWtm4kpLoYn5QTMkvpyNh9jCDBY",
        |		"5AGsGd6Bo4oqpbpmiV7yC4m4Xr4137e9qsMaQwtTcA6f",
        |		"JHtMQxsuNMG7GKjMMeBmthN2KJGMjNDwMFJKWEjY96K",
        |		"BoHnq1ty92dzDzR4RA9jBanYsHDZe1dhgvGCq3Fp7mmA",
        |		"6WNzwZvp9KqwtJxYZyPkcTM9z8d1oBuNUWh1zwwcWm3v",
        |		"FMmpdsgGqUFcj4vzjcKbr3xDbA7xvrWo7zFoFcjPmiza",
        |		"2wpjkgSnzApcVFFcTRhTM36yWNPuf8GzvD7CbBmzycNX",
        |		"5yPYYxjGJHUABatCShQ8F5sZ63J8eJ5KJxEcbV1b6edv",
        |		"DYW6Hr7NmHTWGFZFQkBEBNEmh4g3BkRCp6a7RfnrxfSv",
        |		"3vp93QXc2rpBRUKWVNYKo8DEMSqcCv4b26aXTbHZr18z",
        |		"82BfegxGxMwgggnUuSUCUvRCw5TdejtE2Huo56pd8y7H",
        |		"Buuk5MAvQSt4VVh6jmFPu9dYCiogL42cr5viVm71Zzaf",
        |		"6icLMsiptwyFaZ1x8DgEH7RQR1Z85MLRwqWbA5EQpvhP",
        |		"CmL5HPFXfhz5ZNbdEaYbpPFuckDoadXa1tato6UKbcrA",
        |		"3XETRxrJ1nD5njiFYu8XUVKGaEkdhPdaw4H9RSEYQGJ8"
        |	],
        |	"fee": "10000000",
        |	"to": [
        |		[
        |			"AUA3RmKwr39nVQFFTV1BQFELbFhJQVWfFDdS5YDx7r1om5UCbqef",
        |			{
        |				"type": "Simple",
        |				"quantity": "10000000"
        |			}
        |		]
        |	],
        |	"propositionType": "PublicKeyCurve25519"
        |}
        |""".stripMargin

    val txResult = for {
      tx      <- parser.parse(jsonTx)
      arbitTx <- tx.as[Transaction.TX]
      result <- arbitTx.syntacticValidation
        .leftMap(e => ParsingFailure(s"Parsing failed due to syntactic error=${e.toString}", new Exception(e.toString)))
        .toEither
    } yield result

    // not ideal but I am unsure how best to wrap the syntactic validation into a parsing failure so
    // that the specific errors could be checked for here.
    txResult.isLeft shouldBe true

  }

  private def signTx(
    tx: PolyTransfer[PublicKeyPropositionCurve25519]
  ): PolyTransfer[PublicKeyPropositionCurve25519] =
    tx.copy(attestation =
      Transaction.updateAttestation(tx)(keyRingCurve25519.generateAttestation(tx.from.map(_._1).toSet))
    )

  private def signTx(tx: => PolyTransfer[PublicKeyPropositionEd25519]): PolyTransfer[PublicKeyPropositionEd25519] =
    tx.copy(attestation =
      Transaction.updateAttestation(tx)(keyRingEd25519.generateAttestation(tx.from.map(_._1).toSet))
    )

  private def signTx(
    tx: ArbitTransfer[PublicKeyPropositionCurve25519]
  ): ArbitTransfer[PublicKeyPropositionCurve25519] =
    tx.copy(attestation =
      Transaction.updateAttestation(tx)(keyRingCurve25519.generateAttestation(tx.from.map(_._1).toSet))
    )

  private def signTx(
    tx: => ArbitTransfer[PublicKeyPropositionEd25519]
  ): ArbitTransfer[PublicKeyPropositionEd25519] =
    tx.copy(attestation =
      Transaction.updateAttestation(tx)(keyRingEd25519.generateAttestation(tx.from.map(_._1).toSet))
    )

  private def signTx(
    tx: AssetTransfer[PublicKeyPropositionCurve25519]
  ): AssetTransfer[PublicKeyPropositionCurve25519] =
    tx.copy(attestation =
      Transaction.updateAttestation(tx)(keyRingCurve25519.generateAttestation(tx.from.map(_._1).toSet))
    )

  private def signTx(
    tx: => AssetTransfer[PublicKeyPropositionEd25519]
  ): AssetTransfer[PublicKeyPropositionEd25519] =
    tx.copy(attestation =
      Transaction.updateAttestation(tx)(keyRingEd25519.generateAttestation(tx.from.map(_._1).toSet))
    )

  private def withValidState(test: BoxState => Unit): Unit =
    test(
      generateState(
        GenesisProvider
          .construct(
            keyRingCurve25519.addresses ++ keyRingEd25519.addresses ++ propsThresholdCurve25519.map(_.address),
            Int.MaxValue,
            Long.MaxValue,
            0
          )
          .block
      ).state
    )
}
