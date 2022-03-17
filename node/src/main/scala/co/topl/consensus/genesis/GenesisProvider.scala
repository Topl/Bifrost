package co.topl.consensus.genesis

import akka.actor.typed.ActorSystem
import cats.data.EitherT
import cats.implicits._
import co.topl.attestation.keyManagement.PrivateKeyCurve25519
import co.topl.attestation.{Address, EvidenceProducer, PublicKeyPropositionCurve25519, SignatureCurve25519}
import co.topl.codecs._
import co.topl.consensus.KeyManager.StartupKeyView
import co.topl.consensus.NxtConsensus
import co.topl.consensus.genesis.GenesisProvider.Failures
import co.topl.consensus.genesis.GenesisProvider.StrategySettings.FromBlockJsonSettings
import co.topl.crypto.{PrivateKey, PublicKey}
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.box.{ArbitBox, Box, SimpleValue}
import co.topl.modifier.transaction.{ArbitTransfer, PolyTransfer}
import co.topl.settings.{AppSettings, GenesisStrategies, Version}
import co.topl.utils.IdiomaticScalaTransition.implicits.toEitherOps
import co.topl.utils.Int128
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.StringDataTypes.{Base58Data, Latin1Data}
import io.circe.parser

import scala.collection.immutable.ListMap
import scala.concurrent.{ExecutionContext, Future}

class GenesisProvider(consensusView: NxtConsensus.View, startupKeyView: StartupKeyView) {

  def fetchGenesis(
    settings: AppSettings
  )(implicit
    system:        ActorSystem[_],
    ec:            ExecutionContext,
    networkPrefix: NetworkPrefix
  ): EitherT[Future, GenesisProvider.Failure, NxtConsensus.Genesis] =

    settings.application.genesis.genesisStrategy.outerEnum match {
      case GenesisStrategies.FromBlockJson =>
        EitherT.fromOption[Future](
          settings.application.genesis.fromBlockJson.map(GenesisProvider.fromJsonGenesisProvider),
          Failures.GenesisBlockJsonSettingsNotFound
        )

      case GenesisStrategies.Generated if startupKeyView.addresses.nonEmpty =>
        EitherT.fromOption[Future](
          settings.application.genesis.generated.map { sg =>
            GenesisProvider.construct(
              startupKeyView.addresses,
              sg.balanceForEachParticipant,
              sg.initialDifficulty,
              consensusView.protocolVersions.blockVersion(1)
            )
          },
          Failures.GenesisSettingsNotFound
        )

      case GenesisStrategies.Generated => EitherT.leftT(Failures.AttemptedGenerationButNoAddressAvailable)
    }
}

object GenesisProvider {

  final case class GenesisTransactionParams(
    from:       IndexedSeq[(Address, Box.Nonce)],
    to:         IndexedSeq[(Address, SimpleValue)],
    signatures: ListMap[PublicKeyPropositionCurve25519, SignatureCurve25519],
    fee:        Int128,
    timestamp:  Long,
    data:       Option[Latin1Data],
    minting:    Boolean
  )

  private[genesis] def fromJsonGenesisProvider(strategy: FromBlockJsonSettings)(implicit
    networkPrefix:                                       NetworkPrefix
  ): NxtConsensus.Genesis = {
    def readJson(filename: String)(implicit networkPrefix: NetworkPrefix): Block = {
      val src = scala.io.Source.fromFile(filename)

      // attempt to retrieve the required keyfile type from the data that was just read
      val block: Block = parser.parse(src.mkString) match {
        case Left(ex) => throw ex
        case Right(json) =>
          json.as[Block].getOrThrow(ex => new Exception(s"Could not parse blcok Json: $ex"))
      }

      // close the stream and return the keyfile
      src.close()
      block
    }

    // should checksum be a byte category?
    val checksum: ModifierId =
      Base58Data
        .unsafe(strategy.blockChecksum)
        .encodeAsBytes
        .decodeTransmitted[ModifierId]
        .getOrThrow()

    val block = readJson(strategy.providedJsonGenesisPath)

    require(
      block.id == checksum,
      s"${Console.RED}MALFORMED GENESIS BLOCK! The calculated genesis block " +
      s"with id ${block.id} does not match the required block for the chosen network mode.${Console.RESET}"
    )

    val totalStake = block.transactions
      .flatMap(_.newBoxes.map {
        case box: ArbitBox => box.value.quantity
        case _             => Int128(0)
      })
      .sum

    NxtConsensus.Genesis(block, NxtConsensus.State(totalStake, block.difficulty, 0L, 0L))
  }

  private[genesis] def construct(
    addresses:                 Set[Address],
    balanceForEachParticipant: Long,
    initialDifficulty:         Long,
    blockVersion:              Byte
  )(implicit
    networkPrefix: NetworkPrefix
  ): NxtConsensus.Genesis = {

    val genesisAcctCurve25519 =
      new PrivateKeyCurve25519(PrivateKey(Array.fill(32)(2: Byte)), PublicKey(Array.fill(32)(2: Byte)))

    val totalStake = addresses.size * balanceForEachParticipant

    val txInput: GenesisTransactionParams = GenesisTransactionParams(
      IndexedSeq(),
      (genesisAcctCurve25519.publicImage.address -> SimpleValue(0L)) +: addresses
        .map(_ -> SimpleValue(balanceForEachParticipant))
        .toIndexedSeq,
      ListMap(genesisAcctCurve25519.publicImage -> SignatureCurve25519.genesis),
      Int128(0),
      0L,
      None,
      minting = true
    )

    val block = Block(
      ModifierId.genesisParentId,
      0L,
      ArbitBox(
        EvidenceProducer[PublicKeyPropositionCurve25519].generateEvidence(genesisAcctCurve25519.publicImage),
        0,
        SimpleValue(totalStake)
      ),
      genesisAcctCurve25519.publicImage,
      SignatureCurve25519.genesis,
      1L,
      initialDifficulty,
      Seq(
        ArbitTransfer[PublicKeyPropositionCurve25519](
          txInput.from,
          (txInput.to.head._1, SimpleValue(0)) +: txInput.to, // first 'to'' is feeChangeOutput
          txInput.signatures,
          txInput.fee,
          txInput.timestamp,
          txInput.data,
          txInput.minting
        ),
        PolyTransfer[PublicKeyPropositionCurve25519](
          txInput.from,
          txInput.to,
          txInput.signatures,
          txInput.fee,
          txInput.timestamp,
          txInput.data,
          txInput.minting
        )
      ),
      blockVersion
    )

    val state = NxtConsensus.State(Int128(totalStake), initialDifficulty, 0L, 0L)

    NxtConsensus.Genesis(block, state)
  }

  sealed abstract class Failure

  object Failures {
    case class ConsensusInterfaceFailure(reason: Throwable) extends Failure
    case object GenesisSettingsNotFound extends Failure
    case object GenesisGeneratedSettingsNotFound extends Failure
    case object GenesisBlockJsonSettingsNotFound extends Failure
    case object GenesisBlockJsonNotFound extends Failure
    case object GenesisStrategyNotFound extends Failure
    case object AttemptedGenerationButNoAddressAvailable extends Failure
  }

  sealed abstract class StrategySetting

  object StrategySettings {

    case class GenerationSettings(
      genesisApplicationVersion: Version,
      balanceForEachParticipant: Long,
      initialDifficulty:         Long
    ) extends StrategySetting

    case class FromBlockJsonSettings(providedJsonGenesisPath: String, blockChecksum: String) extends StrategySetting
  }

}
