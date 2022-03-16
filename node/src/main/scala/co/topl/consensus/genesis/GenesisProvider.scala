package co.topl.consensus.genesis

import akka.actor.typed.ActorSystem
import cats.data.EitherT
import cats.implicits._
import co.topl.attestation.keyManagement.PrivateKeyCurve25519
import co.topl.attestation.{Address, EvidenceProducer, PublicKeyPropositionCurve25519, SignatureCurve25519}
import co.topl.codecs._
import co.topl.consensus.NxtConsensus
import co.topl.crypto.{PrivateKey, PublicKey}
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.box.{ArbitBox, Box, SimpleValue}
import co.topl.modifier.transaction.{ArbitTransfer, PolyTransfer}
import co.topl.settings.{AppSettings, GenesisStrategies}
import co.topl.utils.IdiomaticScalaTransition.implicits.toEitherOps
import co.topl.utils.Int128
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.StringDataTypes.{Base58Data, Latin1Data}
import io.circe.parser

import scala.collection.immutable.ListMap
import scala.concurrent.{ExecutionContext, Future}

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

  final case class GenerationDetails(
    addressesToPopulateGenesis: Set[Address],
    blockVersionByte:           Byte
  )

  def fetchGenesis(
    settings:             AppSettings
  )(optGenerationDetails: Option[GenerationDetails])(implicit
    system:               ActorSystem[_],
    ec:                   ExecutionContext,
    networkPrefix:        NetworkPrefix
  ): EitherT[Future, GenesisProvider.Failure, NxtConsensus.Genesis] =
    for {
      genesis <- EitherT.fromOption[Future](
        settings.forging.genesis.genesisStrategy.outerEnum match {
          case GenesisStrategies.Generated     => settings.forging.genesis.generated
          case GenesisStrategies.FromBlockJson => settings.forging.genesis.fromBlockJson
        },
        Failures.GenesisSettingsNotFound
      )
    } yield genesis

//  def fetchAndUpdateConsensusView(
//    settings:            AppSettings,
//    consensusInterface:  ConsensusInterface,
//    fetchStartupKeyView: () => Future[StartupKeyView]
//  )(implicit
//    system:        ActorSystem[_],
//    ec:            ExecutionContext,
//    networkPrefix: NetworkPrefix
//  ): EitherT[Future, Failure, NxtConsensus.Genesis] = for {
//    startupKeys <- EitherT.liftF[Future, Failure, StartupKeyView](fetchStartupKeyView())
//    generatedSettings <- EitherT
//      .fromOption[Future](settings.forging.genesis.map(_.generated), Failures.GenesisGeneratedSettingsNotFound)
//    jsonSettings <- EitherT
//      .fromOption[Future](settings.forging.genesis.map(_.fromBlockJson), Failures.GenesisBlockJsonSettingsNotFound)
//    genesis <- consensusInterface
//      .withView[Either[Failure, NxtConsensus.Genesis]] { view =>
//        settings.forging.genesis
//          .map(_.genesisStrategy)
//          .map {
//            case Generated =>
//              generatedGenesisProvider(startupKeys.addresses, view.protocolVersions.blockVersion(1L))
//                .get(generatedSettings)
//            case FromBlockJson => fromJsonGenesisProvider.get(jsonSettings)
//          }
//          .toRight(Failures.GenesisStrategyNotFound)
//      }
//      .leftMap(e => Failures.ConsensusInterfaceFailure(e.reason))
//      .subflatMap(identity)
//    stateUpdate = NxtConsensus.StateUpdate(
//      Some(genesis.state.totalStake),
//      Some(genesis.state.difficulty),
//      Some(genesis.state.inflation),
//      Some(genesis.state.height)
//    )
//    _ <- consensusInterface
//      .update(genesis.block.id, stateUpdate)
//      .leftMap(e => Failures.ConsensusInterfaceFailure(e.reason): Failure)
//  } yield genesis

  def construct(addresses:  Set[Address], balanceForEachParticipant: Long, initialDifficulty: Long, blockVersion: Byte)(
    implicit networkPrefix: NetworkPrefix
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

  def generatedGenesisProvider(addresses: Set[Address], blockVersion: Byte)(implicit
    networkPrefix:                        NetworkPrefix
  ): GenesisProvider[GenesisStrategies.GenerationSettings] = strategy =>
    construct(addresses, strategy.balanceForEachParticipant, strategy.initialDifficulty, blockVersion)

  def fromJsonGenesisProvider(implicit
    networkPrefix: NetworkPrefix
  ): GenesisProvider[GenesisStrategies.FromBlockJsonSettings] =
    strategy => {
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

  sealed abstract class Failure

  object Failures {
    case class ConsensusInterfaceFailure(reason: Throwable) extends Failure
    case object GenesisSettingsNotFound extends Failure
    case object GenesisGeneratedSettingsNotFound extends Failure
    case object GenesisBlockJsonSettingsNotFound extends Failure
    case object GenesisBlockJsonNotFound extends Failure
    case object GenesisStrategyNotFound extends Failure
  }
}
