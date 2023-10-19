package co.topl.blockchain

import cats.effect.Async
import cats.implicits._
import co.topl.blockchain.BigBang.Config
import co.topl.brambl.constants.NetworkConstants
import co.topl.brambl.models.box.{Challenge, Lock, Value}
import co.topl.brambl.models.transaction.{IoTransaction, UnspentTransactionOutput}
import co.topl.brambl.models.{Datum, LockAddress}
import co.topl.brambl.syntax._
import co.topl.codecs.bytes.tetra.instances.persistableKesProductSecretKey
import co.topl.codecs.bytes.typeclasses.Persistable
import co.topl.config.ApplicationConfig
import co.topl.consensus.models.ProtocolVersion
import co.topl.crypto.hash.Blake2b256
import co.topl.crypto.models.SecretKeyKesProduct
import co.topl.models._
import co.topl.models.utility._
import co.topl.numerics.implicits._
import com.google.protobuf.ByteString
import fs2.Chunk
import fs2.io.file.{Files, Path}
import org.typelevel.log4cats.Logger
import quivr.models.{Int128, Proposition}

import scala.concurrent.duration._

object PrivateTestnet {

  val DefaultTotalStake: Int128 = 10_000_000L
  val DefaultTotalLvls: Int128 = 10_000_000L

  /**
   * Constructs several Operator StakerInitializers.  A Staker is initialized using the concatenation of the timestamp (bytes)
   * with the staker's index (bytes).
   * @param timestamp The Big Bang Timestamp (used as the "seed" of the blockchain)
   * @param stakerCount the number of stakers to initialize.  0 is technically accepted and will return an empty list,
   *                    but the subsequent blockchain will be practically useless.  Without any initial operators,
   *                    the 2nd block of the chain can never be produced
   */
  def stakerInitializers(timestamp: Timestamp, stakerCount: Int): List[StakerInitializers.Operator] = {
    require(stakerCount >= 0)
    val blake2b256 = new Blake2b256()
    List
      .tabulate(stakerCount)(index =>
        // This staker's "seed" is concatenation of timestamp bytes + index bytes
        blake2b256.hash(
          ByteString.copyFrom(BigInt(timestamp).toByteArray).concat(ByteString.copyFrom(BigInt(index).toByteArray))
        )
      )
      .map(bytes => StakerInitializers.Operator(bytes, (9, 9), HeightLockOneSpendingAddress))
  }

  def defaultStake(stakerCount: Int): BigInt = Ratio(DefaultTotalStake, stakerCount).round

  /**
   * Constructs a BigBang Config containing registrations of the given Stakers.  In addition, a single Poly box is
   * produced and is publicly spendable.
   */
  def config(
    timestamp:       Timestamp,
    stakers:         List[StakerInitializers.Operator],
    stakes:          Option[List[BigInt]],
    protocolVersion: ProtocolVersion,
    protocol:        ApplicationConfig.Bifrost.Protocol
  ): BigBang.Config = {
    require(stakes.forall(_.length == stakers.length), "stakes must be the same length as stakers")
    val transactions =
      stakers
        .zip(stakes.getOrElse(List.fill(stakers.length)(defaultStake(stakers.length))))
        .map { case (staker, stake) => staker.registrationTransaction(stake) }
        .appended(
          IoTransaction(
            inputs = Nil,
            outputs = List(
              UnspentTransactionOutput(
                HeightLockOneSpendingAddress,
                Value.defaultInstance.withLvl(Value.LVL(DefaultTotalLvls))
              ),
              UnspentTransactionOutput(
                HeightLockOneSpendingAddress,
                Value.defaultInstance.withUpdateProposal(BigBang.protocolToUpdateProposal(protocol))
              )
            ),
            datum = Datum.IoTransaction.defaultInstance
          )
        )
        .map(_.embedId)
    BigBang.Config(
      timestamp,
      transactions,
      Config.DefaultEtaPrefix,
      protocolVersion
    )
  }

  def writeStaker[F[_]: Async: Logger](
    stakingDir:  Path,
    initializer: StakerInitializers.Operator,
    stake:       Int128
  ): F[Unit] =
    Files.forAsync[F].createDirectories(stakingDir) >>
    Files
      .forAsync[F]
      .list(stakingDir)
      .compile
      .count
      .flatMap(c =>
        if (c > 0) Logger[F].info(show"Staker already initialized at $stakingDir")
        else
          for {
            _ <- Logger[F].info(show"Generating a private testnet genesis staker into $stakingDir")
            registrationTx = initializer.registrationTransaction(stake)
            writeFile = (name: String, data: Array[Byte]) =>
              fs2.Stream
                .chunk(Chunk.array(data))
                .through(Files.forAsync[F].writeAll(stakingDir / name))
                .compile
                .drain
            _ <- Files.forAsync[F].createDirectories(stakingDir / StakingInit.KesDirectoryName)
            _ <- writeFile(
              s"${StakingInit.KesDirectoryName}/0",
              Persistable[SecretKeyKesProduct].persistedBytes(initializer.kesSK).toByteArray
            )
            _ <- writeFile(StakingInit.VrfKeyName, initializer.vrfSK.toByteArray)
            _ <- writeFile(StakingInit.RegistrationTxName, registrationTx.toByteArray)
          } yield ()
      )

  val HeightLockOneProposition: Proposition =
    Proposition(
      Proposition.Value.HeightRange(
        Proposition.HeightRange("header", 1, Long.MaxValue)
      )
    )

  val HeightLockOneChallenge: Challenge =
    Challenge().withRevealed(HeightLockOneProposition)

  val HeightLockOneLock: Lock =
    Lock(
      Lock.Value.Predicate(
        Lock.Predicate(
          List(HeightLockOneChallenge),
          1
        )
      )
    )

  val HeightLockOneSpendingAddress: LockAddress = HeightLockOneLock.lockAddress(
    NetworkConstants.PRIVATE_NETWORK_ID,
    NetworkConstants.MAIN_LEDGER_ID
  )

  val DefaultProtocolVersion: ProtocolVersion = ProtocolVersion(2, 0, 0)

  val DefaultProtocol: ApplicationConfig.Bifrost.Protocol =
    ApplicationConfig.Bifrost.Protocol(
      minAppVersion = "2.0.0",
      fEffective = Ratio(15, 100),
      vrfLddCutoff = 50,
      vrfPrecision = 40,
      vrfBaselineDifficulty = Ratio(1, 20),
      vrfAmplitude = Ratio(1, 2),
      chainSelectionKLookback = 50,
      slotDuration = 1.seconds,
      forwardBiasedSlotWindow = 50,
      operationalPeriodsPerEpoch = 2,
      kesKeyHours = 9,
      kesKeyMinutes = 9
    )

}
