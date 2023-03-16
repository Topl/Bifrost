package co.topl.blockchain

import co.topl.brambl.common.ContainsEvidence
import co.topl.brambl.common.ContainsImmutable.instances.lockImmutable
import co.topl.brambl.models.Identifier
import co.topl.brambl.models.LockAddress
import co.topl.brambl.models.box.Challenge
import co.topl.brambl.models.box.Lock
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction.UnspentTransactionOutput
import co.topl.crypto.hash.Blake2b256
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility._
import co.topl.numerics.implicits._
import com.google.protobuf.ByteString
import quivr.models.Int128
import quivr.models.Proposition

object PrivateTestnet {

  val DefaultTotalStake: Int128 = 10_000_000L

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
      .map(bytes => StakerInitializers.Operator(Sized.strictUnsafe(bytes), (9, 9)))
  }

  /**
   * Constructs a BigBang Config containing registrations of the given Stakers.  In addition, a single Poly box is
   * produced and is publicly spendable.
   */
  def config(timestamp: Timestamp, stakers: List[StakerInitializers.Operator], stakes: Option[List[BigInt]])(implicit
    networkPrefix: NetworkPrefix
  ): BigBang.Config = {
    require(stakes.forall(_.length == stakers.length), "stakes must be the same length as stakers")
    BigBang.Config(
      timestamp,
      stakers
        .zip(
          stakes.getOrElse(
            List.fill(stakers.length)(
              Ratio(DefaultTotalStake, stakers.length).round
            )
          )
        )
        .flatMap { case (staker, stake) => staker.bigBangOutputs(stake) }
        .appended(
          UnspentTransactionOutput(
            HeightLockOneSpendingAddress,
            Value().withLvl(Value.LVL(10_000_000L))
          )
        )
    )
  }

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

  val HeightLockOneSpendingAddress: LockAddress =
    LockAddress(
      0,
      0,
      LockAddress.Id.Lock32(
        Identifier.Lock32(
          ContainsEvidence[Lock].sized32Evidence(HeightLockOneLock)
        )
      )
    )

}
