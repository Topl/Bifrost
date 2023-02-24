package co.topl.blockchain

import cats.data.Chain
import co.topl.crypto.hash.Blake2b256
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility._
import co.topl.numerics.implicits._
import co.topl.typeclasses.implicits._

object PrivateTestnet {

  val TotalStake: Int128 = 10_000_000L

  /**
   * Constructs several Operator StakerInitializers.  A Staker is initialized using the concatenation of the timestamp (bytes)
   * with the staker's index (bytes).
   * @param timestamp The Big Bang Timestamp (used as the "seed" of the blockchain)
   * @param stakerCount the number of stakers to initialize.  0 is technically accepted and will return an empty list,
   *                    but the subsequent blockchain will be practically useless.  Without any initial operators,
   *                    the 2nd block of the chain can never be produced
   */
  def stakerInitializers(timestamp: Timestamp, stakerCount: Int): Chain[StakerInitializers.Operator] = {
    require(stakerCount >= 0)
    val blake2b256 = new Blake2b256()
    Chain
      .fromSeq(
        List.tabulate(stakerCount)(index =>
          // This staker's "seed" is concatenation of timestamp bytes + index bytes
          blake2b256.hash(Bytes.fromLong(timestamp) ++ Bytes.fromInt(index))
        )
      )
      .map(bytes => StakerInitializers.Operator(Sized.strictUnsafe(bytes), (9, 9)))
  }

  /**
   * Constructs a BigBang Config containing registrations of the given Stakers.  In addition, a single Poly box is
   * produced and is publicly spendable.
   */
  def config(timestamp: Timestamp, stakers: Chain[StakerInitializers.Operator])(implicit
    networkPrefix: NetworkPrefix
  ): BigBang.Config =
    BigBang.Config(
      timestamp,
      stakers
        .flatMap(_.bigBangOutputs(Sized.maxUnsafe(Ratio(TotalStake.data, stakers.length).round)))
        .append(
          Transaction.Output(
            FullAddress(
              networkPrefix,
              Propositions.Contextual.HeightLock(1).spendingAddress,
              StakingAddresses.NonStaking,
              Proofs.Knowledge.Ed25519(Sized.strictUnsafe(Bytes.fill(64)(0: Byte)))
            ),
            Box.Values.Poly(10_000_000),
            minting = true
          )
        )
    )
}
