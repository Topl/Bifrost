package co.topl.blockchain

import cats.data.Chain
import co.topl.crypto.hash.Blake2b256
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility._
import co.topl.typeclasses.implicits._

object PrivateTestnet {

  val TotalStake: Int128 = 10_000_000L

  def stakerInitializers(timestamp: Timestamp, stakerCount: Int): Chain[StakerInitializers.Operator] = {
    val blake2b256 = new Blake2b256()
    Chain
      .fromSeq(
        List.tabulate(stakerCount)(index =>
          // This staker's "seed" is concatenation of timestamp bytes + index bytes
          blake2b256.hash(Bytes.fromLong(timestamp) ++ Bytes.fromInt(index))
        )
      )
      .map(StakerInitializers.Operator(_, (9, 9)))
  }

  def config(timestamp: Timestamp, stakers: Chain[StakerInitializers.Operator])(implicit
    networkPrefix:      NetworkPrefix
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
            Box.Values.Poly(10_000),
            minting = true
          )
        )
    )
}
