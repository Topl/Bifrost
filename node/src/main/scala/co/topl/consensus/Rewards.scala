package co.topl.consensus

import cats.implicits._
import co.topl.attestation.{Address, PublicKeyPropositionCurve25519, SignatureCurve25519}
import co.topl.modifier.ModifierId
import co.topl.modifier.box.SimpleValue
import co.topl.modifier.transaction.{ArbitTransfer, PolyTransfer, Transaction}
import co.topl.utils.Int128
import co.topl.utils.StringDataTypes.Latin1Data
import co.topl.utils.TimeProvider.Time
import co.topl.utils.implicits._

import scala.collection.immutable.ListMap
import scala.util.Try

object ArbitReward {

  def apply(
    rewardAdr: Address,
    parentId:  ModifierId,
    forgeTime: Time,
    inflation: Int128,
    fee:       Int128 = 0
  ): ArbitTransfer[PublicKeyPropositionCurve25519] =
    ArbitTransfer(
      IndexedSeq(),
      IndexedSeq(
        (rewardAdr, SimpleValue(0)), // feeChangeOutput (Polys)
        (rewardAdr, SimpleValue(inflation)) // coinOutput (Arbits)
      ),
      ListMap[PublicKeyPropositionCurve25519, SignatureCurve25519](),
      fee,
      forgeTime,
      // the underscore is for letting miners add their own message in the future
      Some(Latin1Data.unsafe(parentId.show + "_")),
      minting = true
    )
}

object PolyReward {

  def apply(
    amount:    Int128,
    rewardAdr: Address,
    parentId:  ModifierId,
    forgeTime: Time,
    fee:       Int128 = 0
  ): PolyTransfer[PublicKeyPropositionCurve25519] =
    PolyTransfer(
      IndexedSeq(),
      IndexedSeq((rewardAdr, SimpleValue(amount))),
      ListMap[PublicKeyPropositionCurve25519, SignatureCurve25519](),
      fee,
      forgeTime,
      // the underscore is for letting miners add their own message in the future
      Some(Latin1Data.unsafe(parentId.show + "_")),
      minting = true
    )
}

object Rewards {
  type TX = Transaction.TX

  def apply(
    transactions: Seq[TX],
    rewardAddr:   Address,
    parentId:     ModifierId,
    forgeTime:    Time,
    inflation:    Int128,
    arbitFee:     Int128 = 0,
    polyFee:      Int128 = 0
  ): Try[Seq[TX]] =
    Try(
      Seq(
        ArbitReward(rewardAddr, parentId, forgeTime, inflation, arbitFee),
        PolyReward(transactions.map(_.fee).sum, rewardAddr, parentId, forgeTime, polyFee)
      )
    )
}
