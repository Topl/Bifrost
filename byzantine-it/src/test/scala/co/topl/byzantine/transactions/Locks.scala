package co.topl.byzantine.transactions

import co.topl.brambl.constants.NetworkConstants
import co.topl.brambl.models.Indices
import co.topl.brambl.models.box.{Challenge, Lock}
import co.topl.brambl.syntax.lockAsLockSyntaxOps
import co.topl.brambl.wallet.WalletApi
import co.topl.crypto.generation.Bip32Indexes
import co.topl.crypto.hash.Blake2b256
import co.topl.crypto.signing.ExtendedEd25519
import com.google.protobuf.ByteString
import quivr.models.{Digest, KeyPair, Preimage, Proposition}
import quivr.models.Proposition.{DigitalSignature, TickRange}

/**
 * Locks used on byzantine-it.TransactionTest
 */
object Locks {

  // HeightRange BEGIN
  private[byzantine] val HeightRangeProposition = Proposition(
    Proposition.Value.HeightRange(Proposition.HeightRange("header", 1, Long.MaxValue))
  )

  private[byzantine] val HeightRangeLock = Lock(
    Lock.Value.Predicate(Lock.Predicate(List(Challenge().withRevealed(HeightRangeProposition)), 1))
  )

  private[byzantine] val HeightRangeLockAddress =
    HeightRangeLock.lockAddress(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_LEDGER_ID)
  // HeightRange END

  // Digest BEGIN
  private[byzantine] val preimage = Preimage(ByteString.copyFrom("secret".getBytes), ByteString.copyFrom("salt".getBytes))

  private[byzantine] val digest = Digest(
    ByteString.copyFrom((new Blake2b256).hash(preimage.input.toByteArray ++ preimage.salt.toByteArray))
  )
  private[byzantine] val DigestProposition = Proposition(Proposition.Value.Digest(Proposition.Digest("Blake2b256", digest)))

  private[byzantine] val DigestLock = Lock(
    Lock.Value.Predicate(Lock.Predicate(List(Challenge().withRevealed(DigestProposition)), 1))
  )

  private[byzantine] val DigestLockAddress =
    DigestLock.lockAddress(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_LEDGER_ID)
  // Digest END

  // Digital Signature BEGIN:
  private val indices = Indices(0, 0, 0)

  private val keyPair: KeyPair =
    WalletApi.cryptoToPbKeyPair((new ExtendedEd25519).deriveKeyPairFromSeed(Array.fill(96)(0: Byte)))

  private[byzantine] val childKeyPair: KeyPair =
    WalletApi.cryptoToPbKeyPair(
      (new ExtendedEd25519).deriveKeyPairFromChildPath(
        WalletApi.pbKeyPairToCryotoKeyPair(keyPair).signingKey,
        List(
          Bip32Indexes.HardenedIndex(indices.x),
          Bip32Indexes.SoftIndex(indices.y),
          Bip32Indexes.SoftIndex(indices.z)
        )
      )
    )

  private val digitalSignature = DigitalSignature("ExtendedEd25519", childKeyPair.vk)
  private val DigitalSignatureProposition = Proposition(Proposition.Value.DigitalSignature(digitalSignature))

  private[byzantine] val DigitalSignatureLock = Lock(
    Lock.Value.Predicate(Lock.Predicate(List(Challenge().withRevealed(DigitalSignatureProposition)), 1))
  )

  private[byzantine] val DigitalSignatureLockAddress =
    DigitalSignatureLock.lockAddress(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_LEDGER_ID)
  // Digital Signature END

  // TickRange BEGIN
  private val tickRange = TickRange(min = 0L, max = 500L)
  private val badTickRange = TickRange(min = 0L, max = 1L)
  private val TickRangeProposition = Proposition(Proposition.Value.TickRange(tickRange))
  private val BadTickRangeProposition = Proposition(Proposition.Value.TickRange(badTickRange))

  private[byzantine] val GoodBadTickRangeAndProposition = Proposition(
    Proposition.Value.And(Proposition.And(TickRangeProposition, BadTickRangeProposition))
  )

  private[byzantine] val GoodTickRangeNotProposition = Proposition(
    Proposition.Value.Not(Proposition.Not(TickRangeProposition))
  )

  private[byzantine] val GoodBadTickRangeOrProposition = Proposition(
    Proposition.Value.Or(Proposition.Or(TickRangeProposition, BadTickRangeProposition))
  )

  private[byzantine] val TickRangeLock = Lock(
    Lock.Value.Predicate(Lock.Predicate(List(Challenge().withRevealed(TickRangeProposition)), 1))
  )

  private[byzantine] val BadTickRangeLock = Lock(
    Lock.Value.Predicate(Lock.Predicate(List(Challenge().withRevealed(BadTickRangeProposition)), 1))
  )

  private[byzantine] val GoodAndBadTickRangeLock = Lock(
    Lock.Value.Predicate(
      Lock.Predicate(
        List(
          Challenge().withRevealed(TickRangeProposition),
          Challenge().withRevealed(BadTickRangeProposition)
        ),
        2
      )
    )
  )

  private[byzantine] val GoodBadTickRangeAndLock = Lock(
    Lock.Value.Predicate(Lock.Predicate(List(Challenge().withRevealed(GoodBadTickRangeAndProposition)), 1))
  )

  private[byzantine] val GoodTickRangeNotLock = Lock(
    Lock.Value.Predicate(Lock.Predicate(List(Challenge().withRevealed(GoodTickRangeNotProposition)), 1))
  )

  private[byzantine] val GoodBadTickRangeOrLock = Lock(
    Lock.Value.Predicate(Lock.Predicate(List(Challenge().withRevealed(GoodBadTickRangeOrProposition)), 1))
  )

  private[byzantine] val TickRangeLockAddress =
    TickRangeLock.lockAddress(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_LEDGER_ID)

  private[byzantine] val BadTickRangeLockAddress =
    BadTickRangeLock.lockAddress(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_LEDGER_ID)

  private[byzantine] val GoodAndBadTickRangeLockAddress =
    GoodAndBadTickRangeLock.lockAddress(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_LEDGER_ID)

  private[byzantine] val GoodBadTickRangeAndLockAddress =
    GoodBadTickRangeAndLock.lockAddress(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_LEDGER_ID)

  private[byzantine] val GoodTickRangeNotLockAddress =
    GoodTickRangeNotLock.lockAddress(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_LEDGER_ID)

  private[byzantine] val GoodBadTickRangeOrLockAddress =
    GoodBadTickRangeOrLock.lockAddress(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_LEDGER_ID)

  // TickRange END

}
