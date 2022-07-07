package co.topl.credential.playground

import cats.data.Chain
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.credential.Credential
import co.topl.credential.implicits._
import co.topl.crypto.generation.KeyInitializer
import co.topl.crypto.generation.KeyInitializer.Instances.{curve25519Initializer, ed25519Initializer}
import co.topl.crypto.signing.{Curve25519, Ed25519, ExtendedEd25519}
import co.topl.models.ModelGenerators._
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Sized
import co.topl.typeclasses.implicits._

object CredentialTestJing extends App {
  type F[A] = cats.effect.IO[A]

  implicit val curve25519: Curve25519 = new Curve25519
  implicit val ed25519: Ed25519 = new Ed25519
  implicit val extendedEd25519: ExtendedEd25519 = new ExtendedEd25519

  implicit val networkPrefix: NetworkPrefix = NetworkPrefix(1: Byte)

  val stakingAddress: StakingAddress =
    StakingAddresses.Operator(ed25519.getVerificationKey(KeyInitializer[SecretKeys.Ed25519].random()))

  val offlineWalletSK =
    KeyInitializer[SecretKeys.Ed25519].random()

  def fullAddress(spendingAddress: SpendingAddress) = FullAddress(
    networkPrefix,
    spendingAddress,
    stakingAddress,
    ed25519.sign(offlineWalletSK, (spendingAddress, stakingAddress).signableBytes)
  )

  // Exercise: Construct complex propositions and attempt to prove them using Credentials
  val admin1SK: SecretKeys.Ed25519 = KeyInitializer[SecretKeys.Ed25519].random()
  val admin2SK: SecretKeys.Curve25519 = KeyInitializer[SecretKeys.Curve25519].random()
  val voter1SK: SecretKeys.Ed25519 = KeyInitializer[SecretKeys.Ed25519].random()
  val voter2SK: SecretKeys.Ed25519 = KeyInitializer[SecretKeys.Ed25519].random()
  val voter3SK: SecretKeys.Curve25519 = KeyInitializer[SecretKeys.Curve25519].random()

  val admin1Prop = admin1SK.vk.asProposition
  val admin2Prop = admin2SK.vk.asProposition
  val voter1Prop = voter1SK.vk.asProposition
  val voter2Prop = voter2SK.vk.asProposition
  val voter3Prop = voter3SK.vk.asProposition

  val heightProp = Propositions.Contextual.HeightLock(3)
  val adminsProp = admin1Prop.and(admin2Prop)
  val votersThresholdProp = List(voter1Prop, voter2Prop, voter3Prop, heightProp).threshold(2)
  val combinedProp = votersThresholdProp or adminsProp

  val recipientAddress: SpendingAddress = KeyInitializer[SecretKeys.Curve25519].random().vk.spendingAddress

  val unprovenTransaction: Transaction.Unproven =
    ModelGenerators.arbitraryUnprovenTransaction.arbitrary.first.copy(
      inputs = Chain(arbitraryTransactionUnprovenInput.arbitrary.first.copy(proposition = combinedProp)),
      outputs = Chain(
        Transaction.Output(fullAddress(recipientAddress), Box.Values.Poly(Sized.maxUnsafe(BigInt(5))), minting = false)
      )
    )

  val credential = (
    combinedProp,
    Iterable(
      Credential.Knowledge.Ed25519(voter1SK, unprovenTransaction),
      Credential.Knowledge.Ed25519(voter2SK, unprovenTransaction),
      Credential.Knowledge.Curve25519(voter3SK, unprovenTransaction),
      heightProp.toCredential,
      Credential.Knowledge.Ed25519(admin1SK, unprovenTransaction),
      Credential.Knowledge.Curve25519(admin2SK, unprovenTransaction)
    )
  ).toCredential

  val transactionAll = unprovenTransaction.prove(_ => credential.proof)
  println(s"all transaction: $transactionAll")

  val adminsCredential = Credential.Compositional.And(
    adminsProp,
    List(
      Credential.Knowledge.Ed25519(admin1SK, unprovenTransaction),
      Credential.Knowledge.Curve25519(admin2SK, unprovenTransaction)
    )
  )
  val adminsProof = adminsCredential.proof
//    println(adminsProof)

  val voter1and2ThresholdCredential =
    Credential.Compositional.Threshold(
      votersThresholdProp,
      List(
        Credential.Knowledge.Ed25519(voter1SK, unprovenTransaction),
        Credential.Knowledge.Ed25519(voter2SK, unprovenTransaction)
      )
    )
  val voter1and2ThresholdProof = voter1and2ThresholdCredential.proof
//    println(voter1and2ThresholdProof)

  val transactionAdmins = unprovenTransaction.prove(_ => adminsProof)
  println(s"admin transaction: $transactionAdmins")

  val transactionVoter1and2 = unprovenTransaction.prove(_ => voter1and2ThresholdProof)
  println(s"voter transaction: $transactionVoter1and2")

}
