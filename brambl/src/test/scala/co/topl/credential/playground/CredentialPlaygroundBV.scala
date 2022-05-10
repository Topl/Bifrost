package co.topl.credential.playground

import cats.data.NonEmptyChain
import co.topl.credential.Credential
import co.topl.crypto.signing.{Ed25519, ExtendedEd25519}
import co.topl.models.utility.Sized
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.scripting.GraalVMScripting
import co.topl.scripting.GraalVMScripting.GraalVMValuable
import co.topl.scripting.GraalVMScripting.instances._
import co.topl.typeclasses.implicits._
import co.topl.typeclasses.{KeyInitializer, VerificationContext}
import io.circe.Json
import org.graalvm.polyglot.Value
import ModelGenerators._
import scala.collection.immutable.ListMap

object CredentialPlaygroundBV extends App {
  type F[A] = cats.effect.IO[A]

  implicit val ed25519: Ed25519 = new Ed25519
  implicit val extendedEd25519: ExtendedEd25519 = ExtendedEd25519.precomputed()

  implicit val jsExecutor: Propositions.Script.JS.JSScript => F[(Json, Json) => F[Boolean]] =
    s =>
      GraalVMScripting
        .jsExecutor[F, Boolean](s.value)
        .map(f =>
          Function.untupled(
            f.compose[(Json, Json)] { t =>
              Seq(
                GraalVMValuable[Json].toGraalValue(t._1),
                GraalVMValuable[Json].toGraalValue(t._2),
                Value.asValue(new VerificationUtils)
              )
            }
          )
        )
  implicit val networkPrefix: NetworkPrefix = NetworkPrefix(1: Byte)

  // Exercise: Construct complex propositions and attempt to prove them using Credentials

  // tic-tac-toe with propositions

  sealed trait Player {
    def prop(x: Proposition, o: Proposition): Proposition
  }

  case object X extends Player {
    override def prop(x: Proposition, o: Proposition): Proposition = x
  }

  case object O extends Player {
    override def prop(x: Proposition, o: Proposition): Proposition = o
  }

  case class TicTacToeBoard(
    a: Player,
    b: Player,
    c: Player,
    d: Player,
    e: Player,
    f: Player,
    g: Player,
    h: Player,
    i: Player
  ) {

    def toProposition(x: Proposition, o: Proposition): Propositions.Compositional.Or =
      (a.prop(x, o) and b.prop(x, o) and c.prop(x, o)) or
      (d.prop(x, o) and e.prop(x, o) and f.prop(x, o)) or
      (g.prop(x, o) and h.prop(x, o) and i.prop(x, o)) or
      (a.prop(x, o) and d.prop(x, o) and g.prop(x, o)) or
      (b.prop(x, o) and e.prop(x, o) and h.prop(x, o)) or
      (c.prop(x, o) and f.prop(x, o) and i.prop(x, o)) or
      (a.prop(x, o) and e.prop(x, o) and i.prop(x, o)) or
      (c.prop(x, o) and e.prop(x, o) and g.prop(x, o))
  }

  object TicTacToeBoard {

    def apply(row1: List[Player], row2: List[Player], row3: List[Player]): TicTacToeBoard =
      TicTacToeBoard(row1.head, row1(1), row1(2), row2.head, row2(1), row2(2), row3.head, row3(1), row3(2))
  }

  val board =
    TicTacToeBoard(
      List(X, O, O),
      List(O, X, O),
      List(X, X, X)
    )

  val playerA = KeyInitializer[SecretKeys.Ed25519].random()
  val playerB = KeyInitializer[SecretKeys.Ed25519].random()

  val boardProp = board.toProposition(playerA.vk.asProposition, playerB.vk.asProposition)

  val randomAddr = KeyInitializer[SecretKeys.Ed25519].random().vk.dionAddress

  val randomTx: Transaction.Unproven =
    Transaction.Unproven(
      inputs = NonEmptyChain(ModelGenerators.arbitraryTransactionUnprovenInput.arbitrary.first),
      outputs = NonEmptyChain(
        Transaction.Output(boardProp.dionAddress, Box.Values.Poly(Sized.maxUnsafe(BigInt(10))), minting = false)
      ),
      timestamp = System.currentTimeMillis(),
      data = None
    )

  val boardCred = Credential.Compositional.Or(
    boardProp,
    List(Credential.Knowledge.Ed25519(playerA, randomTx))
  )

  implicit val context: VerificationContext[F] = new VerificationContext[F] {

    override val currentTransaction: Transaction =
      randomTx.prove(_ => boardCred.proof)

    override val currentHeight: Slot = 10
    def inputBoxes: List[Box] = List()
    def currentSlot: Slot = 1
  }

  val validProof = boardProp.isSatisfiedBy(boardCred.proof)

  println(validProof)

  // Example:
  //  val party1SK: SecretKeys.Ed25519 = KeyInitializer[SecretKeys.Ed25519].random()
  //  val party2SK: SecretKeys.Curve25519 = KeyInitializer[SecretKeys.Curve25519].random()
  //
  //  val party3Address: DionAddress = KeyInitializer[SecretKeys.Curve25519].random().vk.dionAddress
  //
  //  val proposition = party1SK.vk.asProposition.and(party2SK.vk.asProposition)
  //  println(proposition)
  //
  //  val unprovenTransaction: Transaction.Unproven = Transaction.Unproven(
  //    inputs = List((proposition.dionAddress, Random.nextLong())),
  //    feeOutput = None,
  //    coinOutputs = NonEmptyChain[CoinOutput](Transaction.PolyOutput(party3Address, Sized.maxUnsafe(BigInt(10)))),
  //    fee = Sized.maxUnsafe(BigInt(5)),
  //    timestamp = System.currentTimeMillis(),
  //    data = None,
  //    minting = false
  //  )
  //
  //  val credential = Credential.Compositional.And(
  //    proposition,
  //    List(
  //      Credential.Knowledge.Ed25519(party1SK, unprovenTransaction),
  //      Credential.Knowledge.Curve25519(party2SK, unprovenTransaction)
  //    )
  //  )
  //
  //  val proof = credential.proof
  //  println(proof)
  //
  //  val transaction = Transaction(
  //    inputs = ListMap.from(unprovenTransaction.inputs.map(_ -> (proposition, proof))),
  //    feeOutput = unprovenTransaction.feeOutput,
  //    coinOutputs = unprovenTransaction.coinOutputs,
  //    fee = unprovenTransaction.fee,
  //    timestamp = unprovenTransaction.timestamp,
  //    data = unprovenTransaction.data,
  //    minting = unprovenTransaction.minting
  //  )
  //  println(transaction)

}
