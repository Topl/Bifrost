package co.topl.demo

import cats.Monad
import cats.effect._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.models._
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.{Length, Lengths, Sized}
import typings.forceGraph.mod.GraphData
import typings.forceGraph.{mod => fg}
import typings.std.HTMLElement
import typings.std.global.document

import scala.concurrent.duration._
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobalScope
import scala.util.Random

object ScalaJSDemo extends IOApp.Simple {
  import ScalaJSDemoData._
  type F[A] = IO[A]

  type FGGI = fg.ForceGraphGenericInstance[fg.ForceGraphGenericInstance[_]]

  def run: IO[Unit] =
    for {
      g <- Sync[F].delay {
        fg.default()
          .apply(document.body)
          .asInstanceOf[FGGI]
          .graphData(
            fg.GraphData(
              js.Array(),
              js.Array(fg.NodeObject().setId(genesisBlock.id._2.toBase58))
            )
          )
      }
      _ <- Monad[F].iterateForeverM[List[BlockHeaderV2], Unit](List(genesisBlock)) { bs =>
        val bs2 = Random.shuffle(bs.take(10))
        val parent = bs2.head
        val child = nextBlock(parent, 10)
        val delay = Random.nextInt(400).milli
        Async[F].delayBy(
          Sync[F]
            .delay {
              val d1 = g.graphData()
              val n1 = d1.nodes
              n1.push(fg.NodeObject().setId(child.id._2.toBase58))
              val l1 = d1.links
              l1.push(
                fg.LinkObject().setSource(child.id._2.toBase58).setTarget(parent.id._2.toBase58)
              )
              val d2 = GraphData(l1, n1)
              g.graphData(d2)
            }
            .as(child +: bs),
          delay
        )
      }
    } yield ()

  def nextBlock(parent: BlockHeaderV2, slotDiff: Long): BlockHeaderV2 =
    parent.copy(
      parentHeaderId = {
        val (p, d) = parent.id
        TypedBytes(p, d)
      },
      height = parent.height + 1,
      slot = parent.slot + slotDiff
    )
}

object ScalaJSDemoData {

  def zeroBytes[L <: Length](implicit l: L): Sized.Strict[Bytes, L] =
    Sized.strictUnsafe[Bytes, L](Bytes(Array.fill(l.value)(0: Byte)))

  val genesisBlock =
    BlockHeaderV2(
      parentHeaderId = TypedBytes(1: Byte, Bytes.fill(32)(-1: Byte)),
      parentSlot = -1L,
      txRoot = Sized.strictUnsafe(Bytes.fill(32)(0: Byte)),
      bloomFilter = Sized.strictUnsafe(Bytes.fill(256)(0: Byte)),
      timestamp = 0L,
      height = 0L,
      slot = 0L,
      eligibilityCertificate = EligibilityCertificate(
        Proofs.Knowledge.VrfEd25519(Sized.strictUnsafe(Bytes.fill(80)(0: Byte))),
        VerificationKeys.VrfEd25519(Sized.strictUnsafe(Bytes.fill(32)(0: Byte))),
        Sized.strictUnsafe(Bytes.fill(32)(0: Byte)),
        Sized.strictUnsafe(Bytes.fill(32)(0: Byte))
      ),
      operationalCertificate = OperationalCertificate(
        VerificationKeys.KesProduct(zeroBytes(Lengths.`32`), 0),
        Proofs.Knowledge.KesProduct(
          Proofs.Knowledge.KesSum(
            VerificationKeys.Ed25519(zeroBytes(Lengths.`32`)),
            Proofs.Knowledge.Ed25519(zeroBytes(Lengths.`64`)),
            Vector.empty
          ),
          Proofs.Knowledge.KesSum(
            VerificationKeys.Ed25519(zeroBytes(Lengths.`32`)),
            Proofs.Knowledge.Ed25519(zeroBytes(Lengths.`64`)),
            Vector.empty
          ),
          zeroBytes(Lengths.`32`)
        ),
        VerificationKeys.Ed25519(zeroBytes(Lengths.`32`)),
        Proofs.Knowledge.Ed25519(zeroBytes(Lengths.`64`))
      ),
      metadata = None,
      address = TaktikosAddress(
        zeroBytes(Lengths.`32`),
        VerificationKeys.Ed25519(zeroBytes(Lengths.`32`)),
        Proofs.Knowledge.Ed25519(zeroBytes(Lengths.`64`))
      )
    )

}

@js.native
@JSGlobalScope
object Globals extends js.Object {
  def ForceGraph(data: js.Object, otherAargs: js.Object = new js.Object): HTMLElement = js.native
}
