package co.topl.crypto.accumulators

import cats.implicits._
import co.topl.crypto.accumulators.merkle.MerkleTree.MerkleTreeResult
import co.topl.crypto.accumulators.merkle.{Leaf, MerkleTree}
import co.topl.crypto.hash.digest.{Digest, Digest32}
import co.topl.crypto.hash.implicits.{toHashResultOps, _}
import co.topl.crypto.hash.{Blake2b, Hash}
import co.topl.crypto.utils.Generators._
import co.topl.crypto.utils.randomBytes
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class MerkleTreeSpecification extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  type HashScheme = Blake2b
  type HashDigest = Digest32
  val hf = Hash[HashScheme, HashDigest]
  def bytesOf(h: HashDigest): Array[Byte] = Digest[HashDigest].bytes(h)

  private val leafSize = 32

  property("Proof generation by element") {
    forAll(smallInt) { N: Int =>
      whenever(N > 0) {
        val d = (0 until N).map(_ => LeafData(randomBytes(leafSize)))
        val leafs = d.map(data => Leaf[HashScheme, HashDigest](data))
        val tree = MerkleTree.construct[HashScheme, HashDigest](d).getOrThrow()
        val treeRootHash = tree.rootHash.getOrThrow()
        leafs.foreach { l =>
          val proof = tree.proofByElement(l).get
          proof.leafData.value.sameElements(l.data.value) shouldBe true
          proof.valid(treeRootHash) shouldBe true
        }
      }
    }
  }

  property("Proof generation by index") {
    forAll(smallInt) { N: Int =>
      whenever(N > 0) {
        val d = (0 until N).map(_ => LeafData(randomBytes(leafSize)))
        val tree = MerkleTree.construct[HashScheme, HashDigest](d).getOrThrow()

        (0 until N).foreach { i =>
          tree.proofByIndex(i).get.leafData.value shouldEqual d(i).value
          tree
            .proofByIndex(i)
            .get
            .valid(tree.rootHash.getOrThrow()) shouldBe true
        }
        (N until N + 100).foreach { i =>
          tree.proofByIndex(i).isEmpty shouldBe true
        }
        (-(N + 100) until 0).foreach { i =>
          tree.proofByIndex(i).isEmpty shouldBe true
        }
      }
    }
  }

  property("Tree creation from 0 elements") {
    val tree = MerkleTree.construct[HashScheme, HashDigest](Seq.empty).getOrThrow()
    tree.rootHash.getOrThrow().value shouldEqual Array.fill(Digest[HashDigest].size)(0: Byte)
  }

  property("Tree creation from 1 element") {
    forAll { d: Array[Byte] =>
      whenever(d.length > 0) {
        val tree = MerkleTree.construct[HashScheme, HashDigest](Seq(LeafData(d))).getOrThrow()

        bytesOf(tree.rootHash.getOrThrow()) shouldEqual
        bytesOf(
          hf.hash(
            MerkleTree.InternalNodePrefix,
            bytesOf(hf.hash(MerkleTree.LeafPrefix, d).getOrThrow())
          ).getOrThrow()
        )
      }
    }
  }

  property("Tree creation from 5 elements") {
    forAll { d: Array[Byte] =>
      whenever(d.length > 0) {
        val leafs: Seq[LeafData] = (0 until 5).map(_ => LeafData(d))
        val tree = MerkleTree.construct[HashScheme, HashDigest](leafs).getOrThrow()
        val h0x = hf.hash(MerkleTree.LeafPrefix, d).getOrThrow()
        val h10 = hf.hash(MerkleTree.InternalNodePrefix, bytesOf(h0x), bytesOf(h0x)).getOrThrow()
        val h11 = h10
        val h12 = hf.hash(MerkleTree.InternalNodePrefix, bytesOf(h0x)).getOrThrow()
        val h20 = hf.hash(MerkleTree.InternalNodePrefix, bytesOf(h10), bytesOf(h11)).getOrThrow()
        val h21 = hf.hash(MerkleTree.InternalNodePrefix, bytesOf(h12)).getOrThrow()
        val h30 = hf.hash(MerkleTree.InternalNodePrefix, bytesOf(h20), bytesOf(h21)).getOrThrow()
        bytesOf(h30) shouldEqual bytesOf(tree.rootHash.getOrThrow())
      }
    }
  }

  property("Tree creation from 2 element") {
    forAll { (d1: Array[Byte], d2: Array[Byte]) =>
      val tree = MerkleTree.construct[HashScheme, HashDigest](Seq(LeafData(d1), LeafData(d2))).getOrThrow()
      tree.rootHash.getOrThrow() shouldEqual hf
        .hash(
          MerkleTree.InternalNodePrefix,
          bytesOf(hf.hash(MerkleTree.LeafPrefix, d1).getOrThrow()),
          bytesOf(hf.hash(MerkleTree.LeafPrefix, d2).getOrThrow())
        )
        .getOrThrow()
    }
  }

  property("Tree creation from a lot of elements") {
    forAll { d: Seq[Array[Byte]] =>
      whenever(d.nonEmpty) {
        val tree = MerkleTree.construct[HashScheme, HashDigest](d.map(a => LeafData(a))).getOrThrow()
        tree.rootHash.getOrThrow()
      }
    }
  }

  implicit class MerkleTreeResultExtensions[H, D](m: MerkleTreeResult[H, D]) {

    def getOrThrow(): MerkleTree[H, D] =
      m.valueOr(err => throw new Exception(s"Merkle tree construction failure: ${err}"))
  }
}
