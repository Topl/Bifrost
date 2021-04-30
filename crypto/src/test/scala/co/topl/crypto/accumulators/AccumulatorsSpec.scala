package co.topl.crypto.accumulators

import org.scalatest.propspec.AnyPropSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalacheck.Gen
import scala.util.Random
import co.topl.crypto.accumulators.merkle.Leaf
import co.topl.crypto.hash.Blake2b
import co.topl.crypto.hash.Digest32
import co.topl.crypto.accumulators.merkle.MerkleTree

class MerkleTreeSpecification extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {
  
  type HashScheme = Blake2b
  type HashDigest = Digest32

  private val leafSize = 32

  lazy val smallInt: Gen[Int] = Gen.choose(0, 20)

  def randomBytes(length: Int = 32): Array[Byte] = {
    val r = new Array[Byte](length)
    new java.security.SecureRandom().nextBytes(r) //overrides r
    r
  }

  property("Proof generation by element") {
    forAll(smallInt) { N: Int =>
      whenever(N > 0) {
        val d = (0 until N).map(_ => LeafData(randomBytes(leafSize)))
        val leafs = d.map(data => Leaf[HashScheme, HashDigest](data))
        val tree = MerkleTree[HashScheme, HashDigest](d)
        leafs.foreach { l =>
          val proof = tree.proofByElement(l).get
          proof.leafData.value.sameElements(l.data) shouldBe true
          proof.valid(tree.rootHash) shouldBe true
        }
      }
    }
  }

  property("Proof generation by index") {
    forAll(smallInt) { N: Int =>
      whenever(N > 0) {
        val d = (0 until N).map(_ => LeafData @@ scorex.utils.Random.randomBytes(LeafSize))
        val tree = MerkleTree(d)
        (0 until N).foreach { i =>
          tree.proofByIndex(i).get.leafData shouldEqual d(i)
          tree.proofByIndex(i).get.valid(tree.rootHash) shouldBe true
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
    val tree = MerkleTree(Seq.empty)(hf)
    tree.rootHash shouldEqual Array.fill(hf.DigestSize)(0: Byte)
  }

  property("Tree creation from 1 element") {
    forAll { d: Array[Byte] =>
      whenever(d.length > 0) {
        val tree = MerkleTree(Seq(LeafData @@ d))(hf)
        tree.rootHash shouldEqual
          hf.prefixedHash(MerkleTree.InternalNodePrefix, hf.prefixedHash(MerkleTree.LeafPrefix, d))
      }
    }
  }

  property("Tree creation from 5 elements") {
    forAll { d: Array[Byte] =>
      whenever(d.length > 0) {
        val leafs: Seq[LeafData] = (0 until 5).map(_ => LeafData @@ d)
        val tree = MerkleTree(leafs)(hf)
        val h0x = hf.prefixedHash(MerkleTree.LeafPrefix, d)
        val h10 = hf.prefixedHash(MerkleTree.InternalNodePrefix, h0x, h0x)
        val h11 = h10
        val h12 = hf.prefixedHash(MerkleTree.InternalNodePrefix, h0x)
        val h20 = hf.prefixedHash(MerkleTree.InternalNodePrefix, h10, h11)
        val h21 = hf.prefixedHash(MerkleTree.InternalNodePrefix, h12)
        val h30 = hf.prefixedHash(MerkleTree.InternalNodePrefix, h20, h21)
        h30 shouldEqual tree.rootHash
      }
    }
  }

  property("Tree creation from 2 element") {
    forAll { (d1: Array[Byte], d2: Array[Byte]) =>
      val tree = MerkleTree(Seq(LeafData @@ d1, LeafData @@ d2))(hf)
      tree.rootHash shouldEqual
        hf.prefixedHash(MerkleTree.InternalNodePrefix,
          hf.prefixedHash(MerkleTree.LeafPrefix, d1),
          hf.prefixedHash(MerkleTree.LeafPrefix, d2))
    }
  }

  property("Tree creation from a lot of elements") {
    forAll { d: Seq[Array[Byte]] =>
      whenever(d.nonEmpty) {
        val tree = MerkleTree(d.map(a => LeafData @@ a))
        tree.rootHash
      }
    }
  }
}