package co.topl.consensus

import co.topl.db.LDBVersionedStore
import co.topl.utils.CommonGenerators
import com.google.common.primitives.Longs
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.crypto.hash.Blake2b256
import scorex.util.encode.Base58

class ConsensusStorageSpec
    extends AnyFlatSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with CommonGenerators
    with MockFactory {

  "totalStake" should "return default total stake after no updates with empty storage" in {
    forAll(positiveMediumIntGen) { defaultTotalStake =>
      val storage = new ConsensusStorage(None, defaultTotalStake)

      storage.totalStake shouldBe defaultTotalStake
    }
  }

  "totalStake" should "load total stake from storage on start" in {
    forAll(positiveInt128Gen) { storageTotalStake =>
      val store = mock[LDBVersionedStore]
      (store
        .get(_: Array[Byte]))
        .expects(*)
        .onCall { key: Array[Byte] =>
          if (key sameElements Blake2b256("totalStake".getBytes)) {
            Some(storageTotalStake.toByteArray)
          } else Some(Longs.toByteArray(0))
        }
        .anyNumberOfTimes()

      val storage = new ConsensusStorage(Some(store), 100000)

      storage.totalStake shouldBe storageTotalStake
    }
  }

  "totalStake" should "return default total stake when storage does not contain value" in {
    forAll(positiveMediumIntGen) { defaultTotalStake =>
      val store = mock[LDBVersionedStore]
      (store
        .get(_: Array[Byte]))
        .expects(*)
        .returns(None)
        .anyNumberOfTimes()

      val storage = new ConsensusStorage(None, defaultTotalStake)

      storage.totalStake shouldBe defaultTotalStake
    }
  }

}
