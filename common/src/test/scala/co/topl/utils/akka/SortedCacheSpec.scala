package co.topl.utils.akka

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SortedCacheSpec extends AnyFlatSpec with Matchers {

  behavior of "PoppableItemsCache.Impl"

  private val limit = 50

  it should "pop None when empty" in {
    val empty = SortedCache.Impl[TestItem](Nil, limit)
    val (nextCache, popped) =
      empty.pop(_ => true)

    popped shouldBe None
    nextCache shouldBe empty
  }

  it should "insert and pop a value" in {
    val empty = SortedCache.Impl[TestItem](Nil, limit)
    val withItem =
      empty.append(List(TestItem("1a", 1)))
    val (nextCache, popped) =
      withItem.pop(_ => true)
    nextCache shouldBe empty
    popped shouldBe Some(TestItem("1a", 1))
  }

  it should "evict a non-viable candidate after max attempts" in {
    val limit = 3
    val empty = SortedCache.Impl[TestItem](Nil, limit)
    val withItem =
      empty.append(List(TestItem("1a", 1)))

    val implAfter2Attempts =
      (0 until (limit - 1)).foldLeft(withItem) { (impl, _) =>
        val (next, popped) =
          impl.pop(_ => false)
        popped shouldBe None
        next should not equal empty
        next
      }

    val (next, popped) =
      implAfter2Attempts.pop(_ => false)
    popped shouldBe None
    next shouldBe empty
  }

  it should "pop a sorted candidate if it is viable" in {
    val empty = SortedCache.Impl[TestItem](Nil, limit)
    val withItems =
      empty.append(List(TestItem("2a", 2), TestItem("1a", 1)))

    val (withoutItem1a, popped1a) =
      withItems.pop(_ => true)

    popped1a shouldBe Some(TestItem("1a", 1))

    val (withoutItem2a, popped2a) =
      withoutItem1a.pop(_ => true)

    popped2a shouldBe Some(TestItem("2a", 2))

    withoutItem2a shouldBe empty
  }

  it should "not pop a sorted candidate if it is not viable" in {
    val viableCandidates = Set("2a")
    val empty = SortedCache.Impl[TestItem](Nil, limit)
    val withItems =
      empty.append(List(TestItem("2a", 2), TestItem("1a", 1)))

    val (withoutItem2a, popped2a) =
      withItems.pop(item => viableCandidates.contains(item.id))

    popped2a shouldBe Some(TestItem("2a", 2))

    withoutItem2a.pop(item => viableCandidates.contains(item.id))._2 shouldBe None

  }

}

private case class TestItem(id: String, height: Int)

private object TestItem {
  implicit val ordering: Ordering[TestItem] = (a, b) => a.height.compareTo(b.height)
}
