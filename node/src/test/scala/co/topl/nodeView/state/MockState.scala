package co.topl.nodeView.state

import co.topl.modifier.ModifierId
import co.topl.nodeView.history.InMemoryKeyValueStore
import co.topl.utils.NetworkPrefixTestHelper

object MockState extends NetworkPrefixTestHelper {

  def empty: State =
    new State(
      ModifierId.empty,
      InMemoryKeyValueStore.empty,
      new TokenBoxRegistry(InMemoryKeyValueStore.empty),
      new ProgramBoxRegistry(InMemoryKeyValueStore.empty)
    )
}
