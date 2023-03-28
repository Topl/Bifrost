import 'package:bifrost_common/algebras/event_sourced_state_algebra.dart';
import 'package:bifrost_common/algebras/parent_child_tree_algebra.dart';
import 'package:fpdart/fpdart.dart';
import 'package:mutex/mutex.dart';

class EventTreeState<State, Id> extends EventSourcedStateAlgebra<State, Id> {
  final Future<State> Function(State, Id) applyEvent;
  final Future<State> Function(State, Id) unapplyEvent;
  final ParentChildTreeAlgebra<Id> parentChildTree;
  State currentState;
  Id currentEventId;
  final Future<void> Function(Id) currentEventChanged;
  final _mutex = Mutex();

  EventTreeState(
    this.applyEvent,
    this.unapplyEvent,
    this.parentChildTree,
    this.currentState,
    this.currentEventId,
    this.currentEventChanged,
  );

  @override
  Future<State> stateAt(eventId) => useStateAt(eventId, (t) => Future.value(t));

  @override
  Future<U> useStateAt<U>(Id eventId, Future<U> Function(State p1) f) {
    return _mutex.protect(() async {
      if (eventId == currentEventId)
        return f(currentState);
      else {
        final applyUnapplyChains =
            await parentChildTree.findCommmonAncestor(currentEventId, eventId);
        await _unapplyEvents(applyUnapplyChains.first.sublist(1),
            applyUnapplyChains.first.first);
        await _applyEvents(applyUnapplyChains.second.sublist(1));
      }
      return f(currentState);
    });
  }

  _unapplyEvents(List<Id> eventIds, Id newEventId) async {
    final indexedEventIds =
        eventIds.mapWithIndex((t, index) => Tuple2(t, index)).toList().reversed;
    for (final idIndex in indexedEventIds) {
      final newState = await unapplyEvent(currentState, idIndex.first);
      final nextEventId =
          idIndex.second == 0 ? newEventId : eventIds[idIndex.second];
      currentState = newState;
      currentEventId = nextEventId;
      await currentEventChanged(nextEventId);
    }
  }

  _applyEvents(Iterable<Id> eventIds) async {
    for (final eventId in eventIds) {
      currentState = await applyEvent(currentState, eventId);
      currentEventId = eventId;
      await currentEventChanged(eventId);
    }
  }
}
