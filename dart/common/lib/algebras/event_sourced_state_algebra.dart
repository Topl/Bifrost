abstract class EventSourcedStateAlgebra<State, Id> {
  Future<State> stateAt(Id eventId);
  Future<U> useStateAt<U>(Id eventId, Future<U> Function(State) f);
}
