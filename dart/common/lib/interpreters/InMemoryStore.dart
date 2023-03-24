import 'package:bifrost_common/algebras/store_algebra.dart';

class InMemoryStore<Key, Value> extends StoreAlgebra<Key, Value> {
  Map<Key, Value> _entries = {};

  @override
  Future<bool> contains(Key id) => Future.sync(() => _entries.containsKey(id));

  @override
  Future<Value?> get(Key id) => Future.sync(() => _entries[id]);

  @override
  Future<void> put(Key id, Value value) =>
      Future.sync(() => _entries[id] = value);

  @override
  Future<void> remove(Key id) => Future.sync(() => _entries.remove(id));
}
