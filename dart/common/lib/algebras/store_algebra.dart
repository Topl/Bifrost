abstract class StoreAlgebra<Key, T> {
  Future<T?> get(Key id);
  Future<bool> contains(Key id);
  Future<void> put(Key id, T value);
  Future<void> remove(Key id);

  Future<T> getOrRaise(Key id) async {
    final maybeValue = await get(id);
    ArgumentError.checkNotNull(maybeValue, "id=$id");
    return maybeValue!;
  }
}
