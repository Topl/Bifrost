abstract class SecureStoreAlgebra {
  Future<void> write(String name, List<int> bytes);
  Future<List<int>?> consume(String name);
  Future<List<String>> list();
  Future<void> erase(String name);
}
