import 'dart:typed_data';

import 'package:collection/collection.dart';

extension IterableEqOps<T> on Iterable<T> {
  bool sameElements(Iterable<T> other) =>
      const IterableEquality().equals(this, other);
}

extension Uint8ListOps on Uint8List {
  Int8List get int8List {
    final result = Int8List(this.length);
    for (int i = 0; i < this.length; i++) {
      result[i] = this[i] & 0xff;
    }
    return result;
  }
}
