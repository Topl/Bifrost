import 'package:bifrost_common/algebras/parent_child_tree_algebra.dart';
import 'package:fixnum/fixnum.dart';
import 'package:fpdart/fpdart.dart';

class ParentChildTree<T> extends ParentChildTreeAlgebra<T> {
  final Future<Tuple2<Int64, T>?> Function(T) read;
  final Future<void> Function(T, Tuple2<Int64, T>) write;
  final T root;

  ParentChildTree(this.read, this.write, this.root);

  @override
  Future<void> assocate(T child, T parent) async {
    if (parent == root)
      await write(child, Tuple2(Int64(1), parent));
    else {
      final heightId = _readOrRaise(parent);
      await write(child, Tuple2(heightId.first + 1, parent));
    }
  }

  @override
  Future<Tuple2<List<T>, List<T>>> findCommmonAncestor(T a, T b) async {
    if (a == b)
      return Tuple2([a], [b]);
    else {
      final aHeight = await heightOf(a);
      final bHeight = await heightOf(b);
      late List<T> aChain;
      late List<T> bChain;
      if (aHeight == bHeight) {
        aChain = [a];
        bChain = [b];
      } else if (aHeight < bHeight) {
        aChain = [a];
        bChain = (await _traverseBackToHeight([b], bHeight, aHeight)).first;
      } else {
        aChain = (await _traverseBackToHeight([a], aHeight, bHeight)).first;
        bChain = [b];
      }

      while (aChain.first != bChain.first) {
        aChain.insert(0, (await _readOrRaise(aChain.first)).second);
        bChain.insert(0, (await _readOrRaise(bChain.first)).second);
      }
      return Tuple2(aChain, bChain);
    }
  }

  @override
  Future<Int64> heightOf(T t) async {
    if (t == root)
      return Int64.ZERO;
    else
      return (await _readOrRaise(t)).first;
  }

  @override
  Future<T?> parentOf(T t) async {
    if (t == root) return null;
    final v = await read(t);
    if (v != null) return v.second;
    return null;
  }

  _readOrRaise(T id) async {
    final v = await read(id);
    if (v == null) throw Exception("Element not found");
    return v;
  }

  Future<Tuple2<List<T>, Int64>> _traverseBackToHeight(
      List<T> collection, Int64 initialHeight, Int64 targetHeight) async {
    final chain = List.of(collection);
    Int64 height = initialHeight;
    while (height > targetHeight) {
      chain.insert(0, (await _readOrRaise(chain.first)).second);
      height--;
    }
    return Tuple2(chain, height);
  }
}
