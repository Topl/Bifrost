import 'dart:async';

import 'package:bifrost_consensus/algebras/local_chain_algebra.dart';
import 'package:topl_protobuf/consensus/models/block_id.pb.dart';

class LocalChain extends LocalChainAlgebra {
  LocalChain(BlockId initialHead) : this._currentHead = initialHead;
  BlockId _currentHead;

  final StreamController<BlockId> _streamController =
      StreamController.broadcast();

  @override
  FutureOr<void> adopt(BlockId newHead) async {
    if (_currentHead != newHead) {
      _currentHead = newHead;
      _streamController.add(newHead);
    }
  }

  @override
  Stream<BlockId> get adoptions => _streamController.stream;

  @override
  FutureOr<BlockId> get currentHead => Future.value(_currentHead);
}
