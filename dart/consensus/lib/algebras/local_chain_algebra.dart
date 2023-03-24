import 'dart:async';

import 'package:topl_protobuf/consensus/models/block_id.pb.dart';

abstract class LocalChainAlgebra {
  FutureOr<void> adopt(BlockId newHead);
  FutureOr<BlockId> get currentHead;
  Stream<BlockId> get adoptions;
}
