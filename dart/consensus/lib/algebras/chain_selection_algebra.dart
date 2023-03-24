import 'dart:async';

import 'package:topl_protobuf/consensus/models/block_id.pb.dart';

abstract class ChainSelectionAlgebra {
  /**
   * Selects the "better" of the two block IDs
   */
  FutureOr<BlockId> select(BlockId a, BlockId b);
}
