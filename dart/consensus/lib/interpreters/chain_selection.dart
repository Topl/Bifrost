import 'dart:async';

import 'package:bifrost_codecs/codecs.dart';
import 'package:bifrost_common/utils.dart';
import 'package:bifrost_consensus/algebras/chain_selection_algebra.dart';
import 'package:bifrost_consensus/utils.dart';
import 'package:topl_protobuf/consensus/models/block_id.pb.dart';
import 'package:topl_protobuf/consensus/models/slot_data.pb.dart';

class ChainSelection extends ChainSelectionAlgebra {
  final FutureOr<SlotData> Function(BlockId) fetchSlotData;

  ChainSelection(this.fetchSlotData);

  @override
  FutureOr<BlockId> select(BlockId a, BlockId b) async {
    // TODO: Density chain selection
    final slotDataA = await fetchSlotData(a);
    final slotDataB = await fetchSlotData(b);
    if (slotDataA.height > slotDataB.height)
      return a;
    else if (slotDataB.height > slotDataA.height)
      return b;
    else if (slotDataA.slotId.slot < slotDataB.slotId.slot)
      return a;
    else if (slotDataB.slotId.slot < slotDataA.slotId.slot)
      return b;
    else if (slotDataA.rho.rhoTestHash.toBigInt >
        slotDataB.rho.rhoTestHash.toBigInt)
      return a;
    else
      return b;
  }
}
