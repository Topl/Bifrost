import 'package:topl_protobuf/consensus/models/slot_data.pb.dart';
import 'package:bifrost_common/models/common.dart';
import 'package:fixnum/fixnum.dart';

abstract class EtaCalculationAlgebra {
  Future<Eta> etaToBe(SlotId parentSlotId, Int64 childSlot);
}
