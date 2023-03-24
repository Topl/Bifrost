import 'package:bifrost_common/models/ratio.dart';
import 'package:fixnum/fixnum.dart';
import 'package:fpdart/fpdart.dart';

abstract class VrfCalculatorAlgebra {
  Future<List<int>> rhoForSlot(Int64 slot, List<int> eta);
  Future<List<int>> proofForSlot(Int64 slot, List<int> eta);
  Future<List<Int64>> ineligibleSlots(Int64 epoch, List<int> eta,
      Tuple2<Int64, Int64> slotRange, Ratio relativeStake);
}
