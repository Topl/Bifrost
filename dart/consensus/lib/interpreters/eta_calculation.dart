import 'package:bifrost_common/algebras/clock_algebra.dart';
import 'package:bifrost_common/models/common.dart';
import 'package:bifrost_consensus/algebras/eta_calculation_algebra.dart';
import 'package:topl_protobuf/consensus/models/block_id.pb.dart';
import 'package:topl_protobuf/consensus/models/slot_data.pb.dart';
import 'package:fixnum/fixnum.dart';

class EtaCalculation extends EtaCalculationAlgebra {
  final Future<SlotData> Function(BlockId) fetchSlotData;
  final ClockAlgebra clock;
  final Eta bigBangEta;

  @override
  Future<Eta> etaToBe(SlotId parentSlotId, Int64 childSlot) async {
    if (childSlot > clock.slotsPerEpoch) return bigBangEta;
    final parentEpoch = clock.epochOfSlot(parentSlotId.slot);
    final childEpoch = clock.epochOfSlot(childSlot);
    final parentSlotData = await fetchSlotData(parentSlotId.blockId);
    if (parentEpoch == childEpoch)
      return parentSlotData.eta;
    else if (childEpoch - parentEpoch > 1)
      throw Exception("Empty Epoch");
    else
      return _calculate(await _locateTwoThirdsBest(parentSlotData));
  }

  _locateTwoThirdsBest(SlotData from) async {
    if (_isWithinTwoThirds(from))
      return from;
    else
      return _locateTwoThirdsBest(
          await fetchSlotData(from.parentSlotId.blockId));
  }

  _isWithinTwoThirds(SlotData from) =>
      from.slotId.slot % clock.slotsPerEpoch <= (clock.slotsPerEpoch * 2 ~/ 3);

  _calculate(SlotData twoThirdsBest) async {
    // TODO: Caching
    final epoch = clock.epochOfSlot(twoThirdsBest.slotId.slot);
    final epochRange = clock.epochRange(epoch);
    final epochData = <SlotData>[];
    while (epochData.first.parentSlotId.slot >= epochRange.first) {
      epochData.insert(
          0, await fetchSlotData(epochData.first.parentSlotId.blockId));
    }
    final rhoValues = epochData.map((slotData) => slotData.rho);
    return _calculateFromValues(twoThirdsBest.eta, epoch + 1, rhoValues);
  }

  Eta _calculateFromValues(
      Eta previousEta, Int64 epoch, Iterable<Rho> rhoValues) {
    final rhoNonceHashValues = rhoValues.map((rho) =>
        // TODO
        throw UnimplementedError());
  }
}
