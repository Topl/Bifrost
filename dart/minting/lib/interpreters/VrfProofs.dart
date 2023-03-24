import 'package:bifrost_common/algebras/ClockAlgebra.dart';
import 'package:bifrost_common/models/ratio.dart';
import 'package:bifrost_minting/algebras/vrf_calculator_algebra.dart';
import 'package:fpdart/src/tuple.dart';
import 'package:fixnum/fixnum.dart';

class VrfProofs extends VrfCalculatorAlgebra {
  final ClockAlgebra clock;
  // TODO: VrfEd25519

  Map<List<int>, Map<Int64, List<int>>> _vrfProofsCache = {};
  Map<List<int>, Map<Int64, List<int>>> _rhosCache = {};

  VrfProofs(this.clock);

  @override
  Future<List<Int64>> ineligibleSlots(Int64 epoch, List<int> eta,
      Tuple2<Int64, Int64> slotRange, Ratio relativeStake) async {
    if (!_rhosCache.containsKey(eta)) _precomputeForEpoch(epoch, eta);
    final rhosMap = _rhosCache[eta];
    final Ratio threshold = Ratio(BigInt.one, BigInt.one); // TODO
    return rhosMap!.entries
        .map((entry) {
          // TODO: Is Slot Leader?
          return Tuple2(entry.key, false);
        })
        .where((t) => t.second)
        .map((t) => t.first)
        .toList();
  }

  @override
  Future<List<int>> proofForSlot(Int64 slot, List<int> eta) async {
    if (!_vrfProofsCache.containsKey(eta)) {
      final epoch = clock.epochOfSlot(slot);
      _precomputeForEpoch(epoch, eta);
    }
    final p = _vrfProofsCache[eta]!;
    return p[slot]!;
  }

  @override
  Future<List<int>> rhoForSlot(Int64 slot, List<int> eta) async {
    if (!_rhosCache.containsKey(eta)) {
      final epoch = clock.epochOfSlot(slot);
      _precomputeForEpoch(epoch, eta);
    }
    final r = _rhosCache[eta]!;
    return r[slot]!;
  }

  _precomputeForEpoch(Int64 epoch, List<int> eta) async {
    final boundary = clock.epochRange(epoch);
    final vrfProofs = <Int64, List<int>>{};
    final rhoValues = <Int64, List<int>>{};
    var slot = boundary.first;
    while (slot <= boundary.second) {
      // TODO
      vrfProofs[slot] = List.filled(80, 0);
      // TODO
      rhoValues[slot] = [];
      slot = slot + 1;
    }
    // TODO: Evict old entries
    _vrfProofsCache[eta] = vrfProofs;
    _rhosCache[eta] = rhoValues;
  }
}
