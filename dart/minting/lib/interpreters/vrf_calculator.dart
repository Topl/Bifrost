import 'package:bifrost_common/algebras/clock_algebra.dart';
import 'package:bifrost_common/models/common.dart';
import 'package:bifrost_consensus/algebras/leader_election_validation_algebra.dart';
import 'package:bifrost_consensus/models/vrf_config.dart';
import 'package:bifrost_consensus/models/vrf_argument.dart';
import 'package:bifrost_minting/algebras/vrf_calculator_algebra.dart';
import 'package:fpdart/src/tuple.dart';
import 'package:fixnum/fixnum.dart';
import 'package:rational/rational.dart';

class VrfCalculator extends VrfCalculatorAlgebra {
  final List<int> skVrf;
  final ClockAlgebra clock;
  final LeaderElectionValidationAlgebra leaderElectionValidation;
  final VrfConfig vrfConfig;
  final int vrfCacheSize;

  Map<List<int>, Map<Int64, List<int>>> _vrfProofsCache = {};
  Map<List<int>, Map<Int64, List<int>>> _rhosCache = {};

  VrfCalculator(this.skVrf, this.clock, this.leaderElectionValidation,
      this.vrfConfig, this.vrfCacheSize);

  @override
  Future<List<Int64>> ineligibleSlots(Int64 epoch, List<int> eta,
      Tuple2<Int64, Int64>? slotRange, Rational relativeStake) async {
    final boundary = clock.epochRange(epoch);
    final rhosList = <Slot, Rho>{};
    for (Int64 i = boundary.first; i < boundary.second; i++) {
      if (slotRange != null && i >= slotRange.first && i < slotRange.second) {
        rhosList[i] = await rhoForSlot(i, eta);
      }
    }
    final threshold = await leaderElectionValidation.getThreshold(
        relativeStake, Int64(vrfConfig.lddCutoff));
    final leaderCalculations = <Slot>[];
    for (final entry in rhosList.entries) {
      if (await leaderElectionValidation.isSlotLeaderForThreshold(
          threshold, entry.value)) leaderCalculations.add(entry.key);
    }
    return leaderCalculations;
  }

  @override
  Future<List<int>> proofForSlot(Int64 slot, List<int> eta) async {
    if (!_vrfProofsCache.containsKey(eta)) {
      final result = _compute(VrfArgument(eta, slot));
      _vrfProofsCache[eta] = result;
      return result[slot];
    }
    return _vrfProofsCache[eta]![slot]!;
  }

  @override
  Future<List<int>> rhoForSlot(Int64 slot, List<int> eta) async {
    if (!_rhosCache.containsKey(eta)) {
      final proof = proofForSlot(slot, eta);
      // TODO: Ed25519VRF
      throw UnimplementedError();
    }
    return _rhosCache[eta]![slot]!;
  }

  _compute(VrfArgument arg) {
    // TODO: Ed25519VRF
    throw UnimplementedError();
  }
}
