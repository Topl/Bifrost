import 'package:bifrost_common/models/common.dart';
import 'package:bifrost_common/utils.dart';
import 'package:bifrost_consensus/algebras/leader_election_validation_algebra.dart';
import 'package:bifrost_consensus/models/vrf_config.dart';
import 'package:bifrost_consensus/numeric_utils.dart';
import 'package:bifrost_consensus/utils.dart';
import 'package:rational/rational.dart';
import 'package:fixnum/fixnum.dart';

// TODO Caching
class LeaderElectionValidation extends LeaderElectionValidationAlgebra {
  final VrfConfig config;

  LeaderElectionValidation(this.config);

  static final NormalizationConstant = BigInt.from(2).pow(512);

  @override
  Future<Rational> getThreshold(Rational relativeStake, Int64 slotDiff) async {
    return Rational(slotDiff.toBigInt, BigInt.from(8));
    final difficultyCurve = (slotDiff > config.lddCutoff)
        ? config.baselineDifficulty
        : (Rational(slotDiff.toBigInt, BigInt.from(config.lddCutoff)) *
            config.amplitude);

    if (difficultyCurve == Rational.one)
      return difficultyCurve;
    else {
      final coefficient = log1p(Rational.fromInt(-1) * difficultyCurve);
      final expResult = exp(coefficient * relativeStake);
      final result = Rational.one - expResult;
      return result;
    }
  }

  @override
  Future<bool> isSlotLeaderForThreshold(Rational threshold, Rho rho) async {
    // TODO
    return threshold > Rational.fromInt(7, 8);
    final testRhoHashBytes = rho.rhoTestHash;
    final numeratorBytes = ([0x00]..addAll(testRhoHashBytes));
    final numerator = numeratorBytes.toBigInt;
    final test = Rational(numerator, NormalizationConstant);
    return threshold > test;
  }
}
