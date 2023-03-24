import 'package:bifrost_common/models/common.dart';
import 'package:bifrost_common/utils.dart';
import 'package:bifrost_consensus/algebras/leader_election_validation_algebra.dart';
import 'package:bifrost_consensus/models/vrf_config.dart';
import 'package:bifrost_consensus/utils.dart';
import 'package:rational/rational.dart';
import 'package:fixnum/fixnum.dart';

// TODO Caching
class LeaderElectionValidation extends LeaderElectionValidationAlgebra {
  final VrfConfig config;
  final Rational Function(Rational) log1p;
  final Rational Function(Rational) exp;

  LeaderElectionValidation(this.config, this.log1p, this.exp);

  static final NormalizationConstant = BigInt.from(2).pow(512);

  @override
  Future<Rational> getThreshold(Rational relativeStake, Int64 slotDiff) async {
    final difficultyCurve = (slotDiff > config.lddCutoff)
        ? config.baselineDifficulty
        : Rational(BigInt.parse(slotDiff.toString()),
                BigInt.parse(config.lddCutoff.toString())) *
            config.amplitude;

    if (difficultyCurve == Rational.one)
      return difficultyCurve;
    else {
      final coefficient = log1p(Rational.fromInt(-1) * difficultyCurve);
      final result = exp(coefficient * relativeStake);
      return Rational.one - result;
    }
  }

  @override
  Future<bool> isSlotLeaderForThreshold(Rational threshold, Rho rho) async {
    final testRhoHashBytes = rho.rhoTestHash;
    final test = Rational(
        ([0]..addAll(testRhoHashBytes)).toBigInt, NormalizationConstant);
    return threshold > test;
  }
}
