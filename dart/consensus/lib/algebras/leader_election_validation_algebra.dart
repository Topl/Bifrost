import 'package:bifrost_common/models/common.dart';
import 'package:rational/rational.dart';
import 'package:fixnum/fixnum.dart';

abstract class LeaderElectionValidationAlgebra {
  Future<Rational> getThreshold(Rational relativeStake, Int64 slotDiff);
  Future<bool> isSlotLeaderForThreshold(Rational threshold, Rho rho);
}
