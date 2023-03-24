import 'package:bifrost_common/models/common.dart';
import 'package:bifrost_common/models/ratio.dart';

abstract class LeaderElectionValidationAlgebra {
  Future<Ratio> getThreshold(Ratio relativeStake, int slotDiff);
  Future<bool> isSlotLeaderForThreshold(Ratio threshold, Rho rho);
}
