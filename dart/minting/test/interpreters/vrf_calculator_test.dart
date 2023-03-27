import 'package:bifrost_common/algebras/clock_algebra.dart';
import 'package:bifrost_consensus/algebras/leader_election_validation_algebra.dart';
import 'package:bifrost_consensus/models/vrf_config.dart';
import 'package:bifrost_crypto/utils.dart';
import 'package:bifrost_minting/interpreters/vrf_calculator.dart';
import 'package:convert/convert.dart';
import 'package:rational/rational.dart';
import 'package:test/test.dart';
import 'package:mockito/annotations.dart';
import 'package:mockito/mockito.dart';
import 'package:fixnum/fixnum.dart';

@GenerateNiceMocks(
    [MockSpec<ClockAlgebra>(), MockSpec<LeaderElectionValidationAlgebra>()])
import 'vrf_calculator_test.mocks.dart';

void main() {
  group("VrfCalculator", () {
    test("proofForSlot", () async {
      final skVrf = List.filled(32, 0x00);
      final config = VrfConfig(
        lddCutoff: 15,
        precision: 40,
        baselineDifficulty: Rational.fromInt(1, 20),
        amplitude: Rational.fromInt(1, 2),
      );
      final calculator = VrfCalculator(skVrf, MockClockAlgebra(),
          MockLeaderElectionValidationAlgebra(), config, 100);

      final eta = List.filled(32, 0x00);
      final proof = await calculator.proofForSlot(Int64(10), eta);
      final expectedProof = hex.decode(
          "bc31a2fb46995ffbe4b316176407f57378e2f3d7fee57d228a811194361d8e7040c9d15575d7a2e75506ffe1a47d772168b071a99d2e85511730e9c21397a1cea0e7fa4bd161e6d5185a94a665dd190d");
      expect(proof.sameElements(expectedProof), true);
    });
  });
}
