import 'package:rational/rational.dart';

class VrfConfig {
  final int lddCutoff;
  final int precision;
  final Rational baselineDifficulty;
  final Rational amplitude;

  VrfConfig({
    required this.lddCutoff,
    required this.precision,
    required this.baselineDifficulty,
    required this.amplitude,
  });
}
