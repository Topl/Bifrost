import 'package:rational/rational.dart';

class VrfConfig {
  final int lddCutoff;
  final int precision;
  final Rational baselineDifficulty;
  final Rational amplitude;

  VrfConfig(
      this.lddCutoff, this.precision, this.baselineDifficulty, this.amplitude);
}
