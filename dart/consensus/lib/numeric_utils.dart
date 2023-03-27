import 'package:rational/rational.dart';

Rational log1p(Rational x) {
  a(int j) => (j == 0)
      ? Rational.zero
      : (j == 1)
          ? x
          : Rational.fromInt(j - 1) * Rational.fromInt(j - 1) * x;
  b(int j) => (j == 0)
      ? Rational.zero
      : (j == 1)
          ? Rational.one
          : Rational.fromInt(j) - Rational.fromInt(j - 1) * x;

  return _modifiedLenz(10000, 16, a, b);
}

Rational exp(Rational x) {
  a(int j) {
    switch (j) {
      case 0:
        return Rational.zero;
      case 1:
        return Rational.one;
      case 2:
        return Rational.fromInt(-1) * x;
      default:
        return Rational.fromInt(-j + 2) * x;
    }
  }

  b(int j) {
    switch (j) {
      case 0:
        return Rational.zero;
      case 1:
        return Rational.one;
      default:
        return Rational.fromInt(j - 1) + x;
    }
  }

  if (x == Rational.zero) return Rational.one;
  return _modifiedLenz(10000, 38, a, b);
}

_modifiedLenz(int maxIterations, int precision, Rational Function(int) a,
    Rational Function(int) b) {
  final bigFactor = BigInt.from(10).pow(precision + 10);
  final tinyFactor = Rational(BigInt.from(1), bigFactor);
  final truncationError =
      Rational(BigInt.from(1), BigInt.from(10).pow(precision + 1));
  var fj = b(0) == Rational.zero ? tinyFactor : b(0);
  var cj = fj;
  var dj = Rational.zero;
  var deltaj = Rational.one;
  var error = true;
  loop(int j) {
    dj = b(j) + a(j) * dj;
    if (dj == Rational.zero) dj = tinyFactor;
    cj = b(j) + a(j) / cj;
    if (cj == Rational.zero) cj = tinyFactor;
    dj = Rational(dj.denominator, dj.numerator);
    deltaj = cj * dj;
    fj = fj * deltaj;
    if (j > 1)
      error = (deltaj - Rational.one).abs() > truncationError;
    else
      error = true;
  }

  var j = 1;
  while (j < maxIterations + 1 && error) {
    loop(j);
    j = j + 1;
  }
  if (fj.denominator < BigInt.from(0))
    fj = Rational(-fj.numerator, -fj.denominator);
  return fj;
}
