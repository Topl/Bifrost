import 'package:bifrost_crypto/impl/ec.dart';
import 'package:cryptography/cryptography.dart';
import 'package:fpdart/fpdart.dart';

class Ed25519VRF {
  Ed25519VRF() {
    cofactor[0] = 8;
    oneScalar[0] = 1;
    ec.pointSetNeutralAccum(NP);
    ec.encodePoint(NP, neutralPointBytes, 0);
  }

  static const suite = [3];
  final cofactor = List.filled(EC.SCALAR_BYTES, 0, growable: false);
  static final zeroScalar = List.filled(EC.SCALAR_BYTES, 0, growable: false);
  static final oneScalar = List.filled(EC.SCALAR_BYTES, 0, growable: false);
  final np = List.filled(EC.SCALAR_INTS, 0, growable: false);
  final nb = List.filled(EC.SCALAR_INTS, 0, growable: false);
  static const C_BYTES = 16;
  static const PI_BYTES = EC.POINT_BYTES + EC.SCALAR_BYTES + C_BYTES;
  static final neutralPointBytes =
      List.filled(EC.POINT_BYTES, 0, growable: false);
  final NP = PointAccum.fromField(ec.x25519Field);

  Future<Ed25519VRFKeyPair> generateKeyPair() async {
    final random = SecureRandom.safe;
    final seed = List.generate(32, (index) => random.nextInt(256));
    return generateKeyPairFromSeed(seed);
  }

  Future<Ed25519VRFKeyPair> generateKeyPairFromSeed(List<int> seed) async {
    assert(seed.length == 32);
    final sk = (await Sha512().hash(seed)).bytes.sublist(0, 32);
    final vk = await getVerificationKey(sk);
    return Ed25519VRFKeyPair(sk: sk, vk: vk);
  }

  Future<List<int>> getVerificationKey(List<int> secretKey) async {
    assert(secretKey.length == 32);
    final h = (await Sha512().hash(secretKey)).bytes;
    final s = List.filled(EC.SCALAR_BYTES, 0, growable: false);
    ec.pruneScalar(h, 0, s);
    final vk = List.filled(32, 0, growable: false);
    ec.scalarMultBaseEncoded(s, vk, 0);
    return vk;
  }

  Future<bool> verify(
      List<int> signature, List<int> message, List<int> vk) async {
    assert(signature.length == 64);
    assert(vk.length == 32);
    final gamma_str = signature.take(EC.POINT_BYTES).toList();
    final c = gamma_str.sublist(EC.POINT_BYTES, EC.POINT_BYTES + C_BYTES)
      ..addAll(List.filled(EC.SCALAR_BYTES - C_BYTES, 0, growable: false));
    final s = signature.drop(EC.POINT_BYTES + C_BYTES).toList();
    final H = await _hashToCurveTryAndIncrement(vk, message);
    final gamma = PointExt.fromField(ec.x25519Field);
    final Y = PointExt.fromField(ec.x25519Field);
    ec.decodePointVar(gamma_str, 0, false, gamma);
    ec.decodePointVar(vk, 0, false, Y);
    final A = PointAccum.fromField(ec.x25519Field);
    final B = PointAccum.fromField(ec.x25519Field);
    final C = PointAccum.fromField(ec.x25519Field);
    final D = PointAccum.fromField(ec.x25519Field);
    final U = PointAccum.fromField(ec.x25519Field);
    final V = PointAccum.fromField(ec.x25519Field);
    final g = PointAccum.fromField(ec.x25519Field);
    final t = PointExt.fromField(ec.x25519Field);
    ec.scalarMultBase(s, A);
    ec.decodeScalar(c, 0, np);
    ec.decodeScalar(zeroScalar, 0, nb);
    ec.scalarMultStraussVar(nb, np, Y, B);
    ec.decodeScalar(s, 0, np);
    ec.decodeScalar(zeroScalar, 0, nb);
    ec.scalarMultStraussVar(nb, np, ec.pointCopyAccum(H.first), C);
    ec.decodeScalar(c, 0, np);
    ec.decodeScalar(zeroScalar, 0, nb);
    ec.scalarMultStraussVar(nb, np, gamma, D);
    ec.decodeScalar(oneScalar, 0, np);
    ec.decodeScalar(zeroScalar, 0, nb);
    ec.pointAddVar2(true, ec.pointCopyAccum(A), ec.pointCopyAccum(B), t);
    ec.scalarMultStraussVar(nb, np, t, U);
    ec.pointAddVar2(true, ec.pointCopyAccum(C), ec.pointCopyAccum(D), t);
    ec.scalarMultStraussVar(nb, np, t, V);
    ec.scalarMultStraussVar(nb, np, gamma, g);
    final cp = await _hashPoints(H.first, g, U, V);
    return c == cp;
  }

  Future<List<int>> sign(List<int> sk, List<int> message) async {
    assert(sk.length == 32);
    final x = await _pruneHash(sk);
    final pk = List.filled(EC.SCALAR_BYTES, 0x00, growable: false);
    ec.scalarMultBaseEncoded(x, pk, 0);
    final H = await _hashToCurveTryAndIncrement(pk, message);
    final gamma = PointAccum.fromField(ec.x25519Field);
    ec.decodeScalar(x, 0, np);
    ec.decodeScalar(zeroScalar, 0, nb);
    ec.scalarMultStraussVar(nb, np, ec.pointCopyAccum(H.first), gamma);
    final k = await _nonceGenerationRFC8032(sk, H.second);
    assert(ec.checkScalarVar(k));
    final kB = PointAccum.fromField(ec.x25519Field);
    final kH = PointAccum.fromField(ec.x25519Field);
    ec.scalarMultBase(k, kB);
    ec.decodeScalar(k, 0, np);
    ec.decodeScalar(zeroScalar, 0, nb);
    ec.scalarMultStraussVar(nb, np, ec.pointCopyAccum(H.first), kH);
    final c = await _hashPoints(H.first, gamma, kB, kH);
    final s = ec.calculateS(k, c, x);
    final gamma_str = List.filled(EC.POINT_BYTES, 0x00, growable: false);
    ec.encodePoint(gamma, gamma_str, 0);
    final pi = <int>[]
      ..addAll(gamma_str)
      ..addAll(c.take(C_BYTES))
      ..addAll(s);
    assert(pi.length == PI_BYTES);
    return pi;
  }

  Future<List<int>> proofToHash(List<int> signature) async {
    assert(signature.length == 80);
    final gamma_str = signature.sublist(0, EC.POINT_BYTES);
    final zero = [0x00];
    final three = [0x03];
    final gamma = PointExt.fromField(ec.x25519Field);
    final cg = PointAccum.fromField(ec.x25519Field);
    ec.decodePointVar(gamma_str, 0, false, gamma);
    ec.decodeScalar(cofactor, 0, np);
    ec.decodeScalar(zeroScalar, 0, nb);
    ec.scalarMultStraussVar(nb, np, gamma, cg);
    final cg_enc = List.filled(EC.POINT_BYTES, 0x00, growable: false);
    ec.encodePoint(cg, cg_enc, 0);
    final input = <int>[]
      ..addAll(suite)
      ..addAll(three)
      ..addAll(cg_enc)
      ..addAll(zero);
    return (await Sha512().hash(input)).bytes;
  }

  _pruneHash(List<int> s) async {
    final h = (await Sha512().hash(s)).bytes;
    h[0] = h[0] & 0xf8;
    h[EC.SCALAR_BYTES - 1] = h[EC.SCALAR_BYTES - 1] & 0x7f;
    h[EC.SCALAR_BYTES - 1] = h[EC.SCALAR_BYTES - 1] | 0x40;
    return h;
  }

  _hashToCurveTryAndIncrement(List<int> Y, List<int> a) async {
    int ctr = 0;
    final one = [0x01];
    final zero = [0x00];
    List<int> hash = List.filled(EC.POINT_BYTES, 0x00, growable: false);
    final H = PointExt.fromField(ec.x25519Field);
    final HR = PointAccum.fromField(ec.x25519Field);
    bool isPoint = false;
    while (!isPoint) {
      final ctr_byte = [ctr];
      final input = <int>[]
        ..addAll(suite)
        ..addAll(one)
        ..addAll(Y)
        ..addAll(a)
        ..addAll(ctr_byte)
        ..addAll(zero);
      final output = (await Sha512().hash(input)).bytes;
      for (int i = 0; i < EC.POINT_BYTES; i++) hash[i] = output[i];
      isPoint = ec.decodePointVar(hash, 0, false, H);
      if (isPoint) {
        isPoint != _isNeutralPoint(H);
      }
      ctr += 1;
    }

    ec.decodeScalar(cofactor, 0, np);
    ec.decodeScalar(zeroScalar, 0, nb);
    ec.scalarMultStraussVar(nb, np, H, HR);
    ec.encodePoint(HR, hash, 0);
    return Tuple2(HR, hash);
  }

  _isNeutralPoint(PointExt p) {
    final pBytes = List.filled(EC.POINT_BYTES, 0x00, growable: false);
    final pA = PointAccum.fromField(ec.x25519Field);
    ec.decodeScalar(oneScalar, 0, np);
    ec.decodeScalar(zeroScalar, 0, nb);
    ec.scalarMultStraussVar(nb, np, p, pA);
    ec.encodePoint(pA, pBytes, 0);
    return pBytes == neutralPointBytes;
  }

  _nonceGenerationRFC8032(List<int> sk, List<int> h) async {
    final sk_hash = (await Sha512().hash(sk)).bytes;
    final trunc_hashed_sk = <int>[]
      ..addAll(sk_hash.sublist(EC.SCALAR_BYTES))
      ..addAll(h);
    final out = (await Sha512().hash(trunc_hashed_sk)).bytes;
    return ec.reduceScalar(out);
  }

  _hashPoints(
      PointAccum p1, PointAccum p2, PointAccum p3, PointAccum p4) async {
    final zero = [0x00];
    final two = [0x02];
    final str = <int>[]
      ..addAll(suite)
      ..addAll(two);
    final r = List.filled(EC.POINT_BYTES, 0x00, growable: false);
    ec.encodePoint(p1, r, 0);
    str.addAll(r);
    ec.encodePoint(p2, r, 0);
    str.addAll(r);
    ec.encodePoint(p3, r, 0);
    str.addAll(r);
    ec.encodePoint(p4, r, 0);
    str.addAll(r);
    str.addAll(zero);
    final out = (await Sha512().hash(str)).bytes;
    return <int>[]
      ..addAll(out.sublist(0, C_BYTES))
      ..addAll(List.filled(EC.SCALAR_BYTES - C_BYTES, 0x00, growable: false));
  }
}

final ed25519Vrf = Ed25519VRF();

class Ed25519VRFKeyPair {
  final List<int> sk;
  final List<int> vk;

  Ed25519VRFKeyPair({required this.sk, required this.vk});
}
