import 'package:bifrost_crypto/impl/x25519_field.dart';
import 'package:fixnum/fixnum.dart';

class EC {
  final x25519Field = X25519Field();
  List<PointExt> _precompBaseTable = [];
  List<int> _precompBase = [];

  EC() {
    _precompute();
  }

  int mulAddTo256(List<int> x, List<int> y, List<int> zz) {
    final y_0 = y[0] & M;
    final y_1 = y[1] & M;
    final y_2 = y[2] & M;
    final y_3 = y[3] & M;
    final y_4 = y[4] & M;
    final y_5 = y[5] & M;
    final y_6 = y[6] & M;
    final y_7 = y[7] & M;
    Int64 zc = Int64.ZERO;
    for (int i = 0; i < 8; i++) {
      int c = 0;
      final x_i = x[i] & M;
      c += x_i * y_0 + (zz[i + 0] & M);
      zz[i + 0] = c;
      c >>>= 32;
      c += x_i * y_1 + (zz[i + 1] & M);
      zz[i + 1] = c;
      c >>>= 32;
      c += x_i * y_2 + (zz[i + 2] & M);
      zz[i + 2] = c;
      c >>>= 32;
      c += x_i * y_3 + (zz[i + 3] & M);
      zz[i + 3] = c;
      c >>>= 32;
      c += x_i * y_4 + (zz[i + 4] & M);
      zz[i + 4] = c;
      c >>>= 32;
      c += x_i * y_5 + (zz[i + 5] & M);
      zz[i + 5] = c;
      c >>>= 32;
      c += x_i * y_6 + (zz[i + 6] & M);
      zz[i + 6] = c;
      c >>>= 32;
      c += x_i * y_7 + (zz[i + 7] & M);
      zz[i + 7] = c;
      c >>>= 32;
      zc += c + zz[i + 8] & M;
      zz[i + 8] = zc.toInt32().toInt();
      zc >>= 32; // TODO: >>>?
    }
    return zc.toInt32().toInt();
  }

  bool gte256(List<int> x, List<int> y) {
    for (int i = 7; i >= 0; i--) {
      final x_i = x[i] ^ -0x7fffffff;
      final y_i = y[i] ^ -0x7fffffff;
      if (x_i < y_i) return false;
      if (x_i > y_i) return true;
    }
    return true;
  }

  void cmov(int len, int mask, List<int> x, int xOff, List<int> z, int zOff) {
    int maskv = mask;
    maskv = -(maskv & 1);
    for (int i = 0; i < len; i++) {
      int z_i = z[zOff + i];
      final diff = z_i ^ x[xOff + i];
      z_i ^= (diff & maskv);
      z[zOff + i] = z_i;
    }
  }

  int cadd(int len, int mask, List<int> x, List<int> y, List<int> z) {
    final m = -(mask & 1) & M;
    Int64 c = Int64.ZERO;
    for (int i = 0; i < len; i++) {
      c += (x[i] & M) + (y[i] & m);
      z[i] = c.toInt32().toInt();
      c = c >> 32; // TODO: >>>?
    }
    return c.toInt32().toInt();
  }

  int shiftDownBit(int len, List<int> z, int c) {
    int i = len;
    int cv = c;
    while (--i >= 0) {
      final next = z[i];
      z[i] = (next >>> 1) | (cv << 31);
      cv = next;
    }
    return cv << 31;
  }

  int shuffle2(int x) {
    int t = 0;
    int xv = x;
    t = (xv ^ (xv >>> 7)) & 0x00aa00aa;
    xv ^= (t ^ (t << 7));
    t = (xv ^ (xv >>> 14)) & 0x0000cccc;
    xv ^= (t ^ (t << 14));
    t = (xv ^ (xv >>> 4)) & 0x00f000f0;
    xv ^= (t ^ (t << 4));
    t = (xv ^ (xv >>> 8)) & 0x0000ff00;
    xv ^= (t ^ (t << 8));
    return xv;
  }

  bool areAllZeroes(List<int> buf, int off, int len) {
    int bits = 0;
    for (int i = 0; i < len; i++) bits |= buf[off + i];
    return bits == 0;
  }

  List<int> calculateS(List<int> r, List<int> k, List<int> s) {
    final List<int> t = List.filled(SCALAR_INTS * 2, 0);
    decodeScalar(r, 0, t);
    final List<int> u = List.filled(SCALAR_INTS, 0);
    decodeScalar(k, 0, u);
    final List<int> v = List.filled(SCALAR_INTS, 0);
    decodeScalar(s, 0, v);
    mulAddTo256(u, v, t);
    final List<int> result = List.filled(SCALAR_BYTES * 2, 0);
    for (int i = 0; i < t.length; i++) encode32(t[i], result, i * 4);
    return reduceScalar(result);
  }

  bool checkPointVar(List<int> p) {
    final t = List.filled(8, 0);
    decode32(p, 0, t, 0, 8);
    t[7] &= 0x7fffffff; // TODO?
    return !gte256(t, P);
  }

  bool checkScalarVar(List<int> s) {
    final n = List.filled(SCALAR_INTS, 0);
    decodeScalar(s, 0, n);
    return !gte256(n, L);
  }

  int decode24(List<int> bs, int off) {
    int n = bs[off] & 0xff;
    n |= (bs[off + 1] & 0xff) << 8;
    n |= (bs[off + 2] & 0xff) << 16;
    return n;
  }

  int decode32v(List<int> bs, int off) {
    int n = bs[off] & 0xff;
    n |= (bs[off + 1] & 0xff) << 8;
    n |= (bs[off + 2] & 0xff) << 16;
    n |= bs[off + 3] << 24;
    return n;
  }

  void decode32(List<int> bs, int bsOff, List<int> n, int nOff, int nLen) {
    for (int i = 0; i < nLen; i++) n[nOff + i] = decode32v(bs, bsOff + i * 4);
  }

  bool decodePointVar(List<int> p, int pOff, bool negate, PointExt r) {
    final py = p.sublist(pOff, pOff + POINT_BYTES);
    if (!checkPointVar(py)) return false;
    final x_0 = (py[POINT_BYTES - 1] & 0x80) >>> 7;
    py[POINT_BYTES - 1] = (py[POINT_BYTES - 1] & 0x7f);
    x25519Field.decode(py, 0, r.y);
    final u = x25519Field.create;
    final v = x25519Field.create;
    x25519Field.sqr(r.y, u);
    x25519Field.mul2(C_d, u, v);
    x25519Field.subOne(u);
    x25519Field.addOne1(v);
    if (!x25519Field.sqrtRatioVar(u, v, r.x)) return false;
    x25519Field.normalize(r.x);
    if (x_0 == 1 && x25519Field.isZeroVar(r.x)) return false;
    if (negate ^ (x_0 != (r.x[0] & 1))) x25519Field.negate(r.x, r.x);
    pointExtendXY(r);
    return true;
  }

  void decodeScalar(List<int> k, int kOff, List<int> n) =>
      decode32(k, kOff, n, 0, SCALAR_INTS);

  void encode24(int n, List<int> bs, int off) {
    bs[off] = n; // TODO: toByte
    bs[off + 1] = (n >>> 8); // TODO: toByte
    bs[off + 2] = n >>> 16; // TODO: toByte
  }

  void encode32(int n, List<int> bs, int off) {
    bs[off] = n; // TODO: toByte
    bs[off + 1] = n >>> 8; // TODO: toByte
    bs[off + 2] = n >>> 16; // TODO: toByte
    bs[off + 3] = n >>> 24; // TODO: toByte
  }

  void encode56(Int64 n, List<int> bs, int off) {
    encode32(n.toInt32().toInt(), bs, off);
    encode24((n >> 32).toInt32().toInt(), bs, off + 4); // TODO: >>>
  }

  void encodePoint(PointAccum p, List<int> r, int rOff) {
    final x = x25519Field.create;
    final y = x25519Field.create;
    x25519Field.inv(p.z, y);
    x25519Field.mul2(p.x, y, x);
    x25519Field.mul2(p.y, y, y);
    x25519Field.normalize(x);
    x25519Field.normalize(y);
    x25519Field.encode(y, r, rOff);
    r[rOff + POINT_BYTES - 1] =
        (r[rOff + POINT_BYTES - 1] | ((x[0] & 1) << 7)); // TODO: toByte
  }

  List<int> getWNAF(List<int> n, int width) {
    final t = List.filled(SCALAR_INTS * 2, 0);
    var tPos = t.length;
    var c = 0;
    var i = SCALAR_INTS;
    while (--i >= 0) {
      final next = n[i];
      t[--tPos] = (next >>> 16) | (c << 16);
      c = next;
      t[--tPos] = c;
    }
    final ws = List.filled(256, 0);
    final pow2 = 1 << width;
    final mask = pow2 - 1;
    final sign = pow2 >>> 1;
    var j = 0;
    var carry = 0;
    i = 0;
    while (i < t.length) {
      final word = t[i];
      while (j < 16) {
        final word16 = word >>> j;
        final bit = word16 & 1;
        if (bit == carry) {
          j += 1;
        } else {
          var digit = (word16 & mask) + carry;
          carry = digit & sign;
          digit -= (carry << 1);
          carry >>>= (width - 1);
          ws[(i << 4) + j] = digit;
          j += width;
        }
      }
      i += 1;
      j -= 16;
    }
    return ws;
  }

  void scalarMultBaseYZ(List<int> k, int kOff, List<int> y, List<int> z) {
    final n = List.filled(SCALAR_BYTES, 0);
    pruneScalar(k, kOff, n);
    final p = PointAccum.fromField(x25519Field);
    scalarMultBase(n, p);
    x25519Field.copy(p.y, 0, y, 0);
    x25519Field.copy(p.z, 0, z, 0);
  }

  void pointAddVar1(bool negate, PointExt p, PointAccum r) {
    final A = x25519Field.create;
    final B = x25519Field.create;
    final C = x25519Field.create;
    final D = x25519Field.create;
    final E = r.u;
    final F = x25519Field.create;
    final G = x25519Field.create;
    final H = r.v;
    late List<int> c;
    late List<int> d;
    late List<int> f;
    late List<int> g;
    if (negate) {
      c = D;
      d = C;
      f = G;
      g = F;
    } else {
      c = C;
      d = D;
      f = F;
      g = G;
    }
    x25519Field.apm(r.y, r.x, B, A);
    x25519Field.apm(p.y, p.x, d, c);
    x25519Field.mul2(A, C, A);
    x25519Field.mul2(B, D, B);
    x25519Field.mul2(r.u, r.v, C);
    x25519Field.mul2(C, p.t, C);
    x25519Field.mul2(C, C_d2, C);
    x25519Field.mul2(r.z, p.z, D);
    x25519Field.add(D, D, D);
    x25519Field.apm(B, A, H, E);
    x25519Field.apm(D, C, g, f);
    x25519Field.carry(g);
    x25519Field.mul2(E, F, r.x);
    x25519Field.mul2(G, H, r.y);
    x25519Field.mul2(F, G, r.z);
  }

  void pointAddVar2(bool negate, PointExt p, PointExt q, PointExt r) {
    final A = x25519Field.create;
    final B = x25519Field.create;
    final C = x25519Field.create;
    final D = x25519Field.create;
    final E = x25519Field.create;
    final F = x25519Field.create;
    final G = x25519Field.create;
    final H = x25519Field.create;
    late List<int> c;
    late List<int> d;
    late List<int> f;
    late List<int> g;
    if (negate) {
      c = D;
      d = C;
      f = G;
      g = F;
    } else {
      c = C;
      d = D;
      f = F;
      g = G;
    }
    x25519Field.apm(p.y, p.x, B, A);
    x25519Field.apm(q.y, q.x, d, c);
    x25519Field.mul2(A, C, A);
    x25519Field.mul2(B, D, B);
    x25519Field.mul2(p.t, q.t, C);
    x25519Field.mul2(C, C_d2, C);
    x25519Field.mul2(p.z, q.z, D);
    x25519Field.add(D, D, D);
    x25519Field.apm(B, A, H, E);
    x25519Field.apm(D, C, g, f);
    x25519Field.carry(g);
    x25519Field.mul2(E, F, r.x);
    x25519Field.mul2(G, H, r.y);
    x25519Field.mul2(F, G, r.z);
    x25519Field.mul2(E, H, r.t);
  }

  void pointAddPrecomp(PointPrecomp p, PointAccum r) {
    final A = x25519Field.create;
    final B = x25519Field.create;
    final C = x25519Field.create;
    final E = r.u;
    final F = x25519Field.create;
    final G = x25519Field.create;
    final H = r.v;
    x25519Field.apm(r.y, r.x, B, A);
    x25519Field.mul2(A, p.ymx_h, A);
    x25519Field.mul2(B, p.ypx_h, B);
    x25519Field.mul2(r.u, r.v, C);
    x25519Field.mul2(C, p.xyd, C);
    x25519Field.apm(B, A, H, E);
    x25519Field.apm(r.z, C, G, F);
    x25519Field.carry(G);
    x25519Field.mul2(E, F, r.x);
    x25519Field.mul2(G, H, r.y);
    x25519Field.mul2(F, G, r.z);
  }

  PointExt pointCopyAccum(PointAccum p) {
    final r = PointExt.fromField(x25519Field);
    x25519Field.copy(p.x, 0, r.x, 0);
    x25519Field.copy(p.y, 0, r.y, 0);
    x25519Field.copy(p.z, 0, r.z, 0);
    x25519Field.mul2(p.u, p.v, r.t);
    return r;
  }

  PointExt pointCopyExt(PointExt p) {
    final r = PointExt.fromField(x25519Field);
    x25519Field.copy(p.x, 0, r.x, 0);
    x25519Field.copy(p.y, 0, r.y, 0);
    x25519Field.copy(p.z, 0, r.z, 0);
    x25519Field.copy(p.t, 0, r.t, 0);
    return r;
  }

  void pointDouble(PointAccum r) {
    final A = x25519Field.create;
    final B = x25519Field.create;
    final C = x25519Field.create;
    final E = r.u;
    final F = x25519Field.create;
    final G = x25519Field.create;
    final H = r.v;
    x25519Field.sqr(r.x, A);
    x25519Field.sqr(r.y, B);
    x25519Field.sqr(r.z, C);
    x25519Field.add(C, C, C);
    x25519Field.apm(A, B, H, G);
    x25519Field.add(r.x, r.y, E);
    x25519Field.sqr(E, E);
    x25519Field.sub(H, E, E);
    x25519Field.add(C, G, F);
    x25519Field.carry(F);
    x25519Field.mul2(E, F, r.x);
    x25519Field.mul2(G, H, r.y);
    x25519Field.mul2(F, G, r.z);
  }

  void pointExtendXYAccum(PointAccum p) {
    x25519Field.one(p.z);
    x25519Field.copy(p.x, 0, p.u, 0);
    x25519Field.copy(p.y, 0, p.v, 0);
  }

  void pointExtendXY(PointExt p) {
    x25519Field.one(p.z);
    x25519Field.mul2(p.x, p.y, p.t);
  }

  void pointLookup(int block, int index, PointPrecomp p) {
    var off = block * PRECOMP_POINTS * 3 * X25519Field.SIZE;
    for (int i = 0; i < PRECOMP_POINTS; i++) {
      final mask = ((i ^ index) - 1) >> 31;
      cmov(X25519Field.SIZE, mask, _precompBase, off, p.ypx_h, 0);
      off += X25519Field.SIZE;
      cmov(X25519Field.SIZE, mask, _precompBase, off, p.ymx_h, 0);
      off += X25519Field.SIZE;
      cmov(X25519Field.SIZE, mask, _precompBase, off, p.xyd, 0);
      off += X25519Field.SIZE;
    }
  }

  List<PointExt> pointPrecompVar(PointExt p, int count) {
    final d = PointExt.fromField(x25519Field);
    pointAddVar2(false, p, p, d);
    final List<PointExt> table = [];
    table.add(pointCopyExt(p));
    for (int i = 1; i < count; i++) {
      table.add(PointExt.fromField(x25519Field));
      pointAddVar2(false, table[i - 1], d, table[i]);
    }
    return table;
  }

  void pointSetNeutralAccum(PointAccum p) {
    x25519Field.zero(p.x);
    x25519Field.one(p.y);
    x25519Field.one(p.z);
    x25519Field.zero(p.u);
    x25519Field.one(p.v);
  }

  void pointSetNeutralExt(PointExt p) {
    x25519Field.zero(p.x);
    x25519Field.one(p.y);
    x25519Field.one(p.z);
    x25519Field.zero(p.t);
  }

  void _precompute() {
    if (_precompBase.isNotEmpty) return;
    // Precomputed table for the base point in verification ladder
    final b = PointExt.fromField(x25519Field);
    x25519Field.copy(B_x, 0, b.x, 0);
    x25519Field.copy(B_y, 0, b.y, 0);
    pointExtendXY(b);
    _precompBaseTable = pointPrecompVar(b, 1 << (WNAF_WIDTH_BASE - 2));
    final p = PointAccum.fromField(x25519Field);
    x25519Field.copy(B_x, 0, p.x, 0);
    x25519Field.copy(B_y, 0, p.y, 0);
    pointExtendXYAccum(p);
    _precompBase =
        List.filled(PRECOMP_BLOCKS * PRECOMP_POINTS * 3 * X25519Field.SIZE, 0);
    var off = 0;
    for (int b = 0; b < PRECOMP_BLOCKS; b++) {
      final List<PointExt> ds = [];
      final sum = PointExt.fromField(x25519Field);
      pointSetNeutralExt(sum);
      for (int t = 0; t < PRECOMP_TEETH; t++) {
        final q = pointCopyAccum(p);
        pointAddVar2(true, sum, q, sum);
        pointDouble(p);
        ds.add(pointCopyAccum(p));
        if (b + t != PRECOMP_BLOCKS + PRECOMP_TEETH - 2)
          for (int i = 1; i < PRECOMP_SPACING; i++) pointDouble(p);
      }
      final List<PointExt?> points =
          List.filled(PRECOMP_POINTS, null, growable: false);
      var k = 1;
      points[0] = sum;
      for (int t = 0; t < PRECOMP_TEETH - 1; t++) {
        final size = 1 << t;
        var j = 0;
        while (j < size) {
          points[k] = PointExt.fromField(x25519Field);
          pointAddVar2(false, points[k - size]!, ds[t], points[k]!);
          j += 1;
          k += 1;
        }
      }
      for (int i = 0; i < PRECOMP_POINTS; i++) {
        final q = points[i]!;
        final x = x25519Field.create;
        final y = x25519Field.create;
        x25519Field.add(q.z, q.z, x);
        x25519Field.inv(x, y);
        x25519Field.mul2(q.x, y, x);
        x25519Field.mul2(q.y, y, y);
        final r = PointPrecomp.fromField(x25519Field);
        x25519Field.apm(y, x, r.ypx_h, r.ymx_h);
        x25519Field.mul2(x, y, r.xyd);
        x25519Field.mul2(r.xyd, C_d4, r.xyd);
        x25519Field.normalize(r.ypx_h);
        x25519Field.normalize(r.ymx_h);
        x25519Field.copy(r.ypx_h, 0, _precompBase, off);
        off += X25519Field.SIZE;
        x25519Field.copy(r.ymx_h, 0, _precompBase, off);
        off += X25519Field.SIZE;
        x25519Field.copy(r.xyd, 0, _precompBase, off);
        off += X25519Field.SIZE;
      }
    }
    return;
  }

  void pruneScalar(List<int> n, int nOff, List<int> r) {
    for (int i = 0; i < SCALAR_BYTES; i++) {
      r[i] = n[nOff + i];
    }
    r[0] = (r[0] & 0xf8);
    r[SCALAR_BYTES - 1] = r[SCALAR_BYTES - 1] & 0x7f; // TODO: toByte
    r[SCALAR_BYTES - 1] = r[SCALAR_BYTES - 1] & 0x40; // TODO: toByte
  }

  List<int> reduceScalar(List<int> n) {
    var x00 = Int64(decode32v(n, 0)) & M32L; // x00:32/--
    var x01 = Int64((decode24(n, 4)) << 4) & M32L; // x01:28/--
    var x02 = Int64(decode32v(n, 7)) & M32L; // x02:32/--
    var x03 = Int64((decode24(n, 11)) << 4) & M32L; // x03:28/--
    var x04 = Int64(decode32v(n, 14)) & M32L; // x04:32/--
    var x05 = Int64((decode24(n, 18)) << 4) & M32L; // x05:28/--
    var x06 = Int64(decode32v(n, 21)) & M32L; // x06:32/--
    var x07 = Int64((decode24(n, 25)) << 4) & M32L; // x07:28/--
    var x08 = Int64(decode32v(n, 28)) & M32L; // x08:32/--
    var x09 = Int64((decode24(n, 32)) << 4) & M32L; // x09:28/--
    var x10 = Int64(decode32v(n, 35)) & M32L; // x10:32/--
    var x11 = Int64((decode24(n, 39)) << 4) & M32L; // x11:28/--
    var x12 = Int64(decode32v(n, 42)) & M32L; // x12:32/--
    var x13 = Int64((decode24(n, 46)) << 4) & M32L; // x13:28/--
    var x14 = Int64(decode32v(n, 49)) & M32L; // x14:32/--
    var x15 = Int64((decode24(n, 53)) << 4) & M32L; // x15:28/--
    var x16 = Int64(decode32v(n, 56)) & M32L; // x16:32/--
    var x17 = Int64((decode24(n, 60)) << 4) & M32L; // x17:28/--
    final x18 = n[63] & 0xff; // x18:08/-- TODO?
    var t = Int64(0);
    x09 -= x18 * L0; // x09:34/28
    x10 -= x18 * L1; // x10:33/30
    x11 -= x18 * L2; // x11:35/28
    x12 -= x18 * L3; // x12:32/31
    x13 -= x18 * L4; // x13:28/21
    x17 += (x16 >> 28);
    x16 &= M28L; // x17:28/--, x16:28/--
    x08 -= x17 * L0; // x08:54/32
    x09 -= x17 * L1; // x09:52/51
    x10 -= x17 * L2; // x10:55/34
    x11 -= x17 * L3; // x11:51/36
    x12 -= x17 * L4; // x12:41/--
    x07 -= x16 * L0; // x07:54/28
    x08 -= x16 * L1; // x08:54/53
    x09 -= x16 * L2; // x09:55/53
    x10 -= x16 * L3; // x10:55/52
    x11 -= x16 * L4; // x11:51/41
    x15 += (x14 >> 28);
    x14 &= M28L; // x15:28/--, x14:28/--
    x06 -= x15 * L0; // x06:54/32
    x07 -= x15 * L1; // x07:54/53
    x08 -= x15 * L2; // x08:56/--
    x09 -= x15 * L3; // x09:55/54
    x10 -= x15 * L4; // x10:55/53
    x05 -= x14 * L0; // x05:54/28
    x06 -= x14 * L1; // x06:54/53
    x07 -= x14 * L2; // x07:56/--
    x08 -= x14 * L3; // x08:56/51
    x09 -= x14 * L4; // x09:56/--
    x13 += (x12 >> 28);
    x12 &= M28L; // x13:28/22, x12:28/--
    x04 -= x13 * L0; // x04:54/49
    x05 -= x13 * L1; // x05:54/53
    x06 -= x13 * L2; // x06:56/--
    x07 -= x13 * L3; // x07:56/52
    x08 -= x13 * L4; // x08:56/52
    x12 += (x11 >> 28);
    x11 &= M28L; // x12:28/24, x11:28/--
    x03 -= x12 * L0; // x03:54/49
    x04 -= x12 * L1; // x04:54/51
    x05 -= x12 * L2; // x05:56/--
    x06 -= x12 * L3; // x06:56/52
    x07 -= x12 * L4; // x07:56/53
    x11 += (x10 >> 28);
    x10 &= M28L; // x11:29/--, x10:28/--
    x02 -= x11 * L0; // x02:55/32
    x03 -= x11 * L1; // x03:55/--
    x04 -= x11 * L2; // x04:56/55
    x05 -= x11 * L3; // x05:56/52
    x06 -= x11 * L4; // x06:56/53
    x10 += (x09 >> 28);
    x09 &= M28L; // x10:29/--, x09:28/--
    x01 -= x10 * L0; // x01:55/28
    x02 -= x10 * L1; // x02:55/54
    x03 -= x10 * L2; // x03:56/55
    x04 -= x10 * L3; // x04:57/--
    x05 -= x10 * L4; // x05:56/53
    x08 += (x07 >> 28);
    x07 &= M28L; // x08:56/53, x07:28/--
    x09 += (x08 >> 28);
    x08 &= M28L; // x09:29/25, x08:28/--
    t = x08 >> 27; // TODO: >>>
    x09 += t; // x09:29/26
    x00 -= x09 * L0; // x00:55/53
    x01 -= x09 * L1; // x01:55/54
    x02 -= x09 * L2; // x02:57/--
    x03 -= x09 * L3; // x03:57/--
    x04 -= x09 * L4; // x04:57/42
    x01 += (x00 >> 28);
    x00 &= M28L;
    x02 += (x01 >> 28);
    x01 &= M28L;
    x03 += (x02 >> 28);
    x02 &= M28L;
    x04 += (x03 >> 28);
    x03 &= M28L;
    x05 += (x04 >> 28);
    x04 &= M28L;
    x06 += (x05 >> 28);
    x05 &= M28L;
    x07 += (x06 >> 28);
    x06 &= M28L;
    x08 += (x07 >> 28);
    x07 &= M28L;
    x09 = x08 >> 28;
    x08 &= M28L;
    x09 -= t;
    x00 += x09 & L0;
    x01 += x09 & L1;
    x02 += x09 & L2;
    x03 += x09 & L3;
    x04 += x09 & L4;
    x01 += (x00 >> 28);
    x00 &= M28L;
    x02 += (x01 >> 28);
    x01 &= M28L;
    x03 += (x02 >> 28);
    x02 &= M28L;
    x04 += (x03 >> 28);
    x03 &= M28L;
    x05 += (x04 >> 28);
    x04 &= M28L;
    x06 += (x05 >> 28);
    x05 &= M28L;
    x07 += (x06 >> 28);
    x06 &= M28L;
    x08 += (x07 >> 28);
    x07 &= M28L;
    final r = List.filled(SCALAR_BYTES, 0);
    encode56(x00 | (x01 << 28), r, 0);
    encode56(x02 | (x03 << 28), r, 7);
    encode56(x04 | (x05 << 28), r, 14);
    encode56(x06 | (x07 << 28), r, 21);
    encode32(x08.toInt32().toInt(), r, 28);
    return r;
  }

  void scalarMultBase(List<int> k, PointAccum r) {
    pointSetNeutralAccum(r);
    final n = List.filled(SCALAR_INTS, 0);
    decodeScalar(k, 0, n);
    // Recode the scalar into signed-digit form, then group comb bits in each block
    cadd(SCALAR_INTS, ~n[0] & 1, n, L, n);
    shiftDownBit(SCALAR_INTS, n, 1);
    for (int i = 0; i < SCALAR_INTS; i++) n[i] = shuffle2(n[i]);
    final p = PointPrecomp.fromField(x25519Field);
    var cOff = (PRECOMP_SPACING - 1) * PRECOMP_TEETH;
    while (true) {
      for (int b = 0; b < PRECOMP_BLOCKS; b++) {
        final w = n[b] >>> cOff;
        final sign = (w >>> (PRECOMP_TEETH - 1)) & 1;
        final abs = (w ^ -sign) & PRECOMP_MASK;
        pointLookup(b, abs, p);
        x25519Field.cswap(sign, p.ypx_h, p.ymx_h);
        x25519Field.cnegate(sign, p.xyd);
        pointAddPrecomp(p, r);
      }
      cOff -= PRECOMP_TEETH;
      if (cOff < 0) break;
      pointDouble(r);
    }
  }

  List<int> createScalarMultBaseEncoded(List<int> s) {
    final r = List.filled(SCALAR_BYTES, 0x00);
    scalarMultBaseEncoded(s, r, 0);
    return r;
  }

  void scalarMultBaseEncoded(List<int> k, List<int> r, int rOff) {
    final p = PointAccum.fromField(x25519Field);
    scalarMultBase(k, p);
    encodePoint(p, r, rOff);
  }

  void scalarMultStraussVar(
      List<int> nb, List<int> np, PointExt p, PointAccum r) {
    final width = 5;
    final ws_b = getWNAF(nb, WNAF_WIDTH_BASE);
    final ws_p = getWNAF(np, width);
    final tp = pointPrecompVar(p, 1 << (width - 2));
    pointSetNeutralAccum(r);
    var bit = 255;
    while (bit > 0 && (ws_b[bit] | ws_p[bit]) == 0) bit -= 1;
    while (true) {
      final wb = ws_b[bit];
      if (wb != 0) {
        final sign = wb >> 31;
        final index = (wb ^ sign) >>> 1;
        pointAddVar1(sign != 0, _precompBaseTable[index], r);
      }
      final wp = ws_p[bit];
      if (wp != 0) {
        final sign = wp >> 31;
        final index = (wp ^ sign) >>> 1;
        pointAddVar1(sign != 0, tp[index], r);
      }
      if (--bit < 0) break;
      pointDouble(r);
    }
  }

  static const POINT_BYTES = 32;
  static const SCALAR_INTS = 8;
  static const SCALAR_BYTES = SCALAR_INTS * 4;
  static const PREHASH_SIZE = 64;
  static const PUBLIC_KEY_SIZE = POINT_BYTES;
  static const SECRET_KEY_SIZE = 32;
  static const SIGNATURE_SIZE = POINT_BYTES + SCALAR_BYTES;
  static const DOM2_PREFIX = "SigEd25519 no Ed25519 collisions";
  static final M28L = Int64(0x0fffffff);
  static final M32L = Int64(0xffffffff);
  static const P = [
    0xffffffed,
    0xffffffff,
    0xffffffff,
    0xffffffff,
    0xffffffff,
    0xffffffff,
    0xffffffff,
    0x7fffffff
  ];
  static const L = [
    0x5cf5d3ed,
    0x5812631a,
    0xa2f79cd6,
    0x14def9de,
    0x00000000,
    0x00000000,
    0x00000000,
    0x10000000
  ];
  static const L0 = 0xfcf5d3ed;
  static const L1 = 0x012631a6;
  static const L2 = 0x079cd658;
  static const L3 = 0xff9dea2f;
  static const L4 = 0x000014df;

  static const B_x = [
    0x0325d51a,
    0x018b5823,
    0x007b2c95,
    0x0304a92d,
    0x00d2598e,
    0x01d6dc5c,
    0x01388c7f,
    0x013fec0a,
    0x029e6b72,
    0x0042d26d
  ];

  static const B_y = [
    0x02666658,
    0x01999999,
    0x00666666,
    0x03333333,
    0x00cccccc,
    0x02666666,
    0x01999999,
    0x00666666,
    0x03333333,
    0x00cccccc
  ];

  static const C_d = [
    0x035978a3,
    0x02d37284,
    0x018ab75e,
    0x026a0a0e,
    0x0000e014,
    0x0379e898,
    0x01d01e5d,
    0x01e738cc,
    0x03715b7f,
    0x00a406d9
  ];

  static const C_d2 = [
    0x02b2f159,
    0x01a6e509,
    0x01156ebd,
    0x00d4141d,
    0x0001c029,
    0x02f3d130,
    0x03a03cbb,
    0x01ce7198,
    0x02e2b6ff,
    0x00480db3
  ];

  static const C_d4 = [
    0x0165e2b2,
    0x034dca13,
    0x002add7a,
    0x01a8283b,
    0x00038052,
    0x01e7a260,
    0x03407977,
    0x019ce331,
    0x01c56dff,
    0x00901b67
  ];
  static const WNAF_WIDTH_BASE = 7;
  static const PRECOMP_BLOCKS = 8;
  static const PRECOMP_TEETH = 4;
  static const PRECOMP_SPACING = 8;
  static const PRECOMP_POINTS = 1 << (PRECOMP_TEETH - 1);
  static const PRECOMP_MASK = PRECOMP_POINTS - 1;
  static const M = 0xffffffff;
}

final ec = EC();

class PointAccum {
  final List<int> x;
  final List<int> y;
  final List<int> z;
  final List<int> u;
  final List<int> v;

  PointAccum(this.x, this.y, this.z, this.u, this.v);

  factory PointAccum.fromField(X25519Field x25519Field) => PointAccum(
        x25519Field.create,
        x25519Field.create,
        x25519Field.create,
        x25519Field.create,
        x25519Field.create,
      );
}

class PointExt {
  final List<int> x;
  final List<int> y;
  final List<int> z;
  final List<int> t;

  PointExt(this.x, this.y, this.z, this.t);

  factory PointExt.fromField(X25519Field x25519Field) => PointExt(
        x25519Field.create,
        x25519Field.create,
        x25519Field.create,
        x25519Field.create,
      );
}

class PointPrecomp {
  final List<int> ypx_h;
  final List<int> ymx_h;
  final List<int> xyd;

  PointPrecomp(this.ypx_h, this.ymx_h, this.xyd);

  factory PointPrecomp.fromField(X25519Field x25519Field) => PointPrecomp(
        x25519Field.create,
        x25519Field.create,
        x25519Field.create,
      );
}
