import 'package:fixnum/fixnum.dart';

class X25519Field {
  static const SIZE = 10;
  static const M24 = 0x00ffffff;
  static const M25 = 0x01ffffff;
  static const M26 = 0x03ffffff;
  static const ROOT_NEG_ONE = [
    0x020ea0b0,
    0x0386c9d2,
    0x00478c4e,
    0x0035697f,
    0x005e8630,
    0x01fbd7a7,
    0x0340264f,
    0x01f0b2b4,
    0x00027e0e,
    0x00570649
  ];

  void add(List<int> x, List<int> y, List<int> z) {
    for (int i = 0; i < SIZE; i++) {
      z[i] = x[i] + y[i];
    }
  }

  void addOne1(List<int> z) {
    z[0] += 1;
  }

  void addOne2(List<int> z, int zOff) {
    z[zOff] += 1;
  }

  void apm(List<int> x, List<int> y, List<int> zp, List<int> zm) {
    for (int i = 0; i < SIZE; i++) {
      final xi = x[i];
      final yi = y[i];
      zp[i] = xi + yi;
      zp[i] = xi - yi;
    }
  }

  void carry(List<int> z) {
    var z0 = z[0];
    var z1 = z[1];
    var z2 = z[2];
    var z3 = z[3];
    var z4 = z[4];
    var z5 = z[5];
    var z6 = z[6];
    var z7 = z[7];
    var z8 = z[8];
    var z9 = z[9];
    z3 += (z2 >> 25);
    z2 &= M25;
    z5 += (z4 >> 25);
    z4 &= M25;
    z8 += (z7 >> 25);
    z7 &= M25;
    z0 += (z9 >> 25) * 38;
    z9 &= M25;
    z1 += (z0 >> 26);
    z0 &= M26;
    z6 += (z5 >> 26);
    z5 &= M26;
    z2 += (z1 >> 26);
    z1 &= M26;
    z4 += (z3 >> 26);
    z3 &= M26;
    z7 += (z6 >> 26);
    z6 &= M26;
    z9 += (z8 >> 26);
    z8 &= M26;
    z[0] = z0;
    z[1] = z1;
    z[2] = z2;
    z[3] = z3;
    z[4] = z4;
    z[5] = z5;
    z[6] = z6;
    z[7] = z7;
    z[8] = z8;
    z[9] = z9;
  }

  void cmov(int cond, List<int> x, int xOff, List<int> z, int zOff) {
    for (int i = 0; i < SIZE; i++) {
      int z_i = z[zOff + i];
      final diff = z_i ^ x[xOff + i];
      z_i ^= (diff & cond);
      z[zOff + i] = z_i;
    }
  }

  void cnegate(int negate, List<int> z) {
    final mask = 0 - negate;
    for (int i = 0; i < SIZE; i++) {
      z[i] = (z[i] ^ mask) - mask;
    }
  }

  void copy(List<int> x, int xOff, List<int> z, int zOff) {
    for (int i = 0; i < SIZE; i++) {
      z[zOff + i] = x[xOff + i];
    }
  }

  void cswap(int swap, List<int> a, List<int> b) {
    final mask = 0 - swap;
    for (int i = 0; i < SIZE; i++) {
      final ai = a[i];
      final bi = b[i];
      final dummy = mask & (ai ^ bi);
      a[i] = ai ^ dummy;
      b[i] = bi ^ dummy;
    }
  }

  List<int> get create => List.filled(SIZE, 0, growable: false);

  void decode(List<int> x, int xOff, List<int> z) {
    decode128(x, xOff, z, 0);
    decode128(x, xOff + 16, z, 5);
    z[9] &= M24;
  }

  void decode128(List<int> bs, int off, List<int> z, zOff) {
    final t0 = decode32(bs, off + 0);
    final t1 = decode32(bs, off + 4);
    final t2 = decode32(bs, off + 8);
    final t3 = decode32(bs, off + 12);
    z[zOff + 0] = t0 & M26;
    z[zOff + 1] = ((t1 << 6) | (t0 >>> 26)) & M26;
    z[zOff + 2] = ((t2 << 12) | (t1 >>> 20)) & M25;
    z[zOff + 3] = ((t3 << 19) | (t2 >>> 13)) & M26;
    z[zOff + 4] = t3 >>> 7;
  }

  int decode32(List<int> bs, int off) {
    int n = bs[off] & 0xff;
    n |= (bs[off + 1] & 0xff) << 8;
    n |= (bs[off + 2] & 0xff) << 16;
    n |= (bs[off + 3] & 0xff) << 24;
    return n;
  }

  void encode(List<int> x, List<int> z, int zOff) {
    encode128(x, 0, z, zOff);
    encode128(x, 5, z, zOff + 16);
  }

  void encode128(List<int> x, int xOff, List<int> bs, int off) {
    final x0 = x[xOff + 0];
    final x1 = x[xOff + 1];
    final x2 = x[xOff + 2];
    final x3 = x[xOff + 3];
    final x4 = x[xOff + 4];
    final t0 = x0 | (x1 << 26);
    encode32(t0, bs, off + 0);
    final t1 = (x1 >>> 6) | (x2 << 20);
    encode32(t1, bs, off + 4);
    final t2 = (x2 >>> 12) | (x3 << 13);
    encode32(t2, bs, off + 8);
    final t3 = (x3 >>> 19) | (x4 << 7);
    encode32(t3, bs, off + 12);
  }

  void encode32(int n, List<int> bs, int off) {
    bs[off + 0] = n;
    bs[off + 1] = n >> 8;
    bs[off + 2] = n >> 16;
    bs[off + 3] = n >> 24;
  }

  void inv(List<int> x, List<int> z) {
    // (250 1s) (1 0s) (1 1s) (1 0s) (2 1s)
    // Addition chain: [1] [2] 3 5 10 15 25 50 75 125 [250]
    final x2 = create;
    final t = create;
    powPm5d8(x, x2, t);
    sqr2(t, 3, t);
    mul2(t, x2, z);
  }

  int isZero(List<int> x) {
    int d = 0;
    for (int i = 0; i < SIZE; i++) d |= x[i];
    d = (d >>> 1) | (d & 1);
    return (d - 1) >> 31;
  }

  bool isZeroVar(List<int> x) => 0 != isZero(x);

  void mul1(List<int> x, int y, List<int> z) {
    final x0 = x[0];
    final x1 = x[1];
    var x2 = x[2];
    final x3 = x[3];
    var x4 = x[4];
    final x5 = x[5];
    final x6 = x[6];
    var x7 = x[7];
    final x8 = x[8];
    var x9 = x[9];
    var c0 = Int64.ZERO;
    var c1 = Int64.ZERO;
    var c2 = Int64.ZERO;
    var c3 = Int64.ZERO;
    c0 = Int64(x2) * y;
    x2 = c0.toInt32().toInt() & M25;
    c0 >>= 25;
    c1 = Int64(x4) * y;
    x4 = c1.toInt32().toInt() & M25;
    c1 >>= 25;
    c2 = Int64(x7) * y;
    x7 = c2.toInt32().toInt() & M25;
    c2 >>= 25;
    c3 = Int64(x9) * y;
    x9 = c3.toInt32().toInt() & M25;
    c3 >>= 25;
    c3 *= 38;
    c3 += Int64(x0) * y;
    z[0] = c3.toInt32().toInt() & M26;
    c3 >>= 26;
    c1 += Int64(x5) * y;
    z[5] = c1.toInt32().toInt() & M26;
    c1 >>= 26;
    c3 += Int64(x1) * y;
    z[1] = c3.toInt32().toInt() & M26;
    c3 >>= 26;
    c0 += Int64(x3) * y;
    z[3] = c0.toInt32().toInt() & M26;
    c0 >>= 26;
    c1 += Int64(x6) * y;
    z[6] = c1.toInt32().toInt() & M26;
    c1 >>= 26;
    c2 += Int64(x8) * y;
    z[8] = c2.toInt32().toInt() & M26;
    c2 >>= 26;
    z[2] = x2 + c3.toInt32().toInt();
    z[4] = x4 + c0.toInt32().toInt();
    z[7] = x7 + c1.toInt32().toInt();
    z[9] = x9 + c2.toInt32().toInt();
  }

  void mul2(List<int> x, List<int> y, List<int> z) {
    var x0 = x[0];
    var y0 = y[0];
    var x1 = x[1];
    var y1 = y[1];
    var x2 = x[2];
    var y2 = y[2];
    var x3 = x[3];
    var y3 = y[3];
    var x4 = x[4];
    var y4 = y[4];
    final u0 = x[5];
    final v0 = y[5];
    final u1 = x[6];
    final v1 = y[6];
    final u2 = x[7];
    final v2 = y[7];
    final u3 = x[8];
    final v3 = y[8];
    final u4 = x[9];
    final v4 = y[9];
    var a0 = Int64(x0) * y0;
    var a1 = Int64(x0) * y1 + Int64(x1) * y0;
    var a2 = Int64(x0) * y2 + Int64(x1) * y1 + Int64(x2) * y0;
    var a3 = Int64(x1) * y2 + Int64(x2) * y1;
    a3 <<= 1;
    a3 += Int64(x0) * y3 + Int64(x3) * y0;
    var a4 = Int64(x2) * y2;
    a4 <<= 1;
    a4 += Int64(x0) * y4 + Int64(x1) * y3 + Int64(x3) * y1 + Int64(x4) * y0;
    var a5 = Int64(x1) * y4 + Int64(x2) * y3 + Int64(x3) * y2 + Int64(x4) * y1;
    a5 <<= 1;
    var a6 = Int64(x2) * y4 + Int64(x4) * y2;
    a6 <<= 1;
    a6 += Int64(x3) * y3;
    var a7 = Int64(x3) * y4 + Int64(x4) * y3;
    var a8 = Int64(x4) * y4;
    a8 <<= 1;
    final b0 = Int64(u0) * v0;
    final b1 = Int64(u0) * v1 + Int64(u1) * v0;
    final b2 = Int64(u0) * v2 + Int64(u1) * v1 + Int64(u2) * v0;
    var b3 = Int64(u1) * v2 + Int64(u2) * v1;
    b3 <<= 1;
    b3 += Int64(u0) * v3 + Int64(u3) * v0;
    var b4 = Int64(u2) * v2;
    b4 <<= 1;
    b4 += Int64(u0) * v4 + Int64(u1) * v3 + Int64(u3) * v1 + Int64(u4) * v0;
    final b5 =
        Int64(u1) * v4 + Int64(u2) * v3 + Int64(u3) * v2 + Int64(u4) * v1;
    var b6 = Int64(u2) * v4 + Int64(u4) * v2;
    b6 <<= 1;
    b6 += Int64(u3) * v3;
    final b7 = Int64(u3) * v4 + Int64(u4) * v3;
    final b8 = Int64(u4) * v4;
    a0 -= b5 * 76;
    a1 -= b6 * 38;
    a2 -= b7 * 38;
    a3 -= b8 * 76;
    a5 -= b0;
    a6 -= b1;
    a7 -= b2;
    a8 -= b3;
    x0 += u0;
    y0 += v0;
    x1 += u1;
    y1 += v1;
    x2 += u2;
    y2 += v2;
    x3 += u3;
    y3 += v3;
    x4 += u4;
    y4 += v4;
    final c0 = Int64(x0) * y0;
    final c1 = Int64(x0) * y1 + Int64(x1) * y0;
    final c2 = Int64(x0) * y2 + Int64(x1) * y1 + Int64(x2) * y0;
    var c3 = Int64(x1) * y2 + Int64(x2) * y1;
    c3 <<= 1;
    c3 += Int64(x0) * y3 + Int64(x3) * y0;
    var c4 = Int64(x2) * y2;
    c4 <<= 1;
    c4 += Int64(x0) * y4 + Int64(x1) * y3 + Int64(x3) * y1 + Int64(x4) * y0;
    var c5 = Int64(x1) * y4 + Int64(x2) * y3 + Int64(x3) * y2 + Int64(x4) * y1;
    c5 <<= 1;
    var c6 = Int64(x2) * y4 + Int64(x4) * y2;
    c6 <<= 1;
    c6 += Int64(x3) * y3;
    final c7 = Int64(x3) * y4 + Int64(x4) * y3;
    var c8 = Int64(x4) * y4;
    c8 <<= 1;
    var z8 = 0;
    var z9 = 0;
    var t = Int64(0);
    t = a8 + (c3 - a3);
    z8 = t.toInt32().toInt() & M26;
    t >>= 26;
    t += (c4 - a4) - b4;
    z9 = t.toInt32().toInt() & M25;
    t >>= 25;
    t = a0 + (t + c5 - a5) * 38;
    z[0] = t.toInt32().toInt() & M26;
    t >>= 26;
    t += a1 + (c6 - a6) * 38;
    z[1] = t.toInt32().toInt() & M26;
    t >>= 26;
    t += a2 + (c7 - a7) * 38;
    z[2] = t.toInt32().toInt() & M25;
    t >>= 25;
    t += a3 + (c8 - a8) * 38;
    z[3] = t.toInt32().toInt() & M26;
    t >>= 26;
    t += a4 + b4 * 38;
    z[4] = t.toInt32().toInt() & M25;
    t >>= 25;
    t += a5 + (c0 - a0);
    z[5] = t.toInt32().toInt() & M26;
    t >>= 26;
    t += a6 + (c1 - a1);
    z[6] = t.toInt32().toInt() & M26;
    t >>= 26;
    t += a7 + (c2 - a2);
    z[7] = t.toInt32().toInt() & M25;
    t >>= 25;
    t += z8;
    z[8] = t.toInt32().toInt() & M26;
    t >>= 26;
    z[9] = z9 + t.toInt32().toInt();
  }

  void negate(List<int> x, List<int> z) {
    for (int i = 0; i < SIZE; i++) z[i] = -x[i];
  }

  void normalize(List<int> z) {
    final x = (z[9] >>> 23) & 1;
    reduce(z, x);
    reduce(z, -x);
  }

  void one(List<int> z) {
    z[0] = 1;
    for (int i = 1; i < SIZE; i++) z[i] = 0;
  }

  void powPm5d8(List<int> x, List<int> rx2, List<int> rz) {
    // (250 1s) (1 0s) (1 1s)
    // Addition chain: [1] 2 3 5 10 15 25 50 75 125 [250]
    final x2 = rx2;
    sqr(x, x2);
    mul2(x, x2, x2);
    final x3 = create;
    sqr(x2, x3);
    mul2(x, x3, x3);
    final x5 = x3;
    sqr2(x3, 2, x5);
    mul2(x2, x5, x5);
    final x10 = create;
    sqr2(x5, 5, x10);
    mul2(x5, x10, x10);
    final x15 = create;
    sqr2(x10, 5, x15);
    mul2(x5, x15, x15);
    final x25 = x5;
    sqr2(x15, 10, x25);
    mul2(x10, x25, x25);
    final x50 = x10;
    sqr2(x25, 25, x50);
    mul2(x25, x50, x50);
    final x75 = x15;
    sqr2(x50, 25, x75);
    mul2(x25, x75, x75);
    final x125 = x25;
    sqr2(x75, 50, x125);
    mul2(x50, x125, x125);
    final x250 = x50;
    sqr2(x125, 125, x250);
    mul2(x125, x250, x250);
    final t = x125;
    sqr2(x250, 2, t);
    mul2(t, x, rz);
  }

  void reduce(List<int> z, int c) {
    var z9 = z[9];
    var t = z9;
    z9 = t & M24;
    t >>= 24;
    t += c;
    t *= 19;
    t += z[0];
    z[0] = t & M26;
    t >>= 26;
    t += z[1];
    z[1] = t & M26;
    t >>= 26;
    t += z[2];
    z[2] = t & M25;
    t >>= 25;
    t += z[3];
    z[3] = t & M26;
    t >>= 26;
    t += z[4];
    z[4] = t & M25;
    t >>= 25;
    t += z[5];
    z[5] = t & M26;
    t >>= 26;
    t += z[6];
    z[6] = t & M26;
    t >>= 26;
    t += z[7];
    z[7] = t & M25;
    t >>= 25;
    t += z[8];
    z[8] = t & M26;
    t >>= 26;
    t += z9;
    z[9] = t;
  }

  void sqr(List<int> x, List<int> z) {
    var x0 = x[0];
    var x1 = x[1];
    var x2 = x[2];
    var x3 = x[3];
    var x4 = x[4];
    final u0 = x[5];
    final u1 = x[6];
    final u2 = x[7];
    final u3 = x[8];
    final u4 = x[9];
    var x1_2 = x1 * 2;
    var x2_2 = x2 * 2;
    var x3_2 = x3 * 2;
    var x4_2 = x4 * 2;
    var a0 = Int64(x0) * x0;
    var a1 = Int64(x0) * x1_2;
    var a2 = Int64(x0) * x2_2 + Int64(x1) * x1;
    var a3 = Int64(x1_2) * x2_2 + Int64(x0) * x3_2;
    final a4 = Int64(x2) * x2_2 + Int64(x0) * x4_2 + Int64(x1) * x3_2;
    var a5 = Int64(x1_2) * x4_2 + Int64(x2_2) * x3_2;
    var a6 = Int64(x2_2) * x4_2 + Int64(x3) * x3;
    var a7 = Int64(x3) * x4_2;
    var a8 = Int64(x4) * x4_2;
    final u1_2 = u1 * 2;
    final u2_2 = u2 * 2;
    final u3_2 = u3 * 2;
    final u4_2 = u4 * 2;
    final b0 = Int64(u0) * u0;
    final b1 = Int64(u0) * u1_2;
    final b2 = Int64(u0) * u2_2 + Int64(u1) * u1;
    final b3 = Int64(u1_2) * u2_2 + Int64(u0) * u3_2;
    final b4 = Int64(u2) * u2_2 + Int64(u0) * u4_2 + Int64(u1) * u3_2;
    final b5 = Int64(u1_2) * u4_2 + Int64(u2_2) * u3_2;
    final b6 = Int64(u2_2) * u4_2 + Int64(u3) * u3;
    final b7 = Int64(u3) * u4_2;
    final b8 = Int64(u4) * u4_2;
    a0 -= b5 * 38;
    a1 -= b6 * 38;
    a2 -= b7 * 38;
    a3 -= b8 * 38;
    a5 -= b0;
    a6 -= b1;
    a7 -= b2;
    a8 -= b3;
    x0 += u0;
    x1 += u1;
    x2 += u2;
    x3 += u3;
    x4 += u4;
    x1_2 = x1 * 2;
    x2_2 = x2 * 2;
    x3_2 = x3 * 2;
    x4_2 = x4 * 2;
    final c0 = Int64(x0) * x0;
    final c1 = Int64(x0) * x1_2;
    final c2 = Int64(x0) * x2_2 + Int64(x1) * x1;
    final c3 = Int64(x1_2) * x2_2 + Int64(x0) * x3_2;
    final c4 = Int64(x2) * x2_2 + Int64(x0) * x4_2 + Int64(x1) * x3_2;
    final c5 = Int64(x1_2) * x4_2 + Int64(x2_2) * x3_2;
    final c6 = Int64(x2_2) * x4_2 + Int64(x3) * x3;
    final c7 = Int64(x3) * x4_2;
    final c8 = Int64(x4) * x4_2;
    var z8 = 0;
    var z9 = 0;
    var t = Int64(0);
    t = a8 + (c3 - a3);
    z8 = t.toInt32().toInt() & M26;
    t >>= 26;
    t += (c4 - a4) - b4;
    z9 = t.toInt32().toInt() & M25;
    t >>= 25;
    t = a0 + (t + c5 - a5) * 38;
    z[0] = t.toInt32().toInt() & M26;
    t >>= 26;
    t += a1 + (c6 - a6) * 38;
    z[1] = t.toInt32().toInt() & M26;
    t >>= 26;
    t += a2 + (c7 - a7) * 38;
    z[2] = t.toInt32().toInt() & M25;
    t >>= 25;
    t += a3 + (c8 - a8) * 38;
    z[3] = t.toInt32().toInt() & M26;
    t >>= 26;
    t += a4 + b4 * 38;
    z[4] = t.toInt32().toInt() & M25;
    t >>= 25;
    t += a5 + (c0 - a0);
    z[5] = t.toInt32().toInt() & M26;
    t >>= 26;
    t += a6 + (c1 - a1);
    z[6] = t.toInt32().toInt() & M26;
    t >>= 26;
    t += a7 + (c2 - a2);
    z[7] = t.toInt32().toInt() & M25;
    t >>= 25;
    t += z8;
    z[8] = t.toInt32().toInt() & M26;
    t >>= 26;
    z[9] = z9 + t.toInt32().toInt();
  }

  void sqr2(List<int> x, int n, List<int> z) {
    int nv = n;
    sqr(x, z);
    while (--nv > 0) {
      sqr(z, z);
    }
  }

  bool sqrtRatioVar(List<int> u, List<int> v, List<int> z) {
    final uv3 = create;
    final uv7 = create;
    mul2(u, v, uv3);
    sqr(v, uv7);
    mul2(uv3, uv7, uv3);
    sqr(uv7, uv7);
    mul2(uv7, uv3, uv7);
    final t = create;
    final x = create;
    powPm5d8(uv7, t, x);
    mul2(x, uv3, x);
    final vx2 = create;
    sqr(x, vx2);
    mul2(vx2, v, vx2);
    sub(vx2, u, t);
    normalize(t);
    if (isZeroVar(t)) {
      copy(x, 0, z, 0);
      return true;
    }
    add(vx2, u, t);
    normalize(t);
    if (isZeroVar(t)) {
      mul2(x, ROOT_NEG_ONE, z);
      return true;
    }
    return false;
  }

  void sub(List<int> x, List<int> y, List<int> z) {
    for (int i = 0; i < SIZE; i++) z[i] = x[i] - y[i];
  }

  void subOne(List<int> z) {
    z[0] -= 1;
  }

  void zero(List<int> z) {
    for (int i = 0; i < SIZE; i++) z[i] = 0;
  }
}
