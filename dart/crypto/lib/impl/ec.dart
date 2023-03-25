import 'dart:math';

abstract class EC {
  final _x25519Field = X25519Field();
  List<PointExt> _precompBaseTable = [];
  List<int> _precompBase = [];

  int _mulAddTo256(List<int> x, List<int> y, List<int> zz) {
    final y_0 = y[0] & _M;
    final y_1 = y[1] & _M;
    final y_2 = y[2] & _M;
    final y_3 = y[3] & _M;
    final y_4 = y[4] & _M;
    final y_5 = y[5] & _M;
    final y_6 = y[6] & _M;
    final y_7 = y[7] & _M;
    int zc = 0;
    for (int i = 0; i < 8; i++) {
      int c = 0;
      final x_i = x[i] & _M;
      c += x_i * y_0 + (zz[i + 0] & _M);
      zz[i + 0] = c;
      c >>>= 32;
      c += x_i * y_1 + (zz[i + 1] & _M);
      zz[i + 1] = c;
      c >>>= 32;
      c += x_i * y_2 + (zz[i + 2] & _M);
      zz[i + 2] = c;
      c >>>= 32;
      c += x_i * y_3 + (zz[i + 3] & _M);
      zz[i + 3] = c;
      c >>>= 32;
      c += x_i * y_4 + (zz[i + 4] & _M);
      zz[i + 4] = c;
      c >>>= 32;
      c += x_i * y_5 + (zz[i + 5] & _M);
      zz[i + 5] = c;
      c >>>= 32;
      c += x_i * y_6 + (zz[i + 6] & _M);
      zz[i + 6] = c;
      c >>>= 32;
      c += x_i * y_7 + (zz[i + 7] & _M);
      zz[i + 7] = c;
      c >>>= 32;
      zc += c + zz[i + 8] & _M;
      zz[i + 8] = zc;
      zc >>>= 32;
    }
    return zc;
  }

  bool _gte2519(List<int> x, List<int> y) {
    for (int i = 7; i >= 0; i--) {
      final x_i = x[i] ^ 0x80000000;
      final y_i = y[i] ^ 0x80000000;
      if (x_i < y_i) return false;
      if (x_i > y_i) return true;
    }
    return true;
  }

  void _cmov(int len, int mask, List<int> x, int xOff, List<int> z, int zOff) {
    int maskv = mask;
    maskv = -(maskv & 1);
    for (int i = 0; i < len; i++) {
      int z_i = z[zOff + i];
      final diff = z_i ^ x[xOff + i];
      z_i ^= (diff & maskv);
      z[zOff + i] = z_i;
    }
  }

  int _cadd(int len, int mask, List<int> x, List<int> y, List<int> z) {
    final m = -(mask & 1) & _M;
    int c = 0;
    for (int i = 0; i < len; i++) {
      c += (x[i] & _M) + (y[i] & m);
      z[i] = c;
      c >>>= 32;
    }
    return c;
  }

  int _shiftDownBit(int len, List<int> z, int c) {
    int i = len - 1;
    int cv = c;
    while (i >= 0) {
      final next = z[i];
      z[i] = (next >>> 1) | (cv << 32);
      cv = next;
      i -= 1;
    }
    return cv << 31;
  }

  int _shuffle2(int x) {
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

  bool _areAllZeroes(List<int> buf, int off, int len) {
    int bits = 0;
    for (int i = 0; i < len; i++) bits |= buf[off + i];
    return bits == 0;
  }

  List<int> _calculateS(List<int> r, List<int> k, List<int> s) {
    final List<int> t = List.filled(_SCALAR_INTS * 2, 0);
    _decodeScalar(r, 0, t);
    final List<int> u = List.filled(_SCALAR_INTS, 0);
    _decodeScalar(k, 0, u);
    final List<int> v = List.filled(_SCALAR_INTS, 0);
    _decodeScalar(s, 0, v);
    _mulAddTo256(u, v, t);
    final List<int> result = List.filled(_SCALAR_BYTES * 2, 0);
    for (int i = 0; i < t.length; i++) _encode32(t[i], result, i * 4);
    return _reduceScalar(result);
  }

  bool _checkContextVar(List<int> ctx, int phflag) =>
      phflag == 0x00 || ctx.length < 256;

  bool checkPointVar(List<int> p) {
    final t = List.filled(8, 0);
    _decode32(p, 0, t, 0, 8);
    t[7] &= 0x7fffffff;
    return !_gte256(t, _P);
  }

  bool _checkScalarVar(List<int> s) {
    final n = List.filled(_SCALAR_INTS, 0);
    _decodeScalar(s, 0, n);
    return !_gte256(n, _L);
  }

  int _decode24(List<int> bs, int off) {
    int n = bs[off] & 0xff;
    n |= (bs[off + 1] & 0xff) << 8;
    n |= (bs[off + 2] & 0xff) << 16;
    return n;
  }

  int _decode32v(List<int> bs, int off) {
    int n = bs[off] & 0xff;
    n |= (bs[off + 1] & 0xff) << 8;
    n |= (bs[off + 2] & 0xff) << 16;
    n |= bs[off + 3] << 24;
    return n;
  }

  void _decode32(List<int> bs, int bsOff, List<int> n, int nOff, int nLen) {
    for (int i = 0; i < nLen; i++) n[nOff + i] = _decode32v(bs, bsOff + i * 4);
  }

  bool _decodePointVar(List<int> p, int pOff, bool negate, PointExt r) {
    final py = p.sublist(pOff, pOff + _POINT_BYTES);
    if (!checkPointVar(py)) return false;
    final x_0 = (py[_POINT_BYTES - 1] & 0x80) >>> 7;
    py[_POINT_BYTES - 1] = (py[_POINT_BYTES - 1] & 0x7f);
    _x25519Field.decode(py, 0, r.y);
    final u = _x25519Field.create;
    final v = _x25519Field.create;
    _x25519Field.sqr(r.y, u);
    _x25519Field.mul(C_d, u, v);
    _x25519Field.subOne(u);
    _x25519Field.addOne(v);
    if (!_x25519Field.sqrtRatioVar(u, v, r.x)) return false;
    _x25519Field.normalize(r.x);
    if (x_0 == 1 && _x25519Field.isZeroVar(r.x)) return false;
    if (negate ^ (x_0 != (r.x(0) & 1))) _x25519Field.negate(r.x, r.x);
    _pointExtendXY(r);
    return true;
  }

  void _decodeScalar(List<int> k, int kOff, List<int> n) =>
      _decode32(k, kOff, n, 0, _SCALAR_INTS);

  void _encode24(int n, List<int> bs, int off) {
    bs[off] = n;
    bs[off + 1] = n >>> 8;
    bs[off + 2] = n >>> 16;
  }

  void _encode32(int n, List<int> bs, int off) {
    bs[off] = n;
    bs[off + 1] = n >>> 8;
    bs[off + 2] = n >>> 16;
    bs[off + 3] = n >>> 24;
  }

  void encode56(int n, List<int> bs, int off) {
    _encode32(n, bs, off);
    _encode24((n >>> 32), bs, off + 4);
  }

  void encodePoint(PointAccum p, List<int> r, int rOff) {
    final x = _x25519Field.create;
    final y = _x25519Field.create;
    _x25519Field.inv(p.z, y);
    _x25519Field.mul(p.x, y, x);
    _x25519Field.mul(p.y, y, y);
    _x25519Field.normalize(x);
    _x25519Field.normalize(y);
    _x25519Field.encode(y, r, rOff);
    r[rOff + _POINT_BYTES - 1] =
        (r[rOff + _POINT_BYTES - 1] | ((x(0) & 1) << 7));
  }

  List<int> _getWNAF(List<int> n, int width) {
    final t = List.filled(_SCALAR_INTS * 2, 0);
    var tPos = t.length;
    var c = 0;
    var i = _SCALAR_INTS - 1;
    while (i >= 0) {
      final next = n[i];
      tPos -= 1;
      t[tPos] = (next >>> 16) | (c << 16);
      c = next;
      tPos -= 1;
      t[tPos] = c;
      i -= 1;
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
          break;
        }
        var digit = (word16 & mask) + carry;
        carry = digit & sign;
        digit -= (carry << 1);
        carry >>>= (width - 1);
        ws[(i << 4) + j] = digit;
        j += width;
      }
      i += 1;
      j -= 16;
    }
    return ws;
  }

  void _scalarMultBaseYZ(
      List<int> k, List<int> kOff, List<int> y, List<int> z) {
    final n = List.filled(_SCALAR_BYTES, 0);
    _pruneScalar(k, kOff, n);
    final p = PointAccum.fromField(_x25519Field);
    _scalarMultBase(n, p);
    x25519Field.copy(p.y, 0, y, 0);
    x25519Field.copy(p.z, 0, z, 0);
  }

  void _pointAddVar1(bool negate, PointExt p, PointAccum r) {
    final A = _x25519Field.create;
    final B = _x25519Field.create;
    final C = _x25519Field.create;
    final D = _x25519Field.create;
    final E = r.u;
    final F = _x25519Field.create;
    final G = _x25519Field.create;
    final H = r.v;
    List<int> c = List.empty();
    List<int> d = List.empty();
    List<int> f = List.empty();
    List<int> g = List.empty();
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
    _x25519Field.apm(r.y, r.x, B, A);
    _x25519Field.apm(p.y, p.x, d, c);
    _x25519Field.mul(A, C, A);
    _x25519Field.mul(B, D, B);
    _x25519Field.mul(r.u, r.v, C);
    _x25519Field.mul(C, p.t, C);
    _x25519Field.mul(C, _C_d2, C);
    _x25519Field.mul(r.z, p.z, D);
    _x25519Field.add(D, D, D);
    _x25519Field.apm(B, A, H, E);
    _x25519Field.apm(D, C, g, f);
    _x25519Field.carry(g);
    _x25519Field.mul(E, F, r.x);
    _x25519Field.mul(G, H, r.y);
    _x25519Field.mul(F, G, r.z);
  }

  void _pointAddVar2(bool negate, PointExt p, PointExt q, PointExt r) {
    final A = _x25519Field.create;
    final B = _x25519Field.create;
    final C = _x25519Field.create;
    final D = _x25519Field.create;
    final E = _x25519Field.create;
    final F = _x25519Field.create;
    final G = _x25519Field.create;
    final H = _x25519Field.create;
    List<int> c = List.empty();
    List<int> d = List.empty();
    List<int> f = List.empty();
    List<int> g = List.empty();
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
    _x25519Field.apm(p.y, p.x, B, A);
    _x25519Field.apm(q.y, q.x, d, c);
    _x25519Field.mul(A, C, A);
    _x25519Field.mul(B, D, B);
    _x25519Field.mul(p.t, q.t, C);
    _x25519Field.mul(C, _C_d2, C);
    _x25519Field.mul(p.z, q.z, D);
    _x25519Field.add(D, D, D);
    _x25519Field.apm(B, A, H, E);
    _x25519Field.apm(D, C, g, f);
    _x25519Field.carry(g);
    _x25519Field.mul(E, F, r.x);
    _x25519Field.mul(G, H, r.y);
    _x25519Field.mul(F, G, r.z);
    _x25519Field.mul(E, H, r.t);
  }

  void _pointAddPrecomp(PointPrecomp p, PointAccum r) {
    final A = _x25519Field.create;
    final B = _x25519Field.create;
    final C = _x25519Field.create;
    final E = r.u;
    final F = _x25519Field.create;
    final G = _x25519Field.create;
    final H = r.v;
    _x25519Field.apm(r.y, r.x, B, A);
    _x25519Field.mul(A, p.ymx_h, A);
    _x25519Field.mul(B, p.ypx_h, B);
    _x25519Field.mul(r.u, r.v, C);
    _x25519Field.mul(C, p.xyd, C);
    _x25519Field.apm(B, A, H, E);
    _x25519Field.apm(r.z, C, G, F);
    _x25519Field.carry(G);
    _x25519Field.mul(E, F, r.x);
    _x25519Field.mul(G, H, r.y);
    _x25519Field.mul(F, G, r.z);
  }

  PointExt _pointCopyAccum(PointAccum p) {
    final r = PointExt.fromField(_x25519Field);
    _x25519Field.copy(p.x, 0, r.x, 0);
    _x25519Field.copy(p.y, 0, r.y, 0);
    _x25519Field.copy(p.z, 0, r.z, 0);
    _x25519Field.mul(p.u, p.v, r.t);
    return r;
  }

  PointExt _pointCopyExt(PointExt p) {
    final r = PointExt.fromField(_x25519Field);
    _x25519Field.copy(p.x, 0, r.x, 0);
    _x25519Field.copy(p.y, 0, r.y, 0);
    _x25519Field.copy(p.z, 0, r.z, 0);
    _x25519Field.copy(p.t, 0, r.t, 0);
    return r;
  }

  void _pointDouble(PointAccum r) {
    final A = _x25519Field.create;
    final B = _x25519Field.create;
    final C = _x25519Field.create;
    final E = r.u;
    final F = _x25519Field.create;
    final G = _x25519Field.create;
    final H = r.v;
    _x25519Field.sqr(r.x, A);
    _x25519Field.sqr(r.y, B);
    _x25519Field.sqr(r.z, C);
    _x25519Field.add(C, C, C);
    _x25519Field.apm(A, B, H, G);
    _x25519Field.add(r.x, r.y, E);
    _x25519Field.sqr(E, E);
    _x25519Field.sub(H, E, E);
    _x25519Field.add(C, G, F);
    _x25519Field.carry(F);
    _x25519Field.mul(E, F, r.x);
    _x25519Field.mul(G, H, r.y);
    _x25519Field.mul(F, G, r.z);
  }

  void _pointExtendXYAccum(PointAccum p) {
    _x25519Field.one(p.z);
    _x25519Field.copy(p.x, 0, p.u, 0);
    _x25519Field.copy(p.y, 0, p.v, 0);
  }

  void _pointExtendXY(PointExt p) {
    _x25519Field.one(p.z);
    _x25519Field.mul(p.x, p.y, p.t);
  }

  void _pointLookup(int block, int index, PointPrecomp p) {
    var off = block * _PRECOMP_POINTS * 3 * X25519Field.SIZE;
    for (int i = 0; i < _PRECOMP_POINTS; i++) {
      final mask = ((i ^ index) - 1) >> 31
      _cmov(X25519Field.SIZE, mask, _precompBase, off, p.ypx_h, 0);
      off += X25519Field.SIZE;
      _cmov(X25519Field.SIZE, mask, _precompBase, off, p.ymx_h, 0);
      off += X25519Field.SIZE;
      _cmov(X25519Field.SIZE, mask, _precompBase, off, p.xyd, 0);
      off += X25519Field.SIZE;
    }
  }

  List<PointExt> _pointPrecompVar(PointExt p, int count) {
    final d = PointExt.fromField(_x25519Field);
    _pointAddVar2(false, p, p, d);
    final List<PointExt> table = List.empty();
    table.add(_pointCopyExt(p));
    for (int i = 1; i < count; i++) {
      table.add(PointExt.fromField(_x25519Field));
      _pointAddVar2(false, table[i - 1], d, table[i]);
    }
    return table;
  }

  void _pointSetNeutralAccum(PointAccum p) {
    _x25519Field.zero(p.x);
    _x25519Field.one(p.y);
    _x25519Field.one(p.z);
    _x25519Field.zero(p.u);
    _x25519Field.one(p.v);
  }

  void _pointSetNeutralExt(PointExt p) {
    _x25519Field.zero(p.x);
    _x25519Field.one(p.y);
    _x25519Field.one(p.z);
    _x25519Field.zero(p.u);
  }

  void precompute() {
          
      if (_precompBase.isNotEmpty) return;
      // Precomputed table for the base point in verification ladder
      final b = PointExt.fromField(_x25519Field);
      _x25519Field.copy(_B_x, 0, b.x, 0);
      _x25519Field.copy(_B_y, 0, b.y, 0);
      _pointExtendXY(b);
      _precompBaseTable = _pointPrecompVar(b, 1 << (_WNAF_WIDTH_BASE - 2));
      final p = PointAccum.fromField(_x25519Field);
      _x25519Field.copy(_B_x, 0, p.x, 0);
      _x25519Field.copy(_B_y, 0, p.y, 0);
      _pointExtendXY(p);
      _precompBase = List.filled(_PRECOMP_BLOCKS * _PRECOMP_POINTS * 3 * X25519Field.SIZE, 0);
      var off = 0;
      for (int b = 0; b < _PRECOMP_BLOCKS; b++) {
        final List<PointExt> ds = [];
        final  sum = PointExt.fromField(_x25519Field);
        _pointSetNeutralExt(sum);
        for (int t = 0; t < _PRECOMP_TEETH; t++) {
          final  q = _pointCopyAccum(p);
          _pointAddVar2(true, sum, q, sum);
          _pointDouble(p);
          ds.add(_pointCopyAccum(p));
          if (b + t != _PRECOMP_BLOCKS + _PRECOMP_TEETH - 2)
            for (int i = 1; i < _PRECOMP_SPACING; i++)
              _pointDouble(p);
        }
        final List<PointExt> points = [];
        var k = 1;
        points.add(sum);
        for (int t = 0; t < _PRECOMP_TEETH - 1; t++) {
          final  size = 1 << t;
          var j = 0;
          while (j < size) {
            points.add(PointExt.fromField(_x25519Field));
            _pointAddVar2(false, points[k - size], ds[t], points[k]);
            j += 1;
            k += 1;
          }
        }
        for (int i = 0; i < _PRECOMP_POINTS; i++) {
          final  q = points[i];
          final  x = _x25519Field.create;
          final  y = _x25519Field.create;
          _x25519Field.add(q.z, q.z, x);
          _x25519Field.inv(x, y);
          _x25519Field.mul(q.x, y, x);
          _x25519Field.mul(q.y, y, y);
          final  r = PointPrecomp.fromField(_x25519Field);
          _x25519Field.apm(y, x, r.ypx_h, r.ymx_h);
          _x25519Field.mul(x, y, r.xyd);
          _x25519Field.mul(r.xyd, C_d4, r.xyd);
          _x25519Field.normalize(r.ypx_h);
          _x25519Field.normalize(r.ymx_h);
          _x25519Field.copy(r.ypx_h, 0, _precompBase, off);
          off += X25519Field.SIZE;
          _x25519Field.copy(r.ymx_h, 0, _precompBase, off);
          off += X25519Field.SIZE;
          _x25519Field.copy(r.xyd, 0, _precompBase, off);
          off += X25519Field.SIZE;
        }
      }
  }

  void _pruneScalar(List<int> n, int nOff, List<int> r) {
    for(int i = 0; i < _SCALAR_BYTES; i++) {
      r[i] = n[nOff + i];
    }
    r[0] = (r[0] & 0xf8);
    r[_SCALAR_BYTES - 1] = r[_SCALAR_BYTES - 1] & 0x7f;
    r[_SCALAR_BYTES - 1] = r[_SCALAR_BYTES - 1] & 0x40;
  }

  List<int> _reduceScalar(List<int> n) {

    var x00 = _decode32v(n, 0) & _M32L; // x00:32/--
    var x01 = (_decode24(n, 4) << 4) & _M32L; // x01:28/--
    var x02 = _decode32v(n, 7) & _M32L; // x02:32/--
    var x03 = (_decode24(n, 11) << 4) & _M32L; // x03:28/--
    var x04 = _decode32v(n, 14) & _M32L; // x04:32/--
    var x05 = (_decode24(n, 18) << 4) & _M32L; // x05:28/--
    var x06 = _decode32v(n, 21) & _M32L; // x06:32/--
    var x07 = (_decode24(n, 25) << 4) & _M32L; // x07:28/--
    var x08 = _decode32v(n, 28) & _M32L; // x08:32/--
    var x09 = (_decode24(n, 32) << 4) & _M32L; // x09:28/--
    var x10 = _decode32v(n, 35) & _M32L; // x10:32/--
    var x11 = (_decode24(n, 39) << 4) & _M32L; // x11:28/--
    var x12 = _decode32v(n, 42) & _M32L; // x12:32/--
    var x13 = (_decode24(n, 46) << 4) & _M32L; // x13:28/--
    var x14 = _decode32v(n, 49) & _M32L; // x14:32/--
    var x15 = (_decode24(n, 53) << 4) & _M32L; // x15:28/--
    var x16 = _decode32v(n, 56) & _M32L; // x16:32/--
    var x17 = (_decode24(n, 60) << 4) & _M32L; // x17:28/--
    final x18 = n[63] & 0xff; // x18:08/--
    var t = 0;
    x09 -= x18 * _L0; // x09:34/28
    x10 -= x18 * _L1; // x10:33/30
    x11 -= x18 * _L2; // x11:35/28
    x12 -= x18 * _L3; // x12:32/31
    x13 -= x18 * _L4; // x13:28/21
    x17 += (x16 >> 28);
    x16 &= _M28L; // x17:28/--, x16:28/--
    x08 -= x17 * _L0; // x08:54/32
    x09 -= x17 * _L1; // x09:52/51
    x10 -= x17 * _L2; // x10:55/34
    x11 -= x17 * _L3; // x11:51/36
    x12 -= x17 * _L4; // x12:41/--
    x07 -= x16 * _L0; // x07:54/28
    x08 -= x16 * _L1; // x08:54/53
    x09 -= x16 * _L2; // x09:55/53
    x10 -= x16 * _L3; // x10:55/52
    x11 -= x16 * _L4; // x11:51/41
    x15 += (x14 >> 28);
    x14 &= _M28L; // x15:28/--, x14:28/--
    x06 -= x15 * _L0; // x06:54/32
    x07 -= x15 * _L1; // x07:54/53
    x08 -= x15 * _L2; // x08:56/--
    x09 -= x15 * _L3; // x09:55/54
    x10 -= x15 * _L4; // x10:55/53
    x05 -= x14 * _L0; // x05:54/28
    x06 -= x14 * _L1; // x06:54/53
    x07 -= x14 * _L2; // x07:56/--
    x08 -= x14 * _L3; // x08:56/51
    x09 -= x14 * _L4; // x09:56/--
    x13 += (x12 >> 28);
    x12 &= _M28L; // x13:28/22, x12:28/--
    x04 -= x13 * _L0; // x04:54/49
    x05 -= x13 * _L1; // x05:54/53
    x06 -= x13 * _L2; // x06:56/--
    x07 -= x13 * _L3; // x07:56/52
    x08 -= x13 * _L4; // x08:56/52
    x12 += (x11 >> 28);
    x11 &= _M28L; // x12:28/24, x11:28/--
    x03 -= x12 * _L0; // x03:54/49
    x04 -= x12 * _L1; // x04:54/51
    x05 -= x12 * _L2; // x05:56/--
    x06 -= x12 * _L3; // x06:56/52
    x07 -= x12 * _L4; // x07:56/53
    x11 += (x10 >> 28);
    x10 &= _M28L; // x11:29/--, x10:28/--
    x02 -= x11 * _L0; // x02:55/32
    x03 -= x11 * _L1; // x03:55/--
    x04 -= x11 * _L2; // x04:56/55
    x05 -= x11 * _L3; // x05:56/52
    x06 -= x11 * _L4; // x06:56/53
    x10 += (x09 >> 28);
    x09 &= _M28L; // x10:29/--, x09:28/--
    x01 -= x10 * _L0; // x01:55/28
    x02 -= x10 * _L1; // x02:55/54
    x03 -= x10 * _L2; // x03:56/55
    x04 -= x10 * _L3; // x04:57/--
    x05 -= x10 * _L4; // x05:56/53
    x08 += (x07 >> 28);
    x07 &= _M28L; // x08:56/53, x07:28/--
    x09 += (x08 >> 28);
    x08 &= _M28L; // x09:29/25, x08:28/--
    t = x08 >>> 27;
    x09 += t; // x09:29/26
    x00 -= x09 * _L0; // x00:55/53
    x01 -= x09 * _L1; // x01:55/54
    x02 -= x09 * _L2; // x02:57/--
    x03 -= x09 * _L3; // x03:57/--
    x04 -= x09 * _L4; // x04:57/42
    x01 += (x00 >> 28);
    x00 &= _M28L;
    x02 += (x01 >> 28);
    x01 &= _M28L;
    x03 += (x02 >> 28);
    x02 &= _M28L;
    x04 += (x03 >> 28);
    x03 &= _M28L;
    x05 += (x04 >> 28);
    x04 &= _M28L;
    x06 += (x05 >> 28);
    x05 &= _M28L;
    x07 += (x06 >> 28);
    x06 &= _M28L;
    x08 += (x07 >> 28);
    x07 &= _M28L;
    x09 = x08 >> 28;
    x08 &= _M28L;
    x09 -= t;
    x00 += x09 & _L0;
    x01 += x09 & _L1;
    x02 += x09 & _L2;
    x03 += x09 & _L3;
    x04 += x09 & _L4;
    x01 += (x00 >> 28);
    x00 &= _M28L;
    x02 += (x01 >> 28);
    x01 &= _M28L;
    x03 += (x02 >> 28);
    x02 &= _M28L;
    x04 += (x03 >> 28);
    x03 &= _M28L;
    x05 += (x04 >> 28);
    x04 &= _M28L;
    x06 += (x05 >> 28);
    x05 &= _M28L;
    x07 += (x06 >> 28);
    x06 &= _M28L;
    x08 += (x07 >> 28);
    x07 &= _M28L;
    final r = List.filled(_SCALAR_BYTES, 0);
    encode56(x00 | (x01 << 28), r, 0);
    encode56(x02 | (x03 << 28), r, 7);
    encode56(x04 | (x05 << 28), r, 14);
    encode56(x06 | (x07 << 28), r, 21);
    _encode32(x08, r, 28);
    return r;
  }

  void _scalarMultBase(List<int> k, PointAccum r) {
    precompute();
    _pointSetNeutralAccum(r);
    final n = List.filled(_SCALAR_INTS, 0);
    _decodeScalar(k, 0, n);
    // Recode the scalar into signed-digit form, then group comb bits in each block
    _cadd(_SCALAR_INTS, ~n[0] & 1, n, _L, n);
    _shiftDownBit(_SCALAR_INTS, n, 1);
    for (int i = 0; i < _SCALAR_INTS; i++)
      n[i] = _shuffle2(n[i]);
    final p = PointPrecomp.fromField(_x25519Field);
    var cOff = (_PRECOMP_SPACING - 1) * _PRECOMP_TEETH;
      while (true) {
        for (int b = 0; b < _PRECOMP_BLOCKS; b++) {
          final w = n[b] >>> cOff;
          final sign = (w >>> (_PRECOMP_TEETH - 1)) & 1;
          final abs = (w ^ -sign) & _PRECOMP_MASK;
          _pointLookup(b, abs, p);
          _x25519Field.cswap(sign, p.ypx_h, p.ymx_h);
          _x25519Field.cnegate(sign, p.xyd);
          _pointAddPrecomp(p, r);
        }
        cOff -= _PRECOMP_TEETH;
        if (cOff < 0) break;
        _pointDouble(r);
      }
  }

  void _scalarMultBaseEncoded(List<int> k, List<int> r, int rOff) {
    final p = PointAccum.fromField(_x25519Field);
    _scalarMultBase(k, p);
    encodePoint(p, r, rOff);
  }

  void _scalarMultStraussVar(List<int> nb, List<int> np, PointExt p, PointAccum r) {
    precompute();
    final width = 5;
    final ws_b = _getWNAF(nb, _WNAF_WIDTH_BASE);
    final ws_p = _getWNAF(np, width);
    final tp = _pointPrecompVar(p, 1 << (width - 2));
    _pointSetNeutralAccum(r);
    var bit = 255;
    while (bit > 0 && (ws_b[bit] | ws_p[bit]) == 0) bit -= 1;
      while (true) {
        final wb = ws_b[bit];
        if (wb != 0) {
          final sign = wb >> 31;
          final index = (wb ^ sign) >>> 1;
          _pointAddVar1(sign != 0, _precompBaseTable[index], r);
        }
        final wp = ws_p[bit];
        if (wp != 0) {
          final sign = wp >> 31;
          final index = (wp ^ sign) >>> 1;
          _pointAddVar1(sign != 0, tp[index], r);
        }
        bit -= 1;
        if (bit < 0) break;
        _pointDouble(r);
      }
  }
}

class X25519Field {
  static const SIZE = 10;
  List<int> get create;
}

class PointAccum {
  final List<int> x;
  final List<int> y;
  final List<int> z;
  final List<int> u;
  final List<int> v;

  PointAccum(this.x, this.y, this.z, this.u, this.v);

  factory PointAccum.fromField(X25519Field x25519field) => PointAccum(
        x25519field.create,
        x25519field.create,
        x25519field.create,
        x25519field.create,
        x25519field.create,
      );
}

class PointExt {
  final List<int> x;
  final List<int> y;
  final List<int> z;
  final List<int> t;

  PointExt(this.x, this.y, this.z, this.t);

  factory PointExt.fromField(X25519Field x25519field) => PointExt(
        x25519field.create,
        x25519field.create,
        x25519field.create,
        x25519field.create,
      );
}

class PointPrecomp {
  final List<int> ypx_h;
  final List<int> ymx_h;
  final List<int> xyd;

  PointPrecomp(this.ypx_h, this.ymx_h, this.xyd);

  factory PointPrecomp.fromField(X25519Field x25519field) => PointPrecomp(
        x25519field.create,
        x25519field.create,
        x25519field.create,
      );
}

const _POINT_BYTES = 32;
const _SCALAR_INTS = 8;
const _SCALAR_BYTES = _SCALAR_INTS * 4;
const _PREHASH_SIZE = 64;
const _PUBLIC_KEY_SIZE = _POINT_BYTES;
const _SECRET_KEY_SIZE = 32;
const _SIGNATURE_SIZE = _POINT_BYTES + _SCALAR_BYTES;
const _DOM2_PREFIX = "SigEd25519 no Ed25519 collisions";
const _M28L = 0x0fffffff;
const _M32L = 0xffffffff;
const _P = [
  0xffffffed,
  0xffffffff,
  0xffffffff,
  0xffffffff,
  0xffffffff,
  0xffffffff,
  0xffffffff,
  0x7fffffff
];
const _L = [
  0x5cf5d3ed,
  0x5812631a,
  0xa2f79cd6,
  0x14def9de,
  0x00000000,
  0x00000000,
  0x00000000,
  0x10000000
];
const _L0 = 0xfcf5d3ed;
const _L1 = 0x012631a6;
const _L2 = 0x079cd658;
const _L3 = 0xff9dea2f;
const _L4 = 0x000014df;

const _B_x = [
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

const _B_y = [
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

const _C_d = [
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

const _C_d2 = [
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

const _C_d4 = [
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
const _WNAF_WIDTH_BASE = 7;
const _PRECOMP_BLOCKS = 8;
const _PRECOMP_TEETH = 4;
const _PRECOMP_SPACING = 8;
const _PRECOMP_POINTS = 1 << (_PRECOMP_TEETH - 1);
const _PRECOMP_MASK = _PRECOMP_POINTS - 1;
const _M = 0xffffffff;
