// Protocol Buffers - Google's data interchange format
// Copyright 2008 Google Inc.  All rights reserved.
// https://developers.google.com/protocol-buffers/
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     * Redistributions of source code must retain the above copyright
// notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above
// copyright notice, this list of conditions and the following disclaimer
// in the documentation and/or other materials provided with the
// distribution.
//     * Neither the name of Google Inc. nor the names of its
// contributors may be used to endorse or promote products derived from
// this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

package co.topl.codecs.bytes.scodecs.valuetypes.varint64fast;

import java.util.Arrays;

/**
 * This code is copied from https://github.com/protocolbuffers/protobuf/blob/master/java/core/src/main/java/com/google/protobuf/CodedInputStream.java
 *
 * It has been slightly modified for compatibility with Scodec.
 *
 * NOTE: It should not be improved for readability.
 */
public class Varint64FastDecode {

    private int pos;
    private final byte[] buffer;
    private final int limit;

    public Varint64FastDecode(byte[] input) {
        pos = 0;
        buffer = input;
        limit = input.length;
    }

    /**
     * Decodes a raw var int 64 (ULong) from the buffer.
     * @return a raw var int 64 value
     * @throws Exception an exception that occurs while reading the value
     */
    public long readRawVarint64() throws Exception {
        // Implementation notes:
        //
        // Optimized for one-byte values, expected to be common.
        // The particular code below was selected from various candidates
        // empirically, by winning VarintBenchmark.
        //
        // Sign extension of (signed) Java bytes is usually a nuisance, but
        // we exploit it here to more easily obtain the sign of bytes read.
        // Instead of cleaning up the sign extension bits by masking eagerly,
        // we delay until we find the final (positive) byte, when we clear all
        // accumulated bits with one xor.  We depend on javac to constant fold.
        fastpath:
        {
            int tempPos = pos;

            if (limit == tempPos) {
                break fastpath;
            }

            final byte[] buffer = this.buffer;
            long x;
            int y;
            if ((y = buffer[tempPos++]) >= 0) {
                pos = tempPos;
                return y;
            } else if (limit - tempPos < 9) {
                break fastpath;
            } else if ((y ^= (buffer[tempPos++] << 7)) < 0) {
                x = y ^ (~0 << 7);
            } else if ((y ^= (buffer[tempPos++] << 14)) >= 0) {
                x = y ^ ((~0 << 7) ^ (~0 << 14));
            } else if ((y ^= (buffer[tempPos++] << 21)) < 0) {
                x = y ^ ((~0 << 7) ^ (~0 << 14) ^ (~0 << 21));
            } else if ((x = y ^ ((long) buffer[tempPos++] << 28)) >= 0L) {
                x ^= (~0L << 7) ^ (~0L << 14) ^ (~0L << 21) ^ (~0L << 28);
            } else if ((x ^= ((long) buffer[tempPos++] << 35)) < 0L) {
                x ^= (~0L << 7) ^ (~0L << 14) ^ (~0L << 21) ^ (~0L << 28) ^ (~0L << 35);
            } else if ((x ^= ((long) buffer[tempPos++] << 42)) >= 0L) {
                x ^= (~0L << 7) ^ (~0L << 14) ^ (~0L << 21) ^ (~0L << 28) ^ (~0L << 35) ^ (~0L << 42);
            } else if ((x ^= ((long) buffer[tempPos++] << 49)) < 0L) {
                x ^=
                        (~0L << 7)
                                ^ (~0L << 14)
                                ^ (~0L << 21)
                                ^ (~0L << 28)
                                ^ (~0L << 35)
                                ^ (~0L << 42)
                                ^ (~0L << 49);
            } else {
                x ^= ((long) buffer[tempPos++] << 56);
                x ^=
                        (~0L << 7)
                                ^ (~0L << 14)
                                ^ (~0L << 21)
                                ^ (~0L << 28)
                                ^ (~0L << 35)
                                ^ (~0L << 42)
                                ^ (~0L << 49)
                                ^ (~0L << 56);
                if (x < 0L) {
                    if (buffer[tempPos++] < 0L) {
                        break fastpath; // Will throw malformedVarint()
                    }
                }
            }
            pos = tempPos;
            return x;
        }
        return readRawVarint64SlowPath();
    }

    /**
     * Gets the remaining bytes left over after decoding a var int 64 (ULong).
     * @return a byte array of remaining un-decoded data
     */
    public byte[] getRemaining() {
        if (pos >= limit)  {
            return new byte[0];
        } else {
            return Arrays.copyOfRange(buffer, pos, limit);
        }
    }

    protected long readRawVarint64SlowPath() throws Exception {
        long result = 0;
        for (int shift = 0; shift < 64; shift += 7) {
            final byte b = readRawByte();
            result |= (long) (b & 0x7F) << shift;
            if ((b & 0x80) == 0) {
                return result;
            }
        }
        throw new Exception("ULong decode error: value malformed");
    }

    protected byte readRawByte() throws Exception {
        if (pos == limit) {
            throw new Exception("buffer underflow error: attempting to read a raw byte from byte buffer");
        }
        return buffer[pos++];
    }
}