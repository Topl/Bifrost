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

package co.topl.codecs.binary.scodecs.valuetypes.varint64fast;


import java.util.Arrays;

/**
 * This code is copied from https://github.com/protocolbuffers/protobuf/blob/master/java/core/src/main/java/com/google/protobuf/CodedOutputStream.java
 *
 * It has been slightly modified for compatibility with Scodec.
 *
 * To use, first call `writeVarint64`, then `getResult`.
 *
 * NOTE: It should not be improved for readability.
 */
public class Varint64FastEncode {

    private final byte[] buffer;
    private int position;

    public Varint64FastEncode() {
        int limit = 10;
        buffer = new byte[limit];
        position = 0;
    }

    /**
     * Writes a Var int 64 to the internal buffer.
     * @param value the value to encode
     * @throws Exception an exception that occurs while reading the value
     */
    public void writeVarint64(long value) throws Exception {
        try {
            while (true) {
                if ((value & ~0x7FL) == 0) {
                    buffer[position++] = (byte) value;
                    return;
                } else {
                    buffer[position++] = (byte) (((int) value & 0x7F) | 0x80);
                    value >>>= 7;
                }
            }
        } catch (IndexOutOfBoundsException e) {
            throw new Exception("buffer overflow exception: failed to write ULong value to buffer", e);
        }
    }

    /**
     * Gets the resulting bytes of encoding a value.
     * @return a byte array representing an encoded var int 64
     */
    public byte[] getResult() {
        return Arrays.copyOfRange(buffer, 0, position);
    }
}
