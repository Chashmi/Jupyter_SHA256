import struct
import math

# Binary Conversion
message = "hello world"
binary_value = [format(ord(char), '08b') for char in message]
print(binary_value)

# Adding the one bit
binary_str = ''.join(binary_value)
binary_str_with_one = binary_str + '1'

print(binary_str_with_one)


# Padding to 448 bits

target_length = 448
current_length = len(binary_str_with_one)
zeros_to_add = target_length - current_length
# print(zeros_to_add)

padded_binary_str = binary_str_with_one + ('0' * zeros_to_add)

print(padded_binary_str)

# Adding the 64 bit encoding

original_length = 88
original_length_bin = format(original_length, '064b')

# print(original_length_bin)

final_binary_str = padded_binary_str + original_length_bin

print(final_binary_str)


# Converting the 512 bit into 16, 32 bit blocks

blocks = [final_binary_str[i:i+32] for i in range(0, 512, 32)]

for idx, block in enumerate(blocks):
    print(f'Message {idx:2}: {block}')


def rightrotate(x, n):
    return ((x >> n) | (x << (32 - n))) & 0xFFFFFFFF

# 'blocks' previously created (list of 16, 32-bit binary strings)
W = [int(b, 2) for b in blocks]

# Extend the first 16 words into the remaining 48 words
for i in range(16, 64):
    s0 = (rightrotate(W[i-15], 7)) ^ (rightrotate(W[i-15], 18)) ^ (W[i-15] >> 3)
    s1 = (rightrotate(W[i-2], 17)) ^ (rightrotate(W[i-2], 19)) ^ (W[i-2] >> 10)
    new_word = (W[i-16] + s0 + W[i-7] + s1) & 0xFFFFFFFF
    W.append(new_word)

# Print message schedule (W[0..63])
for idx, word in enumerate(W):
    print(f"W[{idx:2}] = {word:032b}")


# SHA-256 initial hash values (previously defined)
H = [
    0x6a09e667,
    0xbb67ae85,
    0x3c6ef372,
    0xa54ff53a,
    0x510e527f,
    0x9b05688c,
    0x1f83d9ab,
    0x5be0cd19
]

# Initialize working variables
a, b, c, d, e, f, g, h = H

# Verify initialization
print(f"a: {a:#010x}")
print(f"b: {b:#010x}")
print(f"c: {c:#010x}")
print(f"d: {d:#010x}")
print(f"e: {e:#010x}")
print(f"f: {f:#010x}")
print(f"g: {g:#010x}")
print(f"h: {h:#010x}")


# SHA-256 constants (K values)
K = [
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b,
    0x59f111f1, 0x923f82a4, 0xab1c5ed5, 0xd807aa98, 0x12835b01,
    0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7,
    0xc19bf174, 0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
    0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da, 0x983e5152,
    0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147,
    0x06ca6351, 0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc,
    0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819,
    0xd6990624, 0xf40e3585, 0x106aa070, 0x19a4c116, 0x1e376c08,
    0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f,
    0x682e6ff3, 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
    0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
]

# Compression loop (core SHA-256 step)
for i in range(64):
    S1 = (rightrotate(e, 6)) ^ (rightrotate(e, 11)) ^ (rightrotate(e, 25))
    Ch = (e & f) ^ ((~e) & g)
    T1 = (h + S1 + Ch + K[i] + W[i]) & 0xFFFFFFFF
    S0 = (rightrotate(a, 2)) ^ (rightrotate(a, 13)) ^ (rightrotate(a, 22))
    Maj = (a & b) ^ (a & c) ^ (b & c)
    T2 = (S0 + Maj) & 0xFFFFFFFF

    # Update working variables
    h = g
    g = f
    f = e
    e = (d + T1) & 0xFFFFFFFF
    d = c
    c = b
    b = a
    a = (T1 + T2) & 0xFFFFFFFF

# After loop completes, add to initial hash values:
H[0] = (H[0] + a) & 0xFFFFFFFF
H[1] = (H[1] + b) & 0xFFFFFFFF
H[2] = (H[2] + c) & 0xFFFFFFFF
H[3] = (H[3] + d) & 0xFFFFFFFF
H[4] = (H[4] + e) & 0xFFFFFFFF
H[5] = (H[5] + f) & 0xFFFFFFFF
H[6] = (H[6] + g) & 0xFFFFFFFF
H[7] = (H[7] + h) & 0xFFFFFFFF


# Final SHA-256 hash (concatenate H0 to H7)
sha256_hash = ''.join(f'{value:08x}' for value in H)

print(f"SHA-256 hash: {sha256_hash}")


import hashlib

message = "hello world"
sha256_hash_lib = hashlib.sha256(message.encode()).hexdigest()

print(sha256_hash_lib)
