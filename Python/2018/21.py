r1 = 0
r5 = r1 | 65536
r1 = 8586263
while r5:
    r1 += r5 & 255
    r1 &= 16777215
    r1 *= 65899
    r1 &= 16777215
    r5 //= 256
print(r1)


r1 = 0
seen = set()
out = r1
while r1 not in seen:
    out = r1
    seen.add(r1)
    r5 = r1 | 65536
    r1 = 8586263
    while r5:
        r1 += r5 & 255
        r1 &= 16777215
        r1 *= 65899
        r1 &= 16777215
        r5 //= 256

print(out)
