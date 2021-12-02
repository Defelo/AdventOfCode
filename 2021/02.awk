BEGIN {
    d1 = 0
    d2 = 0
    h = 0
    x = 0
}

/^forward [0-9]+$/ {
    h += $2
    d2 += d1 * $2
}
/^down [0-9]+$/ { d1 += $2 }
/^up [0-9]+$/ { d1 -= $2 }

END {
    print "Part 1:", d1*h
    print "Part 2:", d2*h
}
