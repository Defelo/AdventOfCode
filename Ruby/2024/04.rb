input = File.read(ARGV[0]).lines.map &:strip

NEIGH = [
  [-1, -1],
  [-1, 0],
  [-1, 1],
  [0, -1],
  [0, 1],
  [1, -1],
  [1, 0],
  [1, 1],
]

w = input[0].length
h = input.length
puts (0...h).flat_map { |i|
  (0...w).map { |j|
    NEIGH
      .filter { |n| (0...h).include?(i + 3 * n[0]) && (0...w).include?(j + 3 * n[1]) }
      .count { |n| (0...4).map { input[i + _1 * n[0]][j + _1 * n[1]] }.join == "XMAS" }
  }
}.sum

puts (1...h-1).map { |i|
  (1...w-1).count { |j|
    input[i][j] == "A" \
      && Set[input[i-1][j-1],input[i+1][j+1]] == Set[?M,?S] \
      && Set[input[i-1][j+1],input[i+1][j-1]] == Set[?M,?S]
  }
}.sum
