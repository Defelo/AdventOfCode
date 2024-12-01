left, right = File.read(ARGV[0]).lines.map { _1.split.map &:to_i }.transpose

puts left.sort.zip(right.sort).map { _1.reduce(&:-).abs }.sum
puts left.map { _1 * right.count(_1) }.sum
