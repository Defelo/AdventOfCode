rules, updates = File.read(ARGV[0]).split("\n\n").map { _1.lines.map { |l| l.scan(/\d+/).map &:to_i } }

sorted = updates.map {
  _1.sort { |a, b| if rules.include? [a, b] then -1 elsif rules.include? [b, a] then 1 else 0 end }
}

p sorted.zip(updates).filter { _1[0] == _1[1] }.map { _1[0][_1[0].length/2] }.sum
p sorted.zip(updates).filter { _1[0] != _1[1] }.map { _1[0][_1[0].length/2] }.sum
