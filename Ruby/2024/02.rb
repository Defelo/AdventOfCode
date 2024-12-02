def check(nums)
  d = nums[...-1].zip(nums[1..]).map { _1.reduce &:- }
  return d.all? { (1..3).include?(_1.abs) && _1.negative? == d[0].negative? }
end

input = File.read(ARGV[0]).lines.map { _1.split.map &:to_i }

p input.filter { check _1 }.length
p input.filter { |nums| (0...nums.length).any? { check nums[..._1] + nums[_1+1..] } }.length
