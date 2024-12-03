input = File.read(ARGV[0])

a = 0
b = 0
enabled = true

input.scan(/(mul\((\d+),(\d+)\)|do\(\)|don't\(\))/).each {
  if _1[0] == "do()"
    enabled = true
  elsif _1[0] == "don't()"
    enabled = false
  else
    x = _1[1].to_i * _1[2].to_i
    a += x
    b += x if enabled
  end
}

puts a
puts b
