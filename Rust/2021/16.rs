#![feature(test)]

type Input = Vec<bool>;

fn setup(input: &str) -> Input {
    input
        .trim()
        .chars()
        .flat_map(|c| {
            let n = "0123456789ABCDEF".find(c).unwrap();
            (0..4).rev().map(move |i| n & (1 << i) > 0)
        })
        .collect()
}

struct Reader<'a> {
    data: &'a [bool],
    pos: usize,
}

impl<'a> Reader<'a> {
    fn new(data: &'a [bool]) -> Self {
        Reader { data, pos: 0 }
    }

    fn read(&mut self, n: usize) -> u64 {
        let mut out = 0;
        for _ in 0..n {
            if self.pos >= self.data.len() {
                break;
            }
            out <<= 1;
            if self.data[self.pos] {
                out |= 1;
            }
            self.pos += 1;
        }
        out
    }
}

struct Result {
    version_sum: u64,
    value: u64,
}

fn solve(io: &mut Reader) -> Result {
    let mut version = io.read(3);
    let tid = io.read(3);
    if tid == 4 {
        let mut lit = 0;
        loop {
            let c = io.read(1);
            lit <<= 4;
            lit |= io.read(4);
            if c == 0 {
                break;
            }
        }
        return Result {
            version_sum: version,
            value: lit,
        };
    }

    let mut values = vec![];
    if io.read(1) == 0 {
        let c = io.read(15) as usize + io.pos;
        while io.pos < c {
            let result = solve(io);
            version += result.version_sum;
            values.push(result.value);
        }
    } else {
        for _ in 0..io.read(11) {
            let result = solve(io);
            version += result.version_sum;
            values.push(result.value);
        }
    }

    Result {
        version_sum: version,
        value: match tid {
            0 => values.iter().cloned().sum(),
            1 => values.iter().cloned().product(),
            2 => values.iter().cloned().min().unwrap(),
            3 => values.iter().cloned().max().unwrap(),
            5 => (values[0] > values[1]) as u64,
            6 => (values[0] < values[1]) as u64,
            7 => (values[0] == values[1]) as u64,
            _ => panic!(),
        },
    }
}

fn part1(input: &Input) -> u64 {
    let mut reader = Reader::new(input);
    solve(&mut reader).version_sum
}

fn part2(input: &Input) -> u64 {
    let mut reader = Reader::new(input);
    solve(&mut reader).value
}

aoc::main!(2021, 16, ex: "1a"[a], "2a"[a], "3a"[a], "4a"[a], "1b"[b], "2b"[b], "3b"[b], "4b"[b], "5b"[b], "6b"[b], "7b"[b], "8b"[b]);
