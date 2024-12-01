use rustc_hash::FxHashMap;
use thiserror::Error;

pub type Int = i64;

#[derive(Debug, Clone)]
pub struct Vm {
    mem: FxHashMap<Int, Int>,
    ip: Int,
}

impl Vm {
    pub fn new(program: impl IntoIterator<Item = Int>) -> Self {
        Self {
            mem: program
                .into_iter()
                .enumerate()
                .map(|(i, x)| (i as _, x))
                .collect(),
            ip: 0,
        }
    }

    pub fn read(&self, addr: Int) -> Int {
        self.mem.get(&addr).copied().unwrap_or(0)
    }

    pub fn write(&mut self, addr: Int, value: Int) -> Int {
        self.mem.insert(addr, value).unwrap_or(0)
    }

    pub fn run(&mut self) -> Result<()> {
        while self.step()? {}
        Ok(())
    }

    pub fn step(&mut self) -> Result<bool> {
        let op = self.parse_op()?;

        match op {
            Op::Add { in1, in2, out } => {
                self.write_arg(out, self.read_arg(in1) + self.read_arg(in2));
                self.ip += 4;
            }
            Op::Mul { in1, in2, out } => {
                self.write_arg(out, self.read_arg(in1) * self.read_arg(in2));
                self.ip += 4;
            }
            Op::Halt => return Ok(false),
        }

        Ok(true)
    }

    fn parse_op(&self) -> Result<Op> {
        let code = self.read(self.ip);
        let in1 = self.parse_arg(0);
        let in2 = self.parse_arg(1);
        let out = self.parse_arg(2);
        Ok(match code {
            1 => Op::Add { in1, in2, out },
            2 => Op::Mul { in1, in2, out },
            99 => Op::Halt,
            code => return Err(Error::InvalidOpcode { ip: self.ip, code }),
        })
    }

    fn parse_arg(&self, n: Int) -> Arg {
        let arg = self.read(self.ip + 1 + n);
        Arg::Addr(arg)
    }

    fn read_arg(&self, arg: Arg) -> Int {
        match arg {
            Arg::Addr(addr) => self.read(addr),
        }
    }

    fn write_arg(&mut self, arg: Arg, value: Int) -> Int {
        match arg {
            Arg::Addr(addr) => self.write(addr, value),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Op {
    Add { in1: Arg, in2: Arg, out: Arg }, // 1
    Mul { in1: Arg, in2: Arg, out: Arg }, // 2
    Halt,                                 // 99
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Arg {
    Addr(Int),
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("Invalid opcode {code} at {ip}")]
    InvalidOpcode { ip: Int, code: Int },
}

pub type Result<T, E = Error> = core::result::Result<T, E>;
