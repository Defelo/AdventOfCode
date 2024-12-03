use std::{convert::Infallible, fmt::Display};

use rustc_hash::FxHashMap;
use thiserror::Error;

pub type Int = i64;

pub fn get_output(
    program: impl IntoIterator<Item = Int>,
    input: impl IntoIterator<Item = Int>,
) -> Result<Vec<Int>, Error<EofError, Infallible>> {
    let input = IterInput::from(input);
    let mut output = Vec::new();
    let mut vm = Vm::with_io(program, input, &mut output);
    vm.run()?;
    Ok(output)
}

#[derive(Debug, Clone)]
pub struct Vm<I: Input, O: Output> {
    mem: FxHashMap<Int, Int>,
    ip: Int,
    base: Int,
    inp: I,
    out: O,
}

impl Vm<(), ()> {
    pub fn new(program: impl IntoIterator<Item = Int>) -> Self {
        Self::with_io(program, (), ())
    }
}

impl<I: Input, O: Output> Vm<I, O> {
    pub fn with_io(program: impl IntoIterator<Item = Int>, input: I, output: O) -> Self {
        Self {
            mem: program
                .into_iter()
                .enumerate()
                .map(|(i, x)| (i as _, x))
                .collect(),
            ip: 0,
            base: 0,
            inp: input,
            out: output,
        }
    }

    pub fn read(&self, addr: Int) -> Int {
        self.mem.get(&addr).copied().unwrap_or(0)
    }

    pub fn write(&mut self, addr: Int, value: Int) -> Int {
        self.mem.insert(addr, value).unwrap_or(0)
    }

    pub fn run(&mut self) -> Result<(), Error<I::Error, O::Error>> {
        while self.step()? {}
        Ok(())
    }

    pub fn step(&mut self) -> Result<bool, Error<I::Error, O::Error>> {
        let op = self.parse_op()?;

        match op {
            Op::Add { in1, in2, out } => {
                self.write_arg(out, self.read_arg(in1) + self.read_arg(in2))?;
                self.ip += 4;
            }
            Op::Mul { in1, in2, out } => {
                self.write_arg(out, self.read_arg(in1) * self.read_arg(in2))?;
                self.ip += 4;
            }
            Op::In(arg) => {
                let value = self
                    .inp
                    .read()
                    .map_err(|err| Error::Input { ip: self.ip, err })?;
                self.write_arg(arg, value)?;
                self.ip += 2;
            }
            Op::Out(arg) => {
                let value = self.read_arg(arg);
                self.out
                    .write(value)
                    .map_err(|err| Error::Output { ip: self.ip, err })?;
                self.ip += 2;
            }
            Op::Jeq { arg, addr } => {
                if self.read_arg(arg) != 0 {
                    self.ip = self.read_arg(addr);
                } else {
                    self.ip += 3;
                }
            }
            Op::Jne { arg, addr } => {
                if self.read_arg(arg) == 0 {
                    self.ip = self.read_arg(addr);
                } else {
                    self.ip += 3;
                }
            }
            Op::Lt { in1, in2, out } => {
                self.write_arg(out, (self.read_arg(in1) < self.read_arg(in2)) as _)?;
                self.ip += 4;
            }
            Op::Eq { in1, in2, out } => {
                self.write_arg(out, (self.read_arg(in1) == self.read_arg(in2)) as _)?;
                self.ip += 4;
            }
            Op::Base(offset) => {
                self.base += self.read_arg(offset);
                self.ip += 2;
            }
            Op::Halt => return Ok(false),
        }

        Ok(true)
    }

    fn parse_op(&self) -> Result<Op, Error<I::Error, O::Error>> {
        let code = self.read(self.ip) % 100;
        let in1 = self.parse_arg::<0>()?;
        let in2 = self.parse_arg::<1>()?;
        let out = self.parse_arg::<2>()?;
        Ok(match code {
            1 => Op::Add { in1, in2, out },
            2 => Op::Mul { in1, in2, out },
            3 => Op::In(in1),
            4 => Op::Out(in1),
            5 => Op::Jeq {
                arg: in1,
                addr: in2,
            },
            6 => Op::Jne {
                arg: in1,
                addr: in2,
            },
            7 => Op::Lt { in1, in2, out },
            8 => Op::Eq { in1, in2, out },
            9 => Op::Base(in1),
            99 => Op::Halt,
            code => return Err(Error::InvalidOpcode { ip: self.ip, code }),
        })
    }

    fn parse_arg<const N: u32>(&self) -> Result<Arg, Error<I::Error, O::Error>> {
        let arg = self.read(self.ip + 1 + N as Int);
        let mode = self.read(self.ip) / 10i64.pow(2 + N) % 10;
        Ok(match mode {
            0 => Arg::Addr(arg),
            1 => Arg::Immediate(arg),
            2 => Arg::Relative(arg),
            _ => {
                return Err(Error::InvalidArgMode {
                    ip: self.ip,
                    n: N,
                    mode,
                })
            }
        })
    }

    fn read_arg(&self, arg: Arg) -> Int {
        match arg {
            Arg::Addr(addr) => self.read(addr),
            Arg::Immediate(value) => value,
            Arg::Relative(offset) => self.read(self.base + offset),
        }
    }

    fn write_arg(&mut self, arg: Arg, value: Int) -> Result<Int, Error<I::Error, O::Error>> {
        Ok(match arg {
            Arg::Addr(addr) => self.write(addr, value),
            Arg::Immediate(_) => return Err(Error::WriteImmediate { ip: self.ip }),
            Arg::Relative(offset) => self.write(self.base + offset, value),
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Op {
    Add { in1: Arg, in2: Arg, out: Arg }, // 1
    Mul { in1: Arg, in2: Arg, out: Arg }, // 2
    In(Arg),                              // 3
    Out(Arg),                             // 4
    Jeq { arg: Arg, addr: Arg },          // 5
    Jne { arg: Arg, addr: Arg },          // 6
    Lt { in1: Arg, in2: Arg, out: Arg },  // 7
    Eq { in1: Arg, in2: Arg, out: Arg },  // 8
    Base(Arg),                            // 9
    Halt,                                 // 99
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Arg {
    Addr(Int),
    Immediate(Int),
    Relative(Int),
}

#[derive(Debug, Error)]
pub enum Error<I: Display, O: Display> {
    #[error("Invalid opcode {code} at {ip}")]
    InvalidOpcode { ip: Int, code: Int },
    #[error("Cannot write to arg in immediate mode at {ip}")]
    WriteImmediate { ip: Int },
    #[error("Invalid arg mode {mode} at {ip} (arg {n})")]
    InvalidArgMode { ip: Int, n: u32, mode: Int },
    #[error("Input error at {ip}: {err}")]
    Input { ip: Int, err: I },
    #[error("Output error at {ip}: {err}")]
    Output { ip: Int, err: O },
}

pub trait Input {
    type Error: Display;

    fn read(&mut self) -> Result<Int, Self::Error>;
}

pub trait Output {
    type Error: Display;

    fn write(&mut self, value: Int) -> Result<(), Self::Error>;
}

#[derive(Debug, Error)]
#[error("Input is disabled")]
pub struct NoInputError;

impl Input for () {
    type Error = NoInputError;

    fn read(&mut self) -> Result<Int, Self::Error> {
        Err(NoInputError)
    }
}

#[derive(Debug, Error)]
#[error("Output is disabled")]
pub struct NoOutputError;

impl Output for () {
    type Error = NoOutputError;

    fn write(&mut self, _value: Int) -> Result<(), Self::Error> {
        Err(NoOutputError)
    }
}

#[derive(Debug, Error)]
#[error("End of file")]
pub struct EofError;

pub struct IterInput<I>(I);

impl<I> From<I> for IterInput<I::IntoIter>
where
    I: IntoIterator,
{
    fn from(value: I) -> Self {
        Self(value.into_iter())
    }
}

impl<I> Input for IterInput<I>
where
    I: Iterator<Item = Int>,
{
    type Error = EofError;

    fn read(&mut self) -> Result<Int, Self::Error> {
        self.0.next().ok_or(EofError)
    }
}

impl Output for &mut Vec<Int> {
    type Error = Infallible;

    fn write(&mut self, value: Int) -> Result<(), Self::Error> {
        self.push(value);
        Ok(())
    }
}
