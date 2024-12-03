use std::collections::VecDeque;

use rustc_hash::FxHashMap;
use thiserror::Error;

pub type Int = i64;

#[derive(Debug, Clone)]
pub struct IntcodeVm {
    memory: FxHashMap<Int, Int>,
    ip: Int,
    base: Int,
    input: VecDeque<Int>,
    output: VecDeque<Int>,
}

impl IntcodeVm {
    /// Create a new vm and load the given program.
    pub fn new(program: impl IntoIterator<Item = Int>) -> Self {
        Self::with_input(program, [])
    }

    /// Create a new vm and load the given program and input.
    pub fn with_input(
        program: impl IntoIterator<Item = Int>,
        input: impl Into<VecDeque<Int>>,
    ) -> Self {
        Self {
            memory: program
                .into_iter()
                .enumerate()
                .map(|(i, x)| (i as _, x))
                .collect(),
            ip: 0,
            base: 0,
            input: input.into(),
            output: Default::default(),
        }
    }

    /// Push additional input to the end of the queue.
    pub fn push_input(&mut self, input: impl IntoIterator<Item = Int>) {
        self.input.extend(input);
    }

    /// Pop values from the output queue.
    pub fn pop_output(&mut self) -> Option<Int> {
        self.output.pop_front()
    }

    /// Return the next output value.
    ///
    /// Advances execution only if the output queue is empty.
    pub fn next_output(&mut self) -> Result<Option<Int>> {
        loop {
            if let Some(out) = self.pop_output() {
                return Ok(Some(out));
            }
            if !self.step()? {
                return Ok(None);
            }
        }
    }

    /// Return and clear the output queue.
    pub fn take_output(&mut self) -> VecDeque<Int> {
        std::mem::take(&mut self.output)
    }

    /// Read the value at the given address from memory.
    pub fn read(&self, addr: Int) -> Int {
        self.memory.get(&addr).copied().unwrap_or(0)
    }

    /// Write the given value to the given address in memory.
    pub fn write(&mut self, addr: Int, value: Int) -> Int {
        self.memory.insert(addr, value).unwrap_or(0)
    }

    /// Run the program until it halts or an error occurs.
    pub fn run(&mut self) -> Result<()> {
        while self.step()? {}
        Ok(())
    }

    /// Run a single step of the program (i.e. one operation).
    ///
    /// Returns `true` if execution can continue and `false` if the program has
    /// halted.
    pub fn step(&mut self) -> Result<bool> {
        match self.parse_instruction()? {
            Instruction::Add(in1, in2, out) => {
                self.write_arg(out, self.read_arg(in1) + self.read_arg(in2))?;
                self.ip += 4;
            }
            Instruction::Mul(in1, in2, out) => {
                self.write_arg(out, self.read_arg(in1) * self.read_arg(in2))?;
                self.ip += 4;
            }
            Instruction::In(arg) => {
                let value = self.input.pop_front().ok_or(Error::NeedsInput)?;
                self.write_arg(arg, value)?;
                self.ip += 2;
            }
            Instruction::Out(arg) => {
                let value = self.read_arg(arg);
                self.output.push_back(value);
                self.ip += 2;
            }
            Instruction::Jeq(arg, addr) => {
                if self.read_arg(arg) != 0 {
                    self.ip = self.read_arg(addr);
                } else {
                    self.ip += 3;
                }
            }
            Instruction::Jne(arg, addr) => {
                if self.read_arg(arg) == 0 {
                    self.ip = self.read_arg(addr);
                } else {
                    self.ip += 3;
                }
            }
            Instruction::Lt(in1, in2, out) => {
                self.write_arg(out, (self.read_arg(in1) < self.read_arg(in2)) as _)?;
                self.ip += 4;
            }
            Instruction::Eq(in1, in2, out) => {
                self.write_arg(out, (self.read_arg(in1) == self.read_arg(in2)) as _)?;
                self.ip += 4;
            }
            Instruction::Base(offset) => {
                self.base += self.read_arg(offset);
                self.ip += 2;
            }
            Instruction::Halt => return Ok(false),
        }

        Ok(true)
    }

    /// Parse the operation at the current instruction pointer.
    fn parse_instruction(&self) -> Result<Instruction> {
        let code = self.read(self.ip) % 100;
        let mut arg = 0;
        let mut next_arg = || {
            arg += 1;
            self.parse_argument(arg - 1)
        };
        Ok(match code {
            1 => Instruction::Add(next_arg()?, next_arg()?, next_arg()?),
            2 => Instruction::Mul(next_arg()?, next_arg()?, next_arg()?),
            3 => Instruction::In(next_arg()?),
            4 => Instruction::Out(next_arg()?),
            5 => Instruction::Jeq(next_arg()?, next_arg()?),
            6 => Instruction::Jne(next_arg()?, next_arg()?),
            7 => Instruction::Lt(next_arg()?, next_arg()?, next_arg()?),
            8 => Instruction::Eq(next_arg()?, next_arg()?, next_arg()?),
            9 => Instruction::Base(next_arg()?),
            99 => Instruction::Halt,
            code => return Err(Error::InvalidOpcode { ip: self.ip, code }),
        })
    }

    /// Parse the nth argument of the current instruction.
    fn parse_argument(&self, n: u32) -> Result<Argument> {
        let ip = self.ip;
        let arg = self.read(ip + 1 + n as Int);
        let mode = self.read(ip) / 10i64.pow(2 + n) % 10;
        match mode {
            0 => Ok(Argument::Addr(arg)),
            1 => Ok(Argument::Immediate(arg)),
            2 => Ok(Argument::Relative(arg)),
            _ => Err(Error::InvalidArgMode { ip, n, mode }),
        }
    }

    /// Return the value referenced by the given argument.
    fn read_arg(&self, arg: Argument) -> Int {
        match arg {
            Argument::Addr(addr) => self.read(addr),
            Argument::Immediate(value) => value,
            Argument::Relative(offset) => self.read(self.base + offset),
        }
    }

    /// Write the given value to the memory position referenced by the given
    /// argument.
    fn write_arg(&mut self, arg: Argument, value: Int) -> Result<Int> {
        Ok(match arg {
            Argument::Addr(addr) => self.write(addr, value),
            Argument::Immediate(_) => return Err(Error::WriteImmediate { ip: self.ip }),
            Argument::Relative(offset) => self.write(self.base + offset, value),
        })
    }
}

impl Iterator for IntcodeVm {
    type Item = Result<Int>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_output().transpose()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Instruction {
    Add(Argument, Argument, Argument), // 1
    Mul(Argument, Argument, Argument), // 2
    In(Argument),                      // 3
    Out(Argument),                     // 4
    Jeq(Argument, Argument),           // 5
    Jne(Argument, Argument),           // 6
    Lt(Argument, Argument, Argument),  // 7
    Eq(Argument, Argument, Argument),  // 8
    Base(Argument),                    // 9
    Halt,                              // 99
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Argument {
    Addr(Int),
    Immediate(Int),
    Relative(Int),
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("Invalid opcode {code} at {ip}")]
    InvalidOpcode { ip: Int, code: Int },
    #[error("Cannot write to arg in immediate mode at {ip}")]
    WriteImmediate { ip: Int },
    #[error("Invalid arg mode {mode} at {ip} (arg {n})")]
    InvalidArgMode { ip: Int, n: u32, mode: Int },
    #[error("Executing cannot continue until more input is provided")]
    NeedsInput,
}

pub type Result<T, E = Error> = core::result::Result<T, E>;
