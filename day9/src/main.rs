use std::io::{stdin, BufRead};
use std::ops::{Index, IndexMut};
use std::process::exit;
use std::sync::mpsc::{channel, Receiver, Sender};
use std::time::Duration;
use FetchMode::*;
use OpcodeInstruction::*;
use std::sync::Mutex;
use std::error::Error;

#[derive(Copy, Clone, Debug)]
struct Opcode {
    instruction: OpcodeInstruction,
    mode1: FetchMode,
    mode2: FetchMode,
    mode3: FetchMode,
}

impl From<i64> for Opcode {
    fn from(n: i64) -> Self {
        let s = format!("{:05}", n);
        Opcode {
            instruction: match &s[3..=4] {
                "01" => Add,
                "02" => Multiply,
                "03" => Read,
                "04" => Write,
                "05" => JmpT,
                "06" => JmpF,
                "07" => Lt,
                "08" => Eq,
                "09" => ShiftRelBase,
                "99" => Exit,
                _ => panic!("unknown opcode {}", &s[3..=4]),
            },
            mode1: s[2..=2].into(),
            mode2: s[1..=1].into(),
            mode3: s[0..=0].into(),
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum OpcodeInstruction {
    Add,
    Multiply,
    Read,
    Write,
    JmpT,
    JmpF,
    Lt,
    Eq,
    ShiftRelBase,
    Exit,
}

#[derive(Copy, Clone, Debug)]
enum FetchMode {
    Pointer,
    Value,
    Relative,
}

impl From<&str> for FetchMode {
    fn from(s: &str) -> Self {
        match s {
            "0" => Pointer,
            "1" => Value,
            "2" => Relative,
            _ => panic!("unknown fetch mode: {}", s),
        }
    }
}

#[derive(Copy, Clone, Debug)]
struct FetchInstruction(i64, FetchMode);

#[derive(Debug)]
struct IntcodeInterpreter {
    code: Vec<i64>,
    pc: i64,
    rel_base: i64,
    in_chan: Receiver<i64>,
    out_chan: Sender<i64>,
}

impl IntcodeInterpreter {
    fn new(text: &str, in_chan: Receiver<i64>, out_chan: Sender<i64>) -> Self {
        IntcodeInterpreter {
            code: text
                .split(",")
                .map(|s| s.trim().to_string())
                .filter(|s| !s.is_empty())
                .map(|s| s.parse::<i64>().unwrap())
                .collect(),
            pc: 0,
            rel_base: 0,
            in_chan,
            out_chan,
        }
    }

    fn run(&mut self) {
        loop {
            let operation: Opcode = self.get_exact(self.pc).into();
            if self.execute(operation) {
                break;
            }
        }
    }

    fn execute(&mut self, code: Opcode) -> bool {
        let pc = self.pc;
        let f1 = FetchInstruction(pc + 1, code.mode1);
        let f2 = FetchInstruction(pc + 2, code.mode2);
        let f3 = FetchInstruction(pc + 3, code.mode3);
        match code.instruction {
            Add => {
                let val = self.get(f1) + self.get(f2);
                self.set(f3, val);
                self.pc += 4;
            }
            Multiply => {
                let val = self.get(f1) * self.get(f2);
                self.set(f3, val);
                self.pc += 4;
            }
            Read => {
                let val = self.in_chan.recv().unwrap();
                self.set(f1, val);
                self.pc += 2;
            }
            Write => {
                let val = self.get(f1);
                self.out_chan.send(val);
                self.pc += 2;
            }
            JmpT => {
                if self.get(f1) != 0 {
                    self.pc = self.get(f2);
                } else {
                    self.pc += 3;
                }
            }
            JmpF => {
                if self.get(f1) == 0 {
                    self.pc = self.get(f2);
                } else {
                    self.pc += 3;
                }
            }
            Lt => {
                let val = if self.get(f1) < self.get(f2) { 1 } else { 0 };
                self.set(f3, val);
                self.pc += 4;
            }
            Eq => {
                let val = if self.get(f1) == self.get(f2) { 1 } else { 0 };
                self.set(f3, val);
                self.pc += 4;
            }
            ShiftRelBase => {
                self.rel_base += self.get(f1);
                self.pc += 2;
            }
            Exit => {
                return true;
            }
        }
        false
    }

    fn expand_mem(&mut self, addr: i64) {
        self.code.extend((self.code.len()..=(addr as usize)).map(|_| 0));
    }

    fn get_ins_loc(&mut self, index: FetchInstruction) -> i64 {
        let idx = match index.1 {
            Pointer => {
                let p = self.get_exact(index.0);
                p
            }
            Value => index.0,
            Relative => self.rel_base + self.get_exact(index.0)
        };
        idx
    }

    fn get(&mut self, index: FetchInstruction) -> i64 {
        let loc = self.get_ins_loc(index);
        self.expand_mem(loc);
        self.code[loc as usize]
    }

    fn get_exact(&mut self, index: i64) -> i64 {
        self.expand_mem(index);
        self.code[index as usize]
    }

    fn set(&mut self, index: FetchInstruction, value: i64) {
        let loc = self.get_ins_loc(index);
        self.expand_mem(loc);
        self.code[loc as usize] = value;
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut buff = String::new();
    stdin().read_line(&mut buff);
    let (input, i) = channel();
    let (o, output) = channel();
    let mut interpreter = IntcodeInterpreter::new(buff.as_str(), i, o);

    std::thread::spawn(move || interpreter.run());

    let handle = std::thread::spawn(move || {
        while let Ok(r) = output.recv() {
            println!("{}", r);
        }
    });

    for line in stdin().lock().lines() {
        let value = line?.trim().parse::<i64>()?;
        input.send(value);
    }

    handle.join();

    Ok(())
}
