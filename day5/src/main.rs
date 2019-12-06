use std::io::stdin;
use std::ops::{Index, IndexMut};
use FetchMode::*;
use OpcodeInstruction::*;

#[derive(Copy, Clone, Debug)]
struct Opcode {
    instruction: OpcodeInstruction,
    mode1: FetchMode,
    mode2: FetchMode,
    mode3: FetchMode,
}

impl From<i32> for Opcode {
    fn from(n: i32) -> Self {
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
    Exit,
}

#[derive(Copy, Clone, Debug)]
enum FetchMode {
    Pointer,
    Value,
}

impl From<&str> for FetchMode {
    fn from(s: &str) -> Self {
        match s {
            "0" => Pointer,
            "1" => Value,
            _ => panic!("unknown fetch mode: {}", s),
        }
    }
}

#[derive(Copy, Clone, Debug)]
struct FetchInstruction(i32, FetchMode);

#[derive(Debug)]
struct IntcodeInterpreter {
    code: Vec<i32>,
    pc: i32,
}

impl IntcodeInterpreter {
    fn new(text: &str) -> Self {
        IntcodeInterpreter {
            code: text
                .split(",")
                .map(|s| s.trim().to_string())
                .filter(|s| !s.is_empty())
                .map(|s| s.parse::<i32>().unwrap())
                .collect(),
            pc: 0,
        }
    }

    fn run(&mut self) {
        loop {
            let operation: Opcode = self.code[self.pc as usize].into();
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
                self[f3] = self[f1] + self[f2];
                self.pc += 4;
            }
            Multiply => {
                self[f3] = self[f1] * self[f2];
                self.pc += 4;
            }
            Read => loop {
                let mut buff = String::new();
                stdin().read_line(&mut buff);
                match buff.trim().parse::<i32>() {
                    Ok(n) => {
                        self[f1] = n;
                        self.pc += 2;
                        break;
                    }
                    Err(_) => (),
                }
            },
            Write => {
                let resp_code = self[f1];
                println!("{}", resp_code);
                if resp_code != 0 {
                    println!("core dump: {:?}", self);
                    println!("head: {:?}", &self.code[(self.pc as usize)..])
                }
                self.pc += 2;
            }
            JmpT => {
                if self[f1] != 0 {
                    self.pc = self[f2];
                } else {
                    self.pc += 3;
                }
            }
            JmpF => {
                if self[f1] == 0 {
                    self.pc = self[f2];
                } else {
                    self.pc += 3;
                }
            }
            Lt => {
                self[f3] = if self[f1] < self[f2] { 1 } else { 0 };
                self.pc += 4;
            }
            Eq => {
                self[f3] = if self[f1] == self[f2] { 1 } else { 0 };
                self.pc += 4;
            }
            Exit => return true,
        }
        false
    }
}

impl Index<FetchInstruction> for IntcodeInterpreter {
    type Output = i32;

    fn index(&self, index: FetchInstruction) -> &Self::Output {
        match index.1 {
            Pointer => {
                let p = self.code[index.0 as usize];
                &self.code[p as usize]
            }
            Value => &self.code[index.0 as usize],
        }
    }
}

impl IndexMut<FetchInstruction> for IntcodeInterpreter {
    fn index_mut(&mut self, index: FetchInstruction) -> &mut Self::Output {
        match index.1 {
            Pointer => {
                let p = self.code[index.0 as usize];
                &mut self.code[p as usize]
            }
            Value => &mut self.code[index.0 as usize],
        }
    }
}

fn main() {
    let mut buff = String::new();
    stdin().read_line(&mut buff);
    let mut interpreter = IntcodeInterpreter::new(buff.as_str());
    interpreter.run();
}
