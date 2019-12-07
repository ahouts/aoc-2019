use permutohedron::Heap;
use std::io::stdin;
use std::ops::{Index, IndexMut};
use std::process::exit;
use std::sync::mpsc::{channel, Receiver, Sender};
use std::time::Duration;
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
    in_chan: Receiver<i32>,
    out_chan: Sender<i32>,
}

impl IntcodeInterpreter {
    fn new(text: &str, in_chan: Receiver<i32>, out_chan: Sender<i32>) -> Self {
        IntcodeInterpreter {
            code: text
                .split(",")
                .map(|s| s.trim().to_string())
                .filter(|s| !s.is_empty())
                .map(|s| s.parse::<i32>().unwrap())
                .collect(),
            pc: 0,
            in_chan,
            out_chan,
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
            Read => {
                self[f1] = self.in_chan.recv().unwrap();
                self.pc += 2;
            }
            Write => {
                self.out_chan.send(self[f1]);
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
            Exit => {
                return true;
            }
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

fn spawn_interpreters<'a>(
    s: &rayon::Scope<'a>,
    code: &'a str,
    phases: &'a [i32],
) -> (Sender<i32>, Receiver<i32>) {
    let (input, i0) = channel();
    input.send(phases[0]);
    let (o1, i1) = channel();
    o1.send(phases[1]);
    let (o2, i2) = channel();
    o2.send(phases[2]);
    let (o3, i3) = channel();
    o3.send(phases[3]);
    let (o4, i4) = channel();
    o4.send(phases[4]);
    let (o5, output) = channel();

    s.spawn(move |_| IntcodeInterpreter::new(code, i0, o1).run());
    s.spawn(move |_| IntcodeInterpreter::new(code, i1, o2).run());
    s.spawn(move |_| IntcodeInterpreter::new(code, i2, o3).run());
    s.spawn(move |_| IntcodeInterpreter::new(code, i3, o4).run());
    s.spawn(move |_| IntcodeInterpreter::new(code, i4, o5).run());

    (input, output)
}

fn part1(code: &str) {
    let mut amps = vec![0, 1, 2, 3, 4];

    let mut max_val = 0;

    for perm in Heap::new(&mut amps) {
        rayon::scope(|s| {
            let (input, output) = spawn_interpreters(s, code, perm.as_slice());

            input.send(0);
            let out = output.recv().unwrap();

            if out > max_val {
                max_val = out;
            }
        });
    }

    println!("part 1: {}", max_val);
}

fn part2(code: &str) {
    let mut amps = vec![5, 6, 7, 8, 9];

    let mut max_val = 0;

    for perm in Heap::new(&mut amps) {
        rayon::scope(|s| {
            let (input, output) = spawn_interpreters(s, code, perm.as_slice());

            input.send(0);

            let mut out = std::i32::MIN;
            while let Ok(res) = output.recv() {
                out = res;
                input.send(res);
            }

            if out > max_val {
                max_val = out;
            }
        });
    }

    println!("part 2: {}", max_val);
}

fn main() {
    let mut buff = String::new();
    stdin().read_line(&mut buff);

    part1(&buff);
    part2(&buff);
}
