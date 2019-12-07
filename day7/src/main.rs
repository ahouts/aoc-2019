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
    exit_chan: Option<Sender<i32>>,
    last_sent: i32,
}

impl IntcodeInterpreter {
    fn new(
        text: &str,
        in_chan: Receiver<i32>,
        out_chan: Sender<i32>,
        exit_chan: Option<Sender<i32>>,
    ) -> Self {
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
            exit_chan,
            last_sent: std::i32::MIN,
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
                self.last_sent = self[f1];
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
                if let Some(exit_chan) = &self.exit_chan {
                    exit_chan.send(self.last_sent);
                }
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

// part 1
//fn main() {
//    let mut buff = String::new();
//    stdin().read_line(&mut buff);
//
//    let mut amps = vec![0, 1, 2, 3, 4];
//
//    let mut max_val = 0;
//    let mut max_perm = vec![];
//
//    for perm in Heap::new(&mut amps) {
//        let buff1 = buff.clone();
//        let buff2 = buff.clone();
//        let buff3 = buff.clone();
//        let buff4 = buff.clone();
//        let buff5 = buff.clone();
//        let (input, i0) = channel();
//        input.send(perm[0]);
//        let (o1, i1) = channel();
//        o1.send(perm[1]);
//        let (o2, i2) = channel();
//        o2.send(perm[2]);
//        let (o3, i3) = channel();
//        o3.send(perm[3]);
//        let (o4, i4) = channel();
//        o4.send(perm[4]);
//        let (o5, output) = channel();
//        std::thread::spawn(move || IntcodeInterpreter::new(buff1.as_str(), i0, o1).run());
//        std::thread::spawn(move || IntcodeInterpreter::new(buff2.as_str(), i1, o2).run());
//        std::thread::spawn(move || IntcodeInterpreter::new(buff3.as_str(), i2, o3).run());
//        std::thread::spawn(move || IntcodeInterpreter::new(buff4.as_str(), i3, o4).run());
//        std::thread::spawn(move || IntcodeInterpreter::new(buff5.as_str(), i4, o5).run());
//
//        input.send(0);
//        let out = output.recv().unwrap();
//
//        if out > max_val {
//            max_val = out;
//            max_perm = perm.clone();
//        }
//    }
//
//    println!("{:?} : {}", max_perm, max_val);
//}
fn main() {
    let mut buff = String::new();
    stdin().read_line(&mut buff);

    let mut amps = vec![5, 6, 7, 8, 9];

    let mut max_val = 0;
    let mut max_perm = vec![];

    for perm in Heap::new(&mut amps) {
        let buff1 = buff.clone();
        let buff2 = buff.clone();
        let buff3 = buff.clone();
        let buff4 = buff.clone();
        let buff5 = buff.clone();
        let (input, i0) = channel();
        input.send(perm[0]);
        let input2 = input.clone();
        let (o1, i1) = channel();
        o1.send(perm[1]);
        let (o2, i2) = channel();
        o2.send(perm[2]);
        let (o3, i3) = channel();
        o3.send(perm[3]);
        let (o4, i4) = channel();
        o4.send(perm[4]);
        let (o5, output) = channel();

        std::thread::spawn(move || IntcodeInterpreter::new(buff1.as_str(), i0, o1, None).run());
        std::thread::spawn(move || IntcodeInterpreter::new(buff2.as_str(), i1, o2, None).run());
        std::thread::spawn(move || IntcodeInterpreter::new(buff3.as_str(), i2, o3, None).run());
        std::thread::spawn(move || IntcodeInterpreter::new(buff4.as_str(), i3, o4, None).run());
        std::thread::spawn(move || {
            IntcodeInterpreter::new(buff5.as_str(), i4, input, Some(o5)).run()
        });

        input2.send(0);

        let out = output.recv().unwrap();
        if out > max_val {
            max_val = out;
            max_perm = perm.clone();
        }
    }

    println!("{:?} : {}", max_perm, max_val);
}
