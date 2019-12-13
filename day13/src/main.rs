use std::collections::HashMap;
use std::error::Error;
use std::io::{stdin, BufRead};
use std::ops::{Index, IndexMut};
use std::process::exit;
use std::sync::mpsc::{channel, Receiver, Sender};
use std::sync::{Arc, Mutex};
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
        self.code
            .extend((self.code.len()..=(addr as usize)).map(|_| 0));
    }

    fn get_ins_loc(&mut self, index: FetchInstruction) -> i64 {
        let idx = match index.1 {
            Pointer => {
                let p = self.get_exact(index.0);
                p
            }
            Value => index.0,
            Relative => self.rel_base + self.get_exact(index.0),
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

    fn set_exact(&mut self, index: i64, value: i64) {
        self.expand_mem(index);
        self.code[index as usize] = value;
    }
}

#[derive(Eq, PartialEq, Copy, Clone)]
enum Tile {
    Empty,
    Wall,
    Block,
    HorizontalPaddle,
    Ball,
}

use Tile::*;

impl From<i64> for Tile {
    fn from(n: i64) -> Self {
        match n {
            0 => Empty,
            1 => Wall,
            2 => Block,
            3 => HorizontalPaddle,
            4 => Ball,
            _ => panic!("unknown tile type"),
        }
    }
}

impl Into<&'static str> for &Tile {
    fn into(self) -> &'static str {
        match *self {
            Empty => "   ",
            Wall => " | ",
            Block => " # ",
            HorizontalPaddle => "---",
            Ball => " O ",
        }
    }
}

fn oart1() -> Result<(), Box<dyn Error>> {
    let mut buff = String::new();
    stdin().read_line(&mut buff);
    let (input, i) = channel();
    let (o, output) = channel();
    let mut interpreter = IntcodeInterpreter::new(buff.as_str(), i, o);

    let handle = std::thread::spawn(move || interpreter.run());

    std::thread::spawn(move || {
        let input = input;
        for line in stdin().lock().lines() {
            let ins = line.unwrap().trim().parse::<i64>().unwrap();
            input.send(ins);
        }
    });

    let mut display = Mutex::new(HashMap::new());
    let mut current_score = Mutex::new(0);

    while let Ok(x) = output.recv() {
        let y = output.recv().unwrap();

        let third = output.recv().unwrap();
        if x == -1 && y == 0 {
            *current_score.lock() = third;
        }

        let tile: Tile = third.into();
        display.insert((x, y), tile);
    }

    println!("{}", display.iter().filter(|(_, t)| **t == Block).count());

    Ok(())
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut buff = String::new();
    stdin().read_line(&mut buff);
    let (input, i) = channel();
    let (o, output) = channel();
    let mut interpreter = IntcodeInterpreter::new(buff.as_str(), i, o);

    let handle = std::thread::spawn(move || {
        interpreter.set_exact(0, 2);
        interpreter.run();
    });

    let mut display = Arc::new(Mutex::new(HashMap::new()));
    let mut current_score = Arc::new(Mutex::new(0));

    let r_display = display.clone();
    let r_current_score = current_score.clone();
    std::thread::spawn(move || {
        while let Ok(x) = output.recv() {
            let y = output.recv().unwrap();

            let third = output.recv().unwrap();
            if x == -1 && y == 0 {
                *r_current_score.lock().unwrap() = third;
                continue;
            }

            let tile: Tile = third.into();
            r_display.lock().unwrap().insert((x, y), tile);
        }
    });

    loop {
        std::thread::sleep(Duration::from_millis(40));

        let display = (*display.lock().unwrap()).clone();
        if display.iter().filter(|(_, t)| **t == Block).count() == 0 {
            println!("score: {}", *current_score.lock().unwrap());
            return Ok(());
        }

        let min_x = match display.iter().map(|((x, _), _)| *x).min() {
            Some(x) => x,
            None => continue,
        };
        let min_x = display.iter().map(|((x, _), _)| *x).min().unwrap();
        let max_x = display.iter().map(|((x, _), _)| *x).max().unwrap();
        let min_y = display.iter().map(|((_, y), _)| *y).min().unwrap();
        let max_y = display.iter().map(|((_, y), _)| *y).max().unwrap();

        for y in min_y..=max_y {
            for x in min_x..=max_x {
                let tile: &'static str = display.get(&(x, y)).unwrap_or(&Empty).into();
                print!("{}", tile);
            }
            println!();
        }
        println!("score: {}", *current_score.lock().unwrap());

        let mut ball_x = display
            .iter()
            .filter(|(_, t)| **t == Ball)
            .map(|((x, _), _)| *x)
            .next()
            .unwrap();
        let mut paddle_x = display
            .iter()
            .filter(|(_, t)| **t == HorizontalPaddle)
            .map(|((x, _), _)| *x)
            .next()
            .unwrap();

        let direction = if paddle_x < ball_x {
            1
        } else if ball_x < paddle_x {
            -1
        } else {
            0
        };

        input.send(direction);
    }

    Ok(())
}
