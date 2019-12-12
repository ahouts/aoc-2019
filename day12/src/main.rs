use std::cmp::Ordering::*;
use std::collections::{HashSet, HashMap};

#[derive(Debug, Clone)]
struct Vec3 { x: i64, y: i64, z: i64 }

#[derive(Debug, Clone)]
struct D {
    pos: Vec3,
    vel: Vec3,
}

impl D {
    fn apply_velocity(&mut self) {
        self.pos.x += self.vel.x;
        self.pos.y += self.vel.y;
        self.pos.z += self.vel.z;
    }

    fn apply_gravity(&mut self, other: &Self) {
        match self.pos.x.cmp(&other.pos.x) {
            Less => self.vel.x += 1,
            Equal => (),
            Greater => self.vel.x -= 1,
        }
        match self.pos.y.cmp(&other.pos.y) {
            Less => self.vel.y += 1,
            Equal => (),
            Greater => self.vel.y -= 1,
        }
        match self.pos.z.cmp(&other.pos.z) {
            Less => self.vel.z += 1,
            Equal => (),
            Greater => self.vel.z -= 1,
        }
    }

    fn energy(&self) -> i64 {
        (self.pos.x.abs() + self.pos.y.abs() + self.pos.z.abs())
        *
        (self.vel.x.abs() + self.vel.y.abs() + self.vel.z.abs())
    }
}

fn part1() {
    let mut planets = vec![
        D { pos: Vec3 { x: 3, y: -6, z: 6 }, vel: Vec3 { x: 0, y: 0, z: 0 } },
        D { pos: Vec3 { x: 10, y: 7, z: -9 }, vel: Vec3 { x: 0, y: 0, z: 0 } },
        D { pos: Vec3 { x: -3, y: -7, z: 9 }, vel: Vec3 { x: 0, y: 0, z: 0 } },
        D { pos: Vec3 { x: -8, y: 0, z: 4 }, vel: Vec3 { x: 0, y: 0, z: 0 } }
    ];

    for _ in 0..1000 {
        for a in 0..4 {
            for b in 0..4 {
                let mut tmp = planets[a].clone();
                tmp.apply_gravity(&planets[b]);
                planets[a] = tmp;
            }
        }

        for planet in planets.iter_mut() {
            planet.apply_velocity();
        }
    }

    println!("{}", planets.into_iter().map(|p| p.energy()).sum::<i64>());
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct D2 {
    pos: i64,
    vel: i64,
}

impl D2 {
    fn apply_velocity(&mut self) {
        self.pos += self.vel;
    }

    fn apply_gravity(&mut self, other: &Self) {
        match self.pos.cmp(&other.pos) {
            Less => self.vel += 1,
            Equal => (),
            Greater => self.vel -= 1,
        }
    }
}

fn resolve_dimension(mut dim: Vec<D2>) -> (u64) {
    let mut history = HashMap::new();
    let mut n_cycles: u64 = 0;
    loop {
        if let Some(p) = history.get(&dim) {
            return n_cycles - *p;
        }

        history.insert(dim.clone(), n_cycles);

        for a in 0..4 {
            for b in 0..4 {
                let mut tmp = dim[a].clone();
                tmp.apply_gravity(&dim[b]);
                dim[a] = tmp;
            }
        }

        for planet in dim.iter_mut() {
            planet.apply_velocity();
        }

        n_cycles += 1;
    }
}

fn main() {
    let mut x = vec![
        D2 { pos: 3, vel: 0 },
        D2 { pos: 10, vel: 0 },
        D2 { pos: -3, vel: 0 },
        D2 { pos: -8, vel: 0 }
    ];
    let mut y = vec![
        D2 { pos: -6, vel: 0 },
        D2 { pos: 7, vel: 0 },
        D2 { pos: -7, vel: 0 },
        D2 { pos: 0, vel: 0 }
    ];
    let mut z = vec![
        D2 { pos: 6, vel: 0 },
        D2 { pos: -9, vel: 0 },
        D2 { pos: 9, vel: 0 },
        D2 { pos: 4, vel: 0 }
    ];

    let mut x_cycle_length = resolve_dimension(x);
    let mut y_cycle_length = resolve_dimension(y);
    let mut z_cycle_length = resolve_dimension(z);

    let mut guess = x_cycle_length;
    loop {
        if guess % y_cycle_length == 0 && guess % z_cycle_length == 0 {
            println!("{}", guess);
            return;
        }
        guess += x_cycle_length;
    }
}
