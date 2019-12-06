use std::collections::HashMap;
use std::error::Error;
use std::io::{stdin, BufRead};

struct Arena {
    id_map: HashMap<String, usize>,
    data: Vec<Option<usize>>,
}

impl Arena {
    fn new() -> Self {
        Arena {
            id_map: vec![("COM".to_string(), 0)].into_iter().collect(),
            data: vec![None],
        }
    }

    fn add(&mut self, parent: &str, child: &str) {
        let pid = self.add_or_get_id(parent);
        let cid = self.add_or_get_id(child);
        self.data[cid] = Some(pid);
    }

    fn add_or_get_id(&mut self, name: &str) -> usize {
        match self.id_map.get(name) {
            Some(id) => *id,
            None => {
                let id = self.data.len();
                self.id_map.insert(name.to_string(), id);
                self.data.push(None);
                id
            }
        }
    }

    fn total_orbits(&self) -> usize {
        (0..self.data.len()).map(|id| self.get_depth(id)).sum()
    }

    fn get_depth(&self, id: usize) -> usize {
        match self.data[id] {
            None => 0,
            Some(pid) => 1 + self.get_depth(pid),
        }
    }

    fn path_to_com(&self, id: usize) -> Vec<usize> {
        match self.data[id] {
            None => vec![id],
            Some(pid) => {
                let mut v = self.path_to_com(pid);
                v.push(id);
                return v;
            }
        }
    }

    fn min_transfers(&self, src: &str, dst: &str) -> usize {
        let src_id = *self.id_map.get(src).expect("unknown src");
        let mut src_path = self.path_to_com(src_id);

        let dst_id = *self.id_map.get(dst).expect("unknown dst");
        let dst_path = self.path_to_com(dst_id);

        let mut distance = 0;
        while !dst_path.contains(&src_path.last().unwrap()) {
            src_path.pop();
            distance += 1;
        }

        let common_planet = *src_path.last().unwrap();

        distance
            + dst_path
                .into_iter()
                .rev()
                .take_while(|id| *id != common_planet)
                .count()
            - 2
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut arena = Arena::new();

    for maybe_line in stdin().lock().lines() {
        let line = maybe_line?;
        let parts: Vec<_> = line.trim().split(")").collect();
        let parent = parts[0];
        let child = parts[1];
        arena.add(parent, child);
    }

    println!(
        "number of direct and indirect orbits: {}",
        arena.total_orbits()
    );
    println!("min transfers: {}", arena.min_transfers("YOU", "SAN"));

    Ok(())
}
