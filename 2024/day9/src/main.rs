use std::{fs::read, ops::RangeInclusive};

const BASE: u8 = 48;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct File(u16);

impl File {
    const EMPTY: u16 = 0;
    const fn empty() -> Self {
        Self(Self::EMPTY)
    }
    const fn new(value: u16) -> Self {
        Self(value + 1)
    }
    fn get(&self) -> Option<u16> {
        match self {
            File(Self::EMPTY) => None,
            File(n) => Some(n - 1),
        }
    }
    fn is_empty(&self) -> bool {
        self.0 == Self::EMPTY
    }
    fn is(&self, n: u16) -> bool {
        self.get().map_or(false, |v| v == n)
    }
}

fn write_range<T: Copy>(buffer: &mut [T], start: usize, count: usize, elem: T) {
    for n in start..(start + count) {
        buffer[n] = elem;
    }
}

fn read_disk(path: &str) -> (Vec<File>, Free) {
    let bytes = match read(path) {
        Err(err) => panic!("{}", err),
        Ok(results) => results,
    };
    let capacity: usize = bytes.iter().fold(0, |acc, e| acc + (e - BASE) as usize);
    let mut tape = vec![File::empty(); capacity];
    let max_length = bytes.len();
    let mut i: usize = 0;
    let mut tape_ix: usize = 0;
    let mut id = 0;
    let mut free_space: Option<Free> = None;

    while i < max_length {
        let head = (bytes[i] - BASE) as usize;
        let empty = if i + 1 >= max_length {
            0
        } else {
            bytes[i + 1] - BASE
        };
        if empty > 0 && free_space.is_none() {
            free_space = Some(Free {
                offset: head,
                count: empty as u32,
            })
        }
        write_range(&mut tape[..], tape_ix, head, File::new(id));
        tape_ix += head + empty as usize;
        id += 1;
        i += 2;
    }
    (tape, free_space.unwrap())
}

fn chase_last_occupied(buffer: &[File], i: &mut usize) {
    if !buffer[*i].is_empty() {
        return;
    }
    for a in (0..*i).rev() {
        if !buffer[a].is_empty() {
            *i = a;
            return;
        }
    }
}

fn checksum(buffer: &[File]) -> u64 {
    buffer
        .iter()
        .enumerate()
        .map(|(i, file)| match file.get() {
            None => 0 as u64,
            Some(n) => i as u64 * n as u64,
        })
        .sum()
}

#[derive(Debug)]
struct Free {
    offset: usize,
    count: u32,
}

#[derive(Debug)]
struct FreeManager {
    holes: Vec<Free>,
}

impl FreeManager {
    fn from_tape(tape: &[File]) -> Self {
        let mut i = 0;
        let mut holes: Vec<Free> = Vec::with_capacity(tape.len() / 10);
        loop {
            let f = Free::new(i, &tape);
            let next_i = f.offset + f.count as usize;
            if next_i == i {
                break;
            }
            i = next_i;
            holes.push(f);
        }
        Self { holes }
    }
    fn swap(&mut self, tape: &mut [File], range: &RangeInclusive<usize>) -> bool {
        let target_distance = range.end() + 1 - range.start();
        let maybe_hole = self
            .holes
            .iter_mut()
            .enumerate()
            .find(|(_, hole)| hole.count >= target_distance as u32);
        let (i, hole) = match maybe_hole {
            None => return false,
            Some(hole) => hole,
        };
        if hole.offset > *range.start() {
            return false;
        }
        let diff = range.start() - hole.offset;
        for n in hole.offset..(hole.offset + target_distance) {
            tape.swap(n, n + diff);
        }
        if hole.move_by(target_distance) {
            self.holes.remove(i);
        }
        return true;
    }
}

impl Free {
    fn new(offset: usize, buffer: &[File]) -> Self {
        let mut s = Self { offset, count: 0 };
        s.chase_first_free(buffer);
        s
    }
    fn move_by(&mut self, i: usize) -> bool {
        self.offset += i;
        self.count -= i as u32;
        return self.exhausted();
    }
    fn exhausted(&self) -> bool {
        self.count == 0
    }
    fn chase_first_free(&mut self, buffer: &[File]) {
        let size = buffer.len();
        let mut found = false;
        let mut count: u32 = 0;
        for n in self.offset..size {
            if buffer[n].is_empty() {
                count += 1;
                if !found {
                    found = true;
                    self.offset = n;
                }
            } else if found {
                break;
            }
        }
        self.count = count;
    }
}

fn from_current_index(buffer: &[File], i: usize) -> RangeInclusive<usize> {
    let value = buffer[i].get().unwrap();
    let mut pointer = i;
    while pointer > 0 && buffer[pointer].is(value) {
        pointer -= 1;
    }
    (pointer + 1)..=i
}

fn chase_last_occupied_range(buffer: &[File], i: usize) -> Option<RangeInclusive<usize>> {
    let mut pointer = i;
    while buffer[pointer].is_empty() {
        if pointer == 0 {
            return None;
        }
        pointer -= 1;
    }
    Some(from_current_index(&buffer, pointer))
}

fn part1(tape: &mut [File], free: &mut Free) {
    let mut dec: usize = tape.len() - 1;
    while dec > free.offset {
        chase_last_occupied(&tape, &mut dec);
        tape.swap(free.offset, dec);
        free.move_by(1);
        if free.exhausted() {
            free.chase_first_free(&tape);
        }
    }
    println!("{}", checksum(&tape));
}

fn part2(tape: &mut [File]) {
    let mut dec: usize = tape.len() - 1;
    let mut fm = FreeManager::from_tape(&tape);
    while dec > 0 {
        let last = chase_last_occupied_range(&tape, dec).unwrap();
        fm.swap(tape, &last);
        let diff = last.start() - 1;
        if diff > dec {
            break;
        }
        dec = diff;
    }
    println!("{}", checksum(&tape));
}

fn main() {
    let (mut tape, mut free) = read_disk("input-test.txt");
    part1(&mut tape.clone(), &mut free);
    part2(&mut tape);
}
