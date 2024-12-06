use std::{
    collections::HashSet,
    fs,
    ops::{Add, Index, IndexMut},
    sync::mpsc,
    thread,
};

#[derive(Debug, Copy, Clone)]
enum Direction {
    Up,
    Left,
    Right,
    Down,
}

#[derive(Debug, Copy, Clone)]
enum RawTile {
    Empty,
    Obstacle,
    Guard(Direction),
}

#[derive(Debug, Copy, Clone)]
enum Tile {
    Empty,
    Obstacle,
}

impl Default for Tile {
    fn default() -> Self {
        Tile::Empty
    }
}

#[derive(Debug, Default, Hash, PartialEq, Eq, Copy, Clone)]
struct Vector {
    x: u8,
    y: u8,
}

impl Add<Vector> for Vector {
    type Output = Vector;
    fn add(self, rhs: Vector) -> Self::Output {
        Self {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

#[derive(Debug, Copy, Clone)]
struct Guard {
    coords: Vector,
    direction: Direction,
}

impl Guard {
    fn can_move(&self, rhs: Direction) -> Option<Vector> {
        let coords = self.coords;
        match rhs {
            Direction::Up => {
                if coords.y == 0 as u8 {
                    None
                } else {
                    Some(Vector {
                        x: coords.x,
                        y: coords.y - 1,
                    })
                }
            }

            Direction::Right => {
                if coords.x >= (SIZE - 1) as u8 {
                    None
                } else {
                    Some(Vector {
                        x: coords.x + 1,
                        y: coords.y,
                    })
                }
            }
            Direction::Down => {
                if coords.y >= (SIZE - 1) as u8 {
                    None
                } else {
                    Some(Vector {
                        x: coords.x,
                        y: coords.y + 1,
                    })
                }
            }
            Direction::Left => {
                if coords.x == 0 {
                    None
                } else {
                    Some(Vector {
                        x: coords.x - 1,
                        y: coords.y,
                    })
                }
            }
        }
    }
    fn rotate(&mut self) {
        self.direction = match self.direction {
            Direction::Up => Direction::Right,
            Direction::Right => Direction::Down,
            Direction::Down => Direction::Left,
            Direction::Left => Direction::Up,
        };
    }
    fn go(&mut self, coord: Vector) {
        self.coords = coord;
    }
}

const SIZE: usize = 130;
const FILE_NAME: &'static str = "input.txt";
type Grid = [[Tile; SIZE]; SIZE];

impl IndexMut<Vector> for Grid {
    #[inline]
    fn index_mut(&mut self, index: Vector) -> &mut Self::Output {
        &mut self.as_mut_slice()[index.y as usize][index.x as usize]
    }
}

impl Index<Vector> for Grid {
    type Output = Tile;
    #[inline]
    fn index(&self, index: Vector) -> &Self::Output {
        &self.as_slice()[index.y as usize][index.x as usize]
    }
}

fn parse_tile(tile: &u8) -> RawTile {
    match tile {
        &b'#' => RawTile::Obstacle,
        &b'^' => RawTile::Guard(Direction::Up),
        &b'>' => RawTile::Guard(Direction::Right),
        &b'<' => RawTile::Guard(Direction::Left),
        &b'v' => RawTile::Guard(Direction::Down),
        _ => RawTile::Empty,
    }
}

fn read_into(file: &str, grid: &mut Grid) -> Guard {
    let input = fs::read(file).expect("Invalid file name");
    let mut guard: Option<Guard> = None;
    for (y, line) in input.split(|ch| *ch == b'\n').enumerate() {
        for (x, char) in line.into_iter().enumerate() {
            let coords = Vector {
                x: x as u8,
                y: y as u8,
            };
            match parse_tile(char) {
                RawTile::Guard(direction) => {
                    guard = Some(Guard { coords, direction });
                    grid[coords] = Tile::Empty;
                }
                RawTile::Empty => {
                    grid[coords] = Tile::Empty;
                }
                RawTile::Obstacle => {
                    grid[coords] = Tile::Obstacle;
                }
            }
        }
    }
    guard.unwrap()
}

#[derive(Debug)]
enum MoveResult {
    Okay(Vector),
    Wall(Vector),
    Out,
}

fn peek_next(grid: &Grid, guard: &Guard) -> MoveResult {
    match guard.can_move(guard.direction) {
        None => MoveResult::Out,
        Some(next) => match grid[next] {
            Tile::Empty => MoveResult::Okay(next),
            Tile::Obstacle => MoveResult::Wall(next),
        },
    }
}

#[derive(Debug)]
enum Solution {
    Loop(Vector),
    Exit { visited: u32 },
}

fn solve(grid: &Grid, guard: &mut Guard) -> (Solution, HashSet<Vector>) {
    let mut seen = HashSet::<Vector>::new();
    let mut walls = HashSet::<(Vector, Vector)>::new();
    seen.insert(guard.coords);
    loop {
        match peek_next(&grid, &guard) {
            MoveResult::Wall(coord) => {
                guard.rotate();
                if !walls.insert((coord, guard.coords)) {
                    return (Solution::Loop(coord), seen);
                }
            }
            MoveResult::Okay(coord) => {
                guard.go(coord);
                seen.insert(coord);
            }
            MoveResult::Out => {
                return (
                    Solution::Exit {
                        visited: seen.len() as u32,
                    },
                    seen,
                );
            }
        }
    }
}

fn main() {
    let start = std::time::Instant::now();
    let mut grid: Grid = [[Tile::Empty; SIZE]; SIZE];
    let mut guard = read_into(FILE_NAME, &mut grid);
    let (solved, seen) = solve(&grid, &mut guard);
    let (tx, rx) = mpsc::channel();
    for loc in seen {
        let sender = tx.clone();
        thread::spawn(move || {
            let mut grid2 = grid.clone();
            let mut guard2 = read_into(FILE_NAME, &mut grid2);
            grid2[loc] = Tile::Obstacle;
            let (solution, _) = solve(&grid2, &mut guard2);
            if let Solution::Loop(_) = solution {
                if let Err(err) = sender.send(1) {
                    panic!("{}", err)
                }
            }
        });
    }

    match solved {
        Solution::Exit { visited } => println!("part1 = {:?}", visited),
        _ => panic!("The solution looped?"),
    }
    let mut total = 0;
    loop {
        if let Err(err) = rx.recv() {
            panic!("{}", err)
        }
        total += 1;
        println!(
            "part2 = {total} time = {}",
            (std::time::Instant::now() - start).as_micros()
        );
    }
}
