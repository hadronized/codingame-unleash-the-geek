use std::collections::HashMap;
use std::convert::{TryFrom, TryInto as _};
use std::fmt;
use std::io;

const WIDTH: usize = 30;
const HEIGHT: usize = 15;

macro_rules! parse_input {
    ($x:expr, $t:ident) => ($x.trim().parse::<$t>().unwrap())
}

/// Entity types.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum EntityType {
  Miner,
  OpponentMiner,
  BurriedRadar,
  BurriedTrap,
}

impl TryFrom<u32> for EntityType {
  type Error = String;

  fn try_from(value: u32) -> Result<Self, Self::Error> {
    match value {
      0 => Ok(EntityType::Miner),
      1 => Ok(EntityType::OpponentMiner),
      2 => Ok(EntityType::BurriedRadar),
      3 => Ok(EntityType::BurriedTrap),
      _ => Err(format!("unknown entity type: {}", value)),
    }
  }
}

/// Possible items a miner can hold.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum Item {
  Radar,
  Trap,
  Ore
}

impl TryFrom<u32> for Item {
  type Error = String;

  fn try_from(value: u32) -> Result<Self, Self::Error> {
    match value {
      2 => Ok(Item::Radar),
      3 => Ok(Item::Trap),
      4 => Ok(Item::Ore),
      _ => Err(format!("unknown item: {}", value)),
    }
  }
}

/// Possible items a miner can request.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum RequestItem {
  Radar,
  Trap
}

impl fmt::Display for RequestItem {
  fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
    match *self {
      RequestItem::Radar => f.write_str("RADAR"),
      RequestItem::Trap => f.write_str("TRAP"),
    }
  }
}

/// Possible request.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum Request {
  Move(u32, u32),
  Wait,
  Dig(u32, u32),
  Item(RequestItem),
}

impl fmt::Display for Request {
  fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
    match *self {
      Request::Move(x, y) => write!(f, "MOVE {} {}", x, y),
      Request::Wait => f.write_str("WAIT"),
      Request::Dig(x, y) => write!(f, "DIG {} {}", x, y),
      Request::Item(ref item) => write!(f, "REQUEST {}", item),
    }
  }
}

/// A request with a possible associated comment.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct OutputRequest {
  req: Request,
  comment: Option<String>
}

impl OutputRequest {
  fn new<C>(req: Request, comment: C) -> Self where C: Into<Option<String>> {
    OutputRequest {
      req,
      comment: comment.into()
    }
  }
}

impl From<Request> for OutputRequest {
  fn from(req: Request) -> Self {
    OutputRequest::new(req, None)
  }
}

impl fmt::Display for OutputRequest {
  fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
    write!(f, "{}", self.req)?;

    if let Some(ref comment) = self.comment {
      write!(f, " {}", comment)
    } else {
      Ok(())
    }
  }
}

/// Unique ID for all entities in the game.
type UID = u32;

#[derive(Debug)]
struct GameState {
  my_score: u32,
  opponent_score: u32,
  cells: Vec<Cell>,
  miners: Vec<Miner>,
  opponent_miners: Vec<Miner>,
  entities: HashMap<UID, EntityType>,
}

impl Default for GameState {
  fn default() -> Self {
    GameState {
      my_score: 0,
      opponent_score: 0,
      cells: vec![Cell::default(); WIDTH * HEIGHT],
      miners: Vec::new(),
      opponent_miners: Vec::new(),
      entities: HashMap::new(),
    }
  }
}

impl GameState {
  fn set_my_score(&mut self, score: u32) {
    self.my_score = score;
  }

  fn set_opponent_score(&mut self, score: u32) {
    self.opponent_score = score;
  }

  fn set_ore(&mut self, x: usize, y: usize, ore_amount: Option<usize>) {
    self.cells[y * WIDTH + x].ore_amount = ore_amount;
  }

  fn set_hole(&mut self, x: usize, y: usize, hole: bool) {
    self.cells[y * WIDTH + x].has_hole = hole;
  }

  fn add_miner(&mut self, miner: Miner) {
    self.miners.push(miner);
  }

  fn add_opponent_miner(&mut self, miner: Miner) {
    self.opponent_miners.push(miner);
  }
}

/// Describe a single cell on the grid.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct Cell {
  ore_amount: Option<usize>,
  has_hole: bool
}

impl Default for Cell {
  fn default() -> Self {
    Cell {
      ore_amount: None,
      has_hole: false,
    }
  }
}

#[derive(Debug, Eq, Hash, PartialEq)]
enum Miner {
  Alive {
    x: u32,
    y: u32,
    item: Option<Item>,
    uid: UID,
  },

  Dead
}

fn main() {
  let mut input_line = String::new();
  io::stdin().read_line(&mut input_line).unwrap();
  let inputs = input_line.split(" ").collect::<Vec<_>>();

  let width = parse_input!(inputs[0], u32);
  let height = parse_input!(inputs[1], u32); // size of the map

  let mut game_state = GameState::default();

  // game loop
  loop {
    let mut input_line = String::new();
    io::stdin().read_line(&mut input_line).unwrap();
    let inputs = input_line.split(" ").collect::<Vec<_>>();

    let my_score = parse_input!(inputs[0], u32); // Amount of ore delivered
    let opponent_score = parse_input!(inputs[1], u32);

    game_state.set_my_score(my_score);
    game_state.set_opponent_score(opponent_score);

    for y in 0..height as usize {
      let mut input_line = String::new();
      io::stdin().read_line(&mut input_line).unwrap();
      let inputs = input_line.split_whitespace().collect::<Vec<_>>();

      // we skip x = 0 as it’s HQ
      for x in 1..width as usize {
        let ore: Option<usize> = inputs[2*y].trim().parse().ok(); // amount of ore or "?" if unknown
        let hole = parse_input!(inputs[2 * y + 1], u32) == 1; // 1 if cell has a hole

        game_state.set_ore(x, y, ore);
        game_state.set_hole(x, y, hole);
      }
    }

    let mut input_line = String::new();
    io::stdin().read_line(&mut input_line).unwrap();
    let inputs = input_line.split(" ").collect::<Vec<_>>();

    let entity_count = parse_input!(inputs[0], u32); // number of entities visible to you
    let radar_cooldown = parse_input!(inputs[1], u32); // turns left until a new radar can be requested
    let trap_cooldown = parse_input!(inputs[2], u32); // turns left until a new trap can be requested

    for i in 0..entity_count as usize {
      let mut input_line = String::new();
      io::stdin().read_line(&mut input_line).unwrap();
      let inputs = input_line.split(" ").collect::<Vec<_>>();

      let uid = parse_input!(inputs[0], u32); // unique id of the entity
      let entity_type: EntityType = parse_input!(inputs[1], u32).try_into().unwrap();

      let x = parse_input!(inputs[2], i32);
      let y = parse_input!(inputs[3], i32); // position of the entity
      let item = parse_input!(inputs[4], u32).try_into().ok(); // if this entity is a robot, the item it is carrying (-1 for NONE, 2 for RADAR, 3 for TRAP, 4 for ORE)

      // check if we need to update our entities
      if !game_state.entities.contains_key(&uid) {
        // we don’t know that entity yet, add it
        game_state.entities.insert(uid, entity_type);

        // if it’s a miner, add it to the list of miner
        match entity_type {
          EntityType::Miner => {
            game_state.add_miner(Miner::Alive {
              x: x as u32,
              y: y as u32,
              item,
              uid
            });
          }

          EntityType::OpponentMiner => {
            game_state.add_opponent_miner(Miner::Alive {
              x: x as u32,
              y: y as u32,
              item,
              uid
            });
          }

          _ => ()
        }
      }
    }

    for i in 0..5 as usize {
      println!("WAIT"); // WAIT|MOVE x y|DIG x y|REQUEST item
    }
  }
}
