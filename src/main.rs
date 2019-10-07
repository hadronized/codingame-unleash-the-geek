use std::convert::TryFrom;
use std::fmt;
use std::io;

macro_rules! parse_input {
    ($x:expr, $t:ident) => ($x.trim().parse::<$t>().unwrap())
}

/// Possible items.
enum Item {
  Radar,
  Trap
}

impl TryFrom<u32> for Item {
  type Error = String;

  fn try_from(value: T) -> Result<Self, Self::Error> {
    match value {
      2 => Item::Radar,
    }
  }
}

impl fmt::Display for Item {
  fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
    match *self {
      Item::Radar => f.write_str("RADAR"),
      Item::Trap => f.write_str("RADAR"),
    }
  }
}

/// Possible request.
enum Request {
  Move(u32, u32),
  Wait,
  Dig(u32, u32),
  Item(Item),
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

struct GameState {
  my_score: u32,
  opponent_score: u32,
  cells: [Cell; 30 * 15],
}

/// Describe a single cell on the grid.
struct Cell {
  ore_amount: Option<usize>,
  has_hole: bool
}

fn main() {
  let mut input_line = String::new();
  io::stdin().read_line(&mut input_line).unwrap();
  let inputs = input_line.split(" ").collect::<Vec<_>>();

  let width = parse_input!(inputs[0], u32);
  let height = parse_input!(inputs[1], u32); // size of the map

  // game loop
  loop {
    let mut input_line = String::new();
    io::stdin().read_line(&mut input_line).unwrap();
    let inputs = input_line.split(" ").collect::<Vec<_>>();

    let my_score = parse_input!(inputs[0], u32); // Amount of ore delivered
    let opponent_score = parse_input!(inputs[1], u32);

    for y in 0..height as usize {
      let mut input_line = String::new();
      io::stdin().read_line(&mut input_line).unwrap();
      let inputs = input_line.split_whitespace().collect::<Vec<_>>();

      // we skip x = 0 as it’s HQ
      for _ in 1..width as usize {
        let ore: Option<usize> = inputs[2*y].trim().parse().ok(); // amount of ore or "?" if unknown
        let hole = parse_input!(inputs[2 * y + 1], u32) == 1; // 1 if cell has a hole
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

      let entity_id = parse_input!(inputs[0], u32); // unique id of the entity
      let entity_type = parse_input!(inputs[1], u32); // 0 for your robot, 1 for other robot, 2 for radar, 3 for trap
      let x = parse_input!(inputs[2], i32);
      let y = parse_input!(inputs[3], i32); // position of the entity
      let item = parse_input!(inputs[4], u32); // if this entity is a robot, the item it is carrying (-1 for NONE, 2 for RADAR, 3 for TRAP, 4 for ORE)
    }

    for i in 0..5 as usize {
      println!("WAIT"); // WAIT|MOVE x y|DIG x y|REQUEST item
    }
  }
}
