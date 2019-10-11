use rand::{Rng, thread_rng};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::collections::btree_map::Entry;
use std::fmt;
use std::io;

/// Threshold of amount of ore we want to have under radars. If we exceed that value, we stop
/// burrying radars.
const MIN_ORE_NO_RADAR_NEEDED: usize = 10;

/// Maximum number of times a miner will try to pick a safe random destination before actually
/// staying where it is.
const MAX_RANDOM_SAFE_DEST_TRIES: usize = 10;

macro_rules! parse_input {
    ($x:expr, $t:ident) => ($x.trim().parse::<$t>().unwrap())
}

/// Compute the “Manhattan distance” between two points.
fn manh_dist(a: [i32; 2], b: [i32; 2]) -> i32 {
  (a[0] - b[0]).abs() + (a[1] - b[1]).abs()
}

/// Check if a cell is at range with another cell — i.e. the Manhattan distance is ≤ 1 or it’s a
/// neighbour.
fn is_cell_at_range(a: [i32; 2], b: [i32; 2]) -> bool {
   (a[0] == b[0] || a[1] == b[1]) && manh_dist(a, b) <= 1
}

trait TryFrom<T>: Sized {
  type Error;

  fn try_from(t: T) -> Result<Self, Self::Error>;
}

trait TryInto<T> {
  type Error;

  fn try_into(self) -> Result<T, Self::Error>;
}

impl<T, U> TryInto<T> for U where T: TryFrom<U> {
  type Error = T::Error;

  fn try_into(self) -> Result<T, Self::Error> {
    T::try_from(self)
  }
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

/// Entity.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum Entity {
  Miner(usize),
  OpponentMiner(usize),
  BurriedRadar,
  BurriedTrap,
}

/// Possible items a miner can hold.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum Item {
  Radar,
  Trap,
  Ore
}

impl TryFrom<i32> for Item {
  type Error = String;

  fn try_from(value: i32) -> Result<Self, Self::Error> {
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
  Move(i32, i32),
  Wait,
  Dig(i32, i32),
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

impl Request {
  fn submit(self) {
    println!("{}", self);
  }

  fn comment<S>(self, msg: S) -> RequestComment where S: Into<String> {
    RequestComment::new(self, Some(msg.into()))
  }

  fn back_to_hq(position: [i32; 2]) -> Request {
    Request::Move(0, position[1])
  }
}

/// A request with a possible associated comment.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct RequestComment {
  req: Request,
  comment: Option<String>
}

impl RequestComment {
  fn new<C>(req: Request, comment: C) -> Self where C: Into<Option<String>> {
    RequestComment {
      req,
      comment: comment.into()
    }
  }

  fn submit(self) {
    println!("{}", self);
  }
}

impl From<Request> for RequestComment {
  fn from(req: Request) -> Self {
    RequestComment::new(req, None)
  }
}

impl fmt::Display for RequestComment {
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
  // informational
  width: usize,
  height: usize,
  my_score: u32,
  opponent_score: u32,
  cells: Vec<Cell>,
  miners: Vec<Miner>,
  opponent_miners: Vec<Miner>,
  entities: HashMap<UID, Entity>,
  burried_radars: HashMap<UID, [i32; 2]>,
  burried_traps: HashMap<UID, [i32; 2]>,
  radar_cooldown: u32,
  trap_cooldown: u32,

  // tactical
  miner_with_radar: Option<usize>,
  dangerous_opponents: HashSet<usize>,
  dangerous_cells: HashMap<[i32; 2], Cell>,
}

impl GameState {
  fn new(width: usize, height: usize) -> Self {
    GameState {
      width,
      height,
      my_score: 0,
      opponent_score: 0,
      cells: vec![Cell::default(); width * height],
      miners: Vec::new(),
      opponent_miners: Vec::new(),
      entities: HashMap::new(),
      burried_radars: HashMap::new(),
      burried_traps: HashMap::new(),
      radar_cooldown: 0,
      trap_cooldown: 0,
      miner_with_radar: None,
      dangerous_opponents: HashSet::new(),
      dangerous_cells: HashMap::new(),
    }
  }

  fn set_my_score(&mut self, score: u32) {
    self.my_score = score;
  }

  fn set_opponent_score(&mut self, score: u32) {
    self.opponent_score = score;
  }

  fn set_ore(&mut self, x: usize, y: usize, ore_amount: Option<usize>) {
    self.cells[y * self.width + x].ore_amount = ore_amount;
  }

  fn set_hole(&mut self, x: usize, y: usize, hole: bool) {
    self.cells[y * self.width + x].has_hole = hole;
  }

  fn set_radar_cooldown(&mut self, cooldown: u32) {
    self.radar_cooldown = cooldown;
  }

  fn set_trap_cooldown(&mut self, cooldown: u32) {
    self.trap_cooldown = cooldown;
  }

  fn add_entity(&mut self, uid: UID, entity: Entity) {
    self.entities.insert(uid, entity);
  }

  fn entity_exists(&self, uid: UID) -> bool {
    self.entities.contains_key(&uid)
  }

  fn add_miner(&mut self, miner: Miner) -> usize {
    let index = self.miners.len();
    self.miners.push(miner);

    index
  }

  fn add_opponent_miner(&mut self, miner: Miner) -> usize {
    let index = self.opponent_miners.len();
    self.opponent_miners.push(miner);

    index
  }

  fn update_position(&mut self, uid: UID, px: i32, py: i32) {
    match self.entities.get(&uid) {
      Some(Entity::Miner(index)) => {
        let miner = &mut self.miners[*index];
        miner.x = px;
        miner.y = py;
      }

      Some(Entity::OpponentMiner(index)) => {
        let miner = &mut self.opponent_miners[*index];
        miner.x = px;
        miner.y = py;
      }

      _ => eprintln!("trying to update miner {} position, but it’s not a miner", uid)
    }
  }

  fn update_item(&mut self, uid: UID, item: Option<Item>) {
    match self.entities.get(&uid) {
      Some(Entity::Miner(index)) => {
        self.miners[*index].item = item;
      }

      Some(Entity::OpponentMiner(index)) => {
        self.opponent_miners[*index].item = item;
      }

      _ => eprintln!("trying to update miner {} item, but it’s not a miner", uid)
    }
  }

  fn kill(&mut self, uid: UID) {
    match self.entities.get(&uid) {
      Some(Entity::Miner(index)) => {
        self.miners[*index].alive = false;
      }

      Some(Entity::OpponentMiner(index)) => {
        self.opponent_miners[*index].alive = false;
      }

      _ => eprintln!("trying to kill miner {}, but it’s not a miner", uid)
    }
  }

  fn burry_radar(&mut self, uid: UID, x: i32, y: i32) {
    self.burried_radars.insert(uid, [x, y]);
  }

  fn burry_trap(&mut self, uid: UID, x: i32, y: i32) {
    self.burried_traps.insert(uid, [x, y]);
  }

  fn update_radar_position(&mut self, uid: UID, x: i32, y: i32) {
    if let Some(ref mut p) = self.burried_radars.get_mut(&uid) {
      p[0] = x;
      p[1] = y;
    } else {
      eprintln!("trying to update burried radar {} position, but it’s not a radar", uid);
    }
  }

  fn update_trap_position(&mut self, uid: UID, x: i32, y: i32) {
    if let Some(ref mut p) = self.burried_traps.get_mut(&uid) {
      p[0] = x;
      p[1] = y;
    } else {
      eprintln!("trying to update burried trap {} position, but it’s not a trap", uid);
    }
  }

  fn miners(&self) -> impl Iterator<Item = &Miner> {
    self.miners.iter()
  }

  fn cell(&self, x: i32, y: i32) -> Option<&Cell> {
    if x < 0 || x >= self.width as i32 || y < 0 || y >= self.height as i32 {
      None
    } else {
      self.cells.get(y as usize * self.width + x as usize)
    }
  }

  // Find the next spot where to put a radar.
  fn find_best_radar_spot(&self) -> Option<[i32; 2]> {
    // the algorithm is simple: we start at HQ and we want to put a radar at a distance of 5 from
    // the HQ so that the last detected cell left to the radar will be 1-cell away from the HQ; the
    // idea is to add pairs of radars for each x; we use a priority queue to quickly get the
    // furthest x pair and see whether we need to add a radar there

    // we get the “furthest” radars we’ve placed so far and get the number of radars per x distance
    // to HQ; we then get the furthest and if its count is < 2, we place our spot there
    let mut furthest = BTreeMap::<i32, usize>::new();
    for (_, radar) in &self.burried_radars {
      match furthest.entry(radar[0]) {
        Entry::Vacant(v) => {
          v.insert(1);
        }

        Entry::Occupied(mut v) => {
          *v.get_mut() += 1;
        }
      }
    }

    // get the furthest pair
    if let Some((&fx, &count)) = furthest.iter().next_back() {
      let col = fx / 4;

      if col % 2 == 0 {
        // even column; it means two radars
        if count == 1 {
          // the first radar is always at north, so place the second at south
          Some([fx, 11])
        } else if count == 2 {
          // two radars, so the next one is on the next column and is alone
          Some([fx + 4, 7])
        } else {
          None
        }
      } else {
        // odd column; it means one radar
        if count == 1 {
          // radar already there, so the next is on north on a pair
          Some([fx + 4, 3])
        } else {
          None
        }
      }
    } else {
      // we have no radar yet; first radar is easy to place
      Some([4, 7])
    }
  }

  fn assign_radar(&mut self) {
    // keep track of the best choice
    let mut found = None;

    for (miner_index, miner) in self.miners().enumerate() {
      if let Some((_, found_dist)) = found {
        if miner.x < found_dist {
          found = Some((miner_index, miner.x));
        }
      } else {
        found = Some((miner_index, miner.x));
      }
    }

    let (index, _) = found.unwrap();
    self.miner_with_radar = Some(index);
    //self.miners[index].order = Order::deploy_radar_to_random(self.width as i32, self.height as i32);

    // instead, we have already access to our burried radars, so let’s just find the next step; each
    // radar has a radius of 4 units, hence 8 units. The map is 15-cells tall so we just need, in
    // theory, two radars per column
    let [radar_x, radar_y] = self.find_best_radar_spot().unwrap();
    self.miners[index].order = Order::DeployRadarAt(radar_x, radar_y);
  }

  /// Find the most appealing order to follow.
  ///
  /// If some ore is available, the miner will try to go to the nearest place without overloading
  /// it. If no ore information is available, the miner will go in a random direction.
  fn choose_order(&self, miner_index: usize) -> Order {
    let mut closest_cell = None;
    let miner = &self.miners[miner_index];

    // FIXME: ensure the cell we’re targetting is not already overcrowded by other miners

    for x in 0 .. self.width {
      for y in 0 .. self.height {
        let x = x as i32;
        let y = y as i32;

        // prevent digging dangerous cells
        if self.is_cell_dangerous(x, y) {
          continue;
        }

        let cell = self.cell(x, y).unwrap();

        match cell.ore_amount {
          Some(ore_amount) if ore_amount > 0 => {
            if let Some((cx, cy, _)) = closest_cell {
              if manh_dist([x, y], [miner.x, miner.y]) < manh_dist([cx, cy], [miner.x, miner.y]) {
                closest_cell = Some((x, y, ore_amount));
              }
            } else {
              closest_cell = Some((x, y, ore_amount));
            }
          }

          _ => ()
        }
      }
    }

    if let Some((x, y, _)) = closest_cell {
      // we don’t really want to go to that cell directly; we want to dig “nearest” to that cell;
      // this part of code finds the solution to that problem
      let mut solution = [x, y];

      for i in &[-1, 1] {
        let candidate = [x + i, y];
        if self.cell(candidate[0], candidate[1]).is_some() {
          if manh_dist([miner.x, miner.y], candidate) < manh_dist([miner.x, miner.y], solution) {
            solution = candidate;
          }
        }

        let candidate = [x, y + i];
        if self.cell(candidate[0], candidate[1]).is_some() {
          if manh_dist([miner.x, miner.y], candidate) < manh_dist([miner.x, miner.y], solution) {
            solution = candidate;
          }
        }
      }

      Order::DigAt(x, y, solution[0], solution[1])
    } else {
      // loop until we find a safe destination; if we cannot find any, we’ll just wait (it’s a
      // bad situation but it’s safer than being killed)
      let mut final_order = None;

      for _ in 0 .. MAX_RANDOM_SAFE_DEST_TRIES {
        let order = Order::go_to_random(self.width as i32, self.height as i32);
        let [x, y] = order.destination();

        if !self.is_cell_dangerous(x, y) {
          final_order = Some(order);
        }
      }

      final_order.unwrap_or(Order::Stay(miner.x, miner.y))
    }
  }

  /// Total amount of ore we know about.
  fn visible_ore_amount(&self) -> usize {
    self.cells.iter().map(|cell| cell.ore_amount.unwrap_or(0)).sum()
  }

  /// Prepare the next turn by updating what must be updated.
  fn setup_next_turn(&mut self) {
    // update “previous” position of miners to be able to compute velocities
    for miner in &mut self.miners {
      miner.prev_xy = Some([miner.x, miner.y]);
    }

    for miner in &mut self.opponent_miners {
      miner.prev_xy = Some([miner.x, miner.y]);
    }
  }

  /// Implement digging go-to-like orders.
  fn order_go_to(&mut self, miner_index: usize, x: i32, y: i32, dig_x: i32, dig_y: i32) -> Request {
    let miner = self.miners[miner_index].clone();

    // first thing first; check if the cell we are going to dig is not trapped; if it is, we need
    // to ABORT mission and regenerate a new order; this might happen if we received the order
    // from far away and the opponent detected that we wanted to go at that place and put a trap
    // while we were commutting (fuck them)
    if self.is_cell_dangerous(dig_x, dig_y) {
      let order = self.choose_order(miner_index);
      let [dx, dy] = order.destination();

      self.miners[miner_index].order = order;

      return Request::Move(dx, dy);
    }

    if manh_dist([x, y], [miner.x, miner.y]) == 0 {
      // we arrived at our destination, so let’s inspect the cell
      let cell = self.cell(dig_x, dig_y).unwrap();

      if miner.item == Some(Item::Ore) {
        // we just digged some ore; get back to the HQ
        self.miners[miner_index].order = Order::Deliver(miner.x, miner.y);
        Request::back_to_hq([miner.x, miner.y])
      } else if cell.ore_amount.is_none() && !cell.has_hole {
        // case of an unknown cell with no hole; we are there so we just dig to check
        Request::Dig(dig_x, dig_y)
      } else if cell.ore_amount.unwrap_or(0) > 0 {
        // the current cell the current cell has some ore so we dig it
        self.miners[miner_index].order = Order::Deliver(miner.x, miner.y);
        Request::Dig(dig_x, dig_y)
      } else {
        // the current cell has no ore and it’s already digged; let’s get another order
        let order = self.choose_order(miner_index);
        let [dx, dy] = order.destination();

        self.miners[miner_index].order = order;

        Request::Move(dx, dy)
      }
    } else {
      // we still have to travel to our cell, but we still look for better solution, because
      // maybe a radar has been burried and we should change our orderh;, abort the current
      // order and go dig in that case!
      let other_order = self.choose_order(miner_index);
      if other_order.is_digging_order()
        && (miner.order.is_random() || manh_dist([miner.x, miner.y], other_order.destination()) < manh_dist([miner.x, miner.y], [x, y])) {
          // if it gets optimal, we’ll move to a closer location
          self.miners[miner_index].order = other_order;
          let [dx, dy] = other_order.destination();
          Request::Move(dx, dy)
      } else {
        // we haven’t found a better solution so let’s keep going
        Request::Move(x, y)
      }
   }
  }

  /// Check whether a cell is dangerous.
  fn is_cell_dangerous(&self, x: i32, y: i32) -> bool {
    self.dangerous_cells.contains_key(&[x, y])
  }

  /// Tag dangerous cells safe if we’re sure they’re not dangerous anymore.
  fn retag_safe_cells(&mut self) {
    let cells = &self.cells;
    let width = self.width;

    self.dangerous_cells.retain(|[dx, dy], dangerous_cell| {
      // a cell is dangerous only if the current amount of ore is still the same number as when it
      // was marked dangerous
      let r = cells[*dy as usize * width + *dx as usize].ore_amount == dangerous_cell.ore_amount;
      if !r {
        eprintln!("cell ({}, {}) is actually safe!", dx, dy);
      }
      r
    });
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

impl fmt::Display for Cell {
  fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
    let hole = if self.has_hole { 'o' } else { 'x' };
    let ore_amount = if let Some(ore_amount) = self.ore_amount {
      format!("{:2}", ore_amount)
    } else {
      "  ".to_owned()
    };

    write!(f, "{}{}", hole, ore_amount)?;

    Ok(())
  }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct Miner {
  x: i32,
  y: i32,
  prev_xy: Option<[i32; 2]>,
  item: Option<Item>,
  uid: UID,
  alive: bool,
  order: Order,
}

impl Miner {
  /// Velocity of the miner as a gradient.
  fn velocity_gradient(&self) -> [i32; 2] {
    let [px, py] = self.prev_xy.unwrap_or([self.x, self.y]);
    [self.x - px, self.y - py]
  }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum Order {
  /// Do just nothing.
  Stay(i32, i32),
  /// Move to a given unit.
  ///
  /// This is an exploration mode. If a better alternative is found, this unit should abort this
  /// order.
  GoTo(i32, i32),
  /// Move to a cell to dig from.
  ///
  /// The first two i32 is the actual ore cell to dig. The last two i32 is the cell from which
  /// we should dig (the destination).
  DigAt(i32, i32, i32, i32),
  DeployRadarAt(i32, i32),
  Deliver(i32, i32),
}

impl Order {
  fn go_to_random(width: i32, height: i32) -> Self {
    let mut rng = thread_rng();
    Order::GoTo(rng.gen_range(1, width), rng.gen_range(0, height))
  }

  /// Deploy a radar at a random location; we prevent burrying the radar too close to edges because
  /// it would be a waste.
  fn deploy_radar_to_random(width: i32, height: i32) -> Self {
    let mut rng = thread_rng();
    Order::DeployRadarAt(rng.gen_range(3, width - 3), rng.gen_range(3, height - 3))
  }

  fn destination(&self) -> [i32; 2] {
    match *self {
      Order::Stay(x, y) => [x, y],
      Order::GoTo(x, y) => [x, y],
      Order::DigAt(_, _, x, y) => [x, y],
      Order::DeployRadarAt(x, y) => [x, y],
      Order::Deliver(x, y) => [x, y],
    }
  }

  fn is_random(&self) -> bool {
    if let Order::GoTo(..) = *self {
      true
    } else {
      false
    }
  }

  fn is_digging_order(&self) -> bool {
    if let Order::DigAt(..) = *self {
      true
    } else {
      false
    }
  }
}

fn main() {
  let mut input_line = String::new();
  io::stdin().read_line(&mut input_line).unwrap();
  let inputs = input_line.split(" ").collect::<Vec<_>>();

  let width = parse_input!(inputs[0], i32);
  let height = parse_input!(inputs[1], i32); // size of the map

  let mut game_state = GameState::new(width as usize, height as usize);

  // game loop
  let mut turn: usize = 0;

  loop {
    let mut input_line = String::new();
    io::stdin().read_line(&mut input_line).unwrap();
    let inputs = input_line.split(" ").collect::<Vec<_>>();

    let my_score = parse_input!(inputs[0], u32); // Amount of ore delivered
    let opponent_score = parse_input!(inputs[1], u32);

    game_state.set_my_score(my_score);
    game_state.set_opponent_score(opponent_score);

    for y in 0 .. height as usize {
      let mut input_line = String::new();
      io::stdin().read_line(&mut input_line).unwrap();
      let inputs = input_line.split_whitespace().collect::<Vec<_>>();

      // we skip x = 0 as it’s HQ
      for x in 1 .. width as usize {
        let ore: Option<usize> = inputs[2 * x].trim().parse().ok(); // amount of ore or "?" if unknown
        let hole = parse_input!(inputs[2 * x + 1], u32) == 1; // 1 if cell has a hole

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

    game_state.set_radar_cooldown(radar_cooldown);
    game_state.set_trap_cooldown(trap_cooldown);

    for _ in 0..entity_count as usize {
      let mut input_line = String::new();
      io::stdin().read_line(&mut input_line).unwrap();
      let inputs = input_line.split(" ").collect::<Vec<_>>();

      let uid = parse_input!(inputs[0], u32); // unique id of the entity
      let entity_type: EntityType = parse_input!(inputs[1], u32).try_into().unwrap();

      let x = parse_input!(inputs[2], i32);
      let y = parse_input!(inputs[3], i32); // position of the entity
      let item = parse_input!(inputs[4], i32).try_into().ok(); // if this entity is a robot, the item it is carrying (-1 for NONE, 2 for RADAR, 3 for TRAP, 4 for ORE)

      // check if we need to update our entities
      if !game_state.entity_exists(uid) {
        // if it’s a miner, add it to the list of miners
        match entity_type {
          EntityType::Miner => {
            let miner_index = game_state.add_miner(Miner {
              x,
              y,
              prev_xy: None,
              item,
              uid,
              alive: true,
              order: Order::go_to_random(width, height),
            });

            game_state.add_entity(uid, Entity::Miner(miner_index));
          }

          EntityType::OpponentMiner => {
            let opponent_miner_index = game_state.add_opponent_miner(Miner {
              x,
              y,
              prev_xy: None,
              item,
              uid,
              alive: true,
              order: Order::go_to_random(width, height),
            });

            game_state.add_entity(uid, Entity::OpponentMiner(opponent_miner_index));
          }

          EntityType::BurriedRadar => {
            game_state.add_entity(uid, Entity::BurriedRadar);
            game_state.burry_radar(uid, x, y);
          }

          EntityType::BurriedTrap => {
            game_state.add_entity(uid, Entity::BurriedTrap);
            game_state.burry_trap(uid, x, y);
          }
        }
      } else {
        match entity_type {
          EntityType::Miner | EntityType::OpponentMiner => {
            if x == -1 && y == -1 {
              // this miner is dead
              game_state.kill(uid);
            }

            // update position
            game_state.update_position(uid, x, y);

            // update item
            game_state.update_item(uid, item);
          }

          EntityType::BurriedRadar => {
            // position of this radar has changed
            game_state.update_radar_position(uid, x, y);
          }

          EntityType::BurriedTrap => {
            // position of this trap has changed
            game_state.update_trap_position(uid, x, y);
          }
        }
      }
    }

    game_state.retag_safe_cells();

    // try to detect if an opponent is asking for an item
    for miner_index in 0 .. game_state.opponent_miners.len() {
      let miner = game_state.opponent_miners[miner_index].clone();

      // we don’t do anything at the first round since velocity gradients are not yet computed
      if turn != 0 {
        if miner.velocity_gradient() == [0, 0] {
          if miner.x == 0 {
            eprintln!("{} -> might be asking for an item!", miner_index);
            game_state.dangerous_opponents.insert(miner_index);
          } else if game_state.dangerous_opponents.contains(&miner_index) {
            game_state.dangerous_opponents.remove(&miner_index);

            eprintln!("{} -> might be burying a radar or a trap around ({}, {})!", miner_index, miner.x, miner.y);

            // maybe the trap is on the current player position
            let cell = game_state.cell(miner.x, miner.y).unwrap().clone();
            eprintln!("({}, {}) is dangerous", miner.x, miner.y);
            game_state.dangerous_cells.insert([miner.x, miner.y], cell);

            // we add the whole cross as dangerous too, which is very “defensive” but whatever
            for i in &[-1, 1] {
              if let Some(cell) = game_state.cell(miner.x + i, miner.y).cloned() {
                eprintln!("({}, {}) is dangerous", miner.x + i, miner.y);
                game_state.dangerous_cells.insert([miner.x + i, miner.y], cell);
              }

              if let Some(cell) = game_state.cell(miner.x, miner.y + i).cloned() {
                eprintln!("({}, {}) is dangerous", miner.x, miner.y + i);
                game_state.dangerous_cells.insert([miner.x, miner.y + i], cell);
              }
            }
          }
        }
      }
    }

    // FIXME: idea: burry the radar then unburry it immediately in order to burry it elsewhere
    // select a miner to carry radar if not already there
    if game_state.radar_cooldown == 0
      && game_state.miner_with_radar.is_none()
      && game_state.burried_radars.len() < 10
      && game_state.visible_ore_amount() < MIN_ORE_NO_RADAR_NEEDED
    {
      game_state.assign_radar();
    }

    for miner_index in 0 .. game_state.miners.len() {
      let miner = game_state.miners[miner_index].clone();

      let request = if Some(miner_index) == game_state.miner_with_radar {
        if let Order::DeployRadarAt(x, y) = miner.order {
          if miner.item == Some(Item::Radar) {
            // if that unit has already the radar
            if is_cell_at_range([x, y], [miner.x, miner.y]) {
              // if we arrived at destination, just burry the radar
              game_state.miner_with_radar = None;
              game_state.miners[miner_index].order = game_state.choose_order(miner_index);
              Request::Dig(x, y) // FIXME: UNSAFE IF TRAP
            } else {
              // otherwise, go there
              Request::Move(x, y)
            }
          } else if miner.x != 0 {
            // go home to ask for a radar
            Request::back_to_hq([miner.x, miner.y])
          } else {
            // ask a radar
            Request::Item(RequestItem::Radar)
          }
        } else {
          unreachable!()
        }
      } else {
        match miner.order {
          Order::GoTo(x, y) => {
            game_state.order_go_to(miner_index, x, y, x, y)
          }

          Order::DigAt(dig_x, dig_y, x, y) => {
            game_state.order_go_to(miner_index, x, y, dig_x, dig_y)
          }

          Order::Deliver(..) => {
            if miner.x != 0 {
              Request::back_to_hq([miner.x, miner.y])
            } else {
              let order = game_state.choose_order(miner_index);
              let [dx, dy] = order.destination();
              game_state.miners[miner_index].order = order;

              Request::Move(dx, dy)
            }
          }

          Order::Stay(..) => Request::Wait,

          _ => unreachable!()
        }
      };

      request.submit();
    }

    game_state.setup_next_turn();
    turn += 1;
  }
}
