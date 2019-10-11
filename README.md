# Unleash the Geek

## v0.1

For this first version, I wanted:

- [x] To discover the game mechanics around mining.
- [x] Have a single unit handling radars. That miner is not necessarily the same one every time.
  instead, I pick the “best” one when I need a radar (i.e. it’s the one nearest a HQ cell) then
  dispatch it to grab a radar and deploy it at a random location. I don’t take cooldown into account
  so far.
- [x] All other units are dispatched according to a simple rule:
  - If we have ore information, we know for sure that some ore is availble, so we take the nearest
    ore cell to the miner and dispatch the miner to dig there.
  - If no ore information is available (typical when starting the game), we go in a complete random
    location.
- [x] When a unit either burries a radar or digs, if it has found ore, it immediately goes back to HQ.
- [x] We don’t use traps at all so far nor we defend against enemies nor attack them. Our current
  strategy is mining first and multiplying the number of radars we dig. No defence / attack
  strategy for v1.0.

Possible enhancements:

- When a radar-miner is dispatched, it picks a random location to deploy the radar. It’s a bit dumb
  as we might want to have a better idea of the map. Instead, we need to find the “best place” in
  on the grid we already know to cover as much area as possible. Also, we should explore first near
  HQ and then try to go far away.
- When choosing a new order for a miner that has finished their job, we need to be smarter when
  picking a new place to dig. Currently, we go to the “nearest ore cell” to the unit, which is
  completely fine BUT we need to ensure that given the amount of ore we know this ore cell has,
  going there won’t overcrowd the cell. Implementing this idea will bring a nice heuristics and a
  smoother repartition on the map.
- We can try to burry a radar and unburry it immediately. This will have the effect of allowing to
  explore way quicker but we lose “live” information, so we need to change a bit the way the cells
  store information.
- Radars should be placed near first and we should be smarter about “area awareness.” Currently, we
  are doing them randomly so the first radar might be very far, which is not really a good idea
  (i.e. if the opponent implements a nearest-first approach, they will grab that ore before me).

## v0.2

- [x] Radars are now deployed in a pattern using even / odd sequences in order to tightly map the whole
  grid. That pattern is optimized and hard-coded for 30×15 grids so if we need to change later for
  other maps, we’ll have to come up with a formula.
- [x] The radar-miner can now be dismissed if we know “enough” ore veins. I’m still playing with the
  value but started at 20 ore units. Also, I stop burrying radars if the number of radars goes up to
  ten. This is a potential flaw: if an ennemy destroys one of my radar, I’ll have to detect it and
  replace it if it’s needed, but currently, we don’t care (I think).
- [x] Detect when a player burries a trap. We need to think how we’re supposed to detect that. It
  should be doable by storing the last position of every enemy unit along with its speed: if the
  last speed was 0 and the enemy is now moving straight to the left, it’s very likely the previous
  cell was an ore vein.
- [x] Enemies that stop at x = 0 “might” be carrying something. We need to take that into account.
- [x] Fix when a miner would bury something. In v0.1, a miner would wait until it has hit the cell
  while now, it starts digging as soon as it’s “near” enough.
- [ ] When no ore data is available, we shouldn’t go too far away to unburry.
- [ ] Handle overcrowded ore cell dispatch.
- [x] Abort mission if we’ve detected a trap has been placed AFTER our order was dispatched.

## v0.3

- [x] When no ore data is available, we shouldn’t go too far away to unburry. This is fixed via a
  _permanent snapshot_ of the cells. That is the same thing as the grid of cells but instead of
  showing the current live version of the map, it keeps memory of thing we knew was there. That
  should allow for several cool optimizations.
- [ ] Handle overcrowded ore cell dispatch.
- [x] We can untag dangerous cell and make them safe back by storing the amount of ore for each
  dangerous zone: at every turn, we look at each bad zone and check whether the value has decreased.
  If so, it means that the enemy has been digging in it and then it’s safe.

## v0.4

- [ ] Handle overcrowded ore cell dispatch.
- [ ] We need to be able to recover broken radars.
- [ ] We sometimes timeout!! I think it was due to a bad handling of dead opponents.
