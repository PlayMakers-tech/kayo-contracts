#include "fighter.mligo"
let init_store: fighter_storage = {
    next_id = 1n;
    mint_fee = 10tez;
    fusion_fee = 20tez;
    fight_addr = ("tz3QE72w1vC613ZaeAZwW5fxwrBUQAstyaZy": address);
    tournament_addr = ("tz3QE72w1vC613ZaeAZwW5fxwrBUQAstyaZy": address);
    attribute_addr = ("tz3QE72w1vC613ZaeAZwW5fxwrBUQAstyaZy": address);
    ability_addr = ("tz3QE72w1vC613ZaeAZwW5fxwrBUQAstyaZy": address);
    admin = ("tz3QE72w1vC613ZaeAZwW5fxwrBUQAstyaZy" : address);
    mints = Set.empty;
    fighters = Big_map.empty;
    fighters_by_owner = Big_map.empty;
}
