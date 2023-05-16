#include "fighter.mligo"
let init_store: fighter_storage = {
    admins = Set.empty; // This needs to be the deployer
    managers = Set.empty; // This needs to have Fight, Tournament, Marketfighter
    minters = Set.empty;
    ability_addr = ("tz3QE72w1vC613ZaeAZwW5fxwrBUQAstyaZy": address);
    attribute_addr = ("tz3QE72w1vC613ZaeAZwW5fxwrBUQAstyaZy": address);
    shop_addr = ("tz3QE72w1vC613ZaeAZwW5fxwrBUQAstyaZy": address);
    next_id = 1n;
    mint_fee = 10tez;
    fusion_fee = 20tez;
    mints = Set.empty;
    fighters = Big_map.empty;
    fighters_by_owner = Big_map.empty;
}
