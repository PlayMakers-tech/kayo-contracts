#include "fight.mligo"
let init_store: fight_storage = {
    admins = Set.empty; // This needs to be the deployer
    managers = Set.empty; // This needs to have Tournament
    matchers = Set.empty;
    resolvers = Set.empty;
    next_id = 1n;
    fight_fee = 0.5tez;
    fighter_addr = ("tz3QE72w1vC613ZaeAZwW5fxwrBUQAstyaZy": address);
    attribute_addr = ("tz3QE72w1vC613ZaeAZwW5fxwrBUQAstyaZy": address);
    fights = Big_map.empty;
    fights_by_fighter = Big_map.empty;
    queues = Big_map.empty
}