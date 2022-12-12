#include "fight.mligo"
let init_store: fight_storage = {
    next_id = 1n;
    next_roundid = 1n;
    fight_fee = 0.5tez;
    fighter_addr = ("tz3QE72w1vC613ZaeAZwW5fxwrBUQAstyaZy": address);
    tournament_addr = ("tz3QE72w1vC613ZaeAZwW5fxwrBUQAstyaZy": address);
    attribute_addr = ("tz3QE72w1vC613ZaeAZwW5fxwrBUQAstyaZy": address);
    admin = ("tz3QE72w1vC613ZaeAZwW5fxwrBUQAstyaZy": address);
    fights = Big_map.empty;
    rounds = Big_map.empty;
    queues = Big_map.empty
}