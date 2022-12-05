#include "tournament.mligo"
let init_store: tournament_storage = {
    next_id = 1n;
    tournament_fee = 1tez;
    fighter_addr = ("tz3QE72w1vC613ZaeAZwW5fxwrBUQAstyaZy": address);
    fight_addr = ("tz3QE72w1vC613ZaeAZwW5fxwrBUQAstyaZy": address);
    admin = ("tz3QE72w1vC613ZaeAZwW5fxwrBUQAstyaZy": address);
    active_tournaments = Set.empty;
    tournaments = Big_map.empty
}