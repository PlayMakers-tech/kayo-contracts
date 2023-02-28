#include "marketfighter.mligo"
let init_store: marketfighter_storage = {
    is_open = true;
    listing_fee = 0.1tez;
    fighter_addr = ("tz3QE72w1vC613ZaeAZwW5fxwrBUQAstyaZy": address);
    admin = ("tz3QE72w1vC613ZaeAZwW5fxwrBUQAstyaZy": address);
    min_price = 10tez;
    listed_offer = Set.empty;
    listed_sale = Set.empty;
    sells = Big_map.empty;
    buys = Big_map.empty;
}
