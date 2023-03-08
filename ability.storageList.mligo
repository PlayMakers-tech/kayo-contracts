#include "ability.mligo"
let init_store: ability_storage = {
    next_id = 1n;
    fighter_addr = ("tz3QE72w1vC613ZaeAZwW5fxwrBUQAstyaZy": address);
    admin = ("tz3QE72w1vC613ZaeAZwW5fxwrBUQAstyaZy": address);
    available_abilities = Big_map.literal [
        (Common,    (Set.empty: ability_id set));
        (Uncommon,  (Set.empty: ability_id set));
        (Rare,      (Set.empty: ability_id set));
        (Epic,      (Set.empty: ability_id set));
        (Mythic,    (Set.empty: ability_id set));
        (Unique,    (Set.empty: ability_id set))
    ];
    fighter_abilities = Big_map.empty;
    abilities = Big_map.empty;
    proba_rarity = Map.literal [
        (Common,       1n);
        (Uncommon,     0n);
        (Rare,         6n);
        (Epic,        20n);
        (Mythic,       0n);
        (Unique,   10000n)
    ]
}