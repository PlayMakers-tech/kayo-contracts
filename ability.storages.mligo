#include "ability.mligo"
let init_store: ability_storage = {
    next_id = 1n;
    fighter_addr = ("tz3QE72w1vC613ZaeAZwW5fxwrBUQAstyaZy": address);
    admin = ("tz3QE72w1vC613ZaeAZwW5fxwrBUQAstyaZy": address);
    available_abilities = Big_map.literal [
        (COMMON,    (Set.empty: ability_id set));
        (UNCOMMON,  (Set.empty: ability_id set));
        (RARE,      (Set.empty: ability_id set));
        (LEGENDARY, (Set.empty: ability_id set));
        (MYTHIC,    (Set.empty: ability_id set));
        (UNIQUE,    (Set.empty: ability_id set))
    ];
    fighter_abilities = Big_map.empty;
    abilities = Big_map.empty;
    proba_rarity = Map.literal [
        (COMMON,       1n);
        (UNCOMMON,     0n);
        (RARE,        10n);
        (LEGENDARY,   40n);
        (MYTHIC,       0n);
        (UNIQUE,     160n)
    ];
    amount_rarity = Map.literal [
        (COMMON,       0n);
        (UNCOMMON,     0n);
        (RARE,      1000n);
        (LEGENDARY,  100n);
        (MYTHIC,       0n);
        (UNIQUE,       1n)
    ]
}