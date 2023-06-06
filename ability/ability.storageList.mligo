#include "ability.mligo"
let init_store: ability_storage = {
    admins = Set.empty; // This needs to be the deployer
    managers = Set.empty; // This needs to have Fighter
    next_id = 1n;
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