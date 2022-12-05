#if !ABILITY_SCHEMA
#define ABILITY_SCHEMA

type ability_id = nat

type rarity =
| COMMON
| UNCOMMON
| RARE
| LEGENDARY
| MYTHIC
| UNIQUE

type ability_data = {
    id: ability_id;
    rarity: rarity;
    cnt: nat
}

#include "fighter.schema.mligo"

type ability_storage = {
    next_id: ability_id;
    fighter_addr: address;
    admin: address;
    available_abilities: (rarity, ability_id set) big_map;
    fighter_abilities: (fighter_id, ability_id set) big_map;
    abilities: (ability_id, ability_data) big_map;
    proba_rarity: (rarity, nat) map;
    amount_rarity: (rarity, nat) map
}

type ability_parameter =
| SetFighterAddr of address
| SetProbaRarity of rarity * nat
| SetAmountRarity of rarity * nat
| CreateAbility of rarity list
| Mint of fighter_id
| Fusion of fighter_id * fighter_id * fighter_id
| LearnAbility of fighter_id * ability_id
| ForgetAbility of fighter_id * ability_id

#endif