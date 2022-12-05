#if !ATTRIBUTE_SCHEMA
#define ATTRIBUTE_SCHEMA

type attribute_value = nat

#include "fighter.schema.mligo"

type attribute_data = {
    id: fighter_id;
    xp: nat;
    str: attribute_value;
    agi: attribute_value;
    con: attribute_value;
    spd: attribute_value
}


type attribute_storage = {
    fight_addr: address;
    fighter_addr: address;
    admin: address;
    attributes: (fighter_id, attribute_data) big_map
}

type attribute_parameter =
| SetFightAddr of address
| SetFighterAddr of address
| EarnXP of fighter_id * nat
| Mint of fighter_id
| Fusion of fighter_id * fighter_id * fighter_id

#endif