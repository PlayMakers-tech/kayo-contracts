#if !ATTRIBUTE_SCHEMA
#define ATTRIBUTE_SCHEMA

type attribute_value = nat

// Type definition for skin (tree of genes)
type attribute_leaf = string
type attribute_node = nat
type attribute_tree =
| Node attribute_node (attribute_leaf list)
| Leaf attribute_leaf

#include "fighter.schema.mligo"

type attribute_data = {
    id: fighter_id;
    xp: nat;
    str: attribute_value;
    agi: attribute_value;
    con: attribute_value;
    spd: attribute_value;
    skin: attribute_tree
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