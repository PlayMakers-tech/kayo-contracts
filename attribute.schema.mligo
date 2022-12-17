#if !ATTRIBUTE_SCHEMA
#define ATTRIBUTE_SCHEMA

type attribute_value = nat

// Type definition for skin (tree of genes)
type attribute_leaf = bytes
type attribute_node = nat
type attribute_tree =
| Node of (attribute_node * (attribute_leaf list)) (* * (attribute_tree list)) *)
| Leaf of attribute_leaf

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
    skin_node_limit: attribute_node;
    attributes: (fighter_id, attribute_data) big_map
}

type attribute_parameter =
| SetFightAddr of address
| SetFighterAddr of address
| SetSkinNodeLimit of attribute_node
| EarnXP of fighter_id * nat
| Mint of fighter_id
| Fusion of fighter_id * fighter_id * fighter_id

#endif