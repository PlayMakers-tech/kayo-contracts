#if !ATTRIBUTE_SCHEMA
#define ATTRIBUTE_SCHEMA

type attribute_value = bytes

// Type definition for skin (tree of genes)
type attribute_skin = bytes
type attribute_skin_node = bytes * nat * nat (* id, size, likelihood *)

#include "fighter.schema.mligo"

type attribute_data = {
    id: fighter_id;
    xp: nat;
    crypted: attribute_value;
    skin: attribute_skin
}

type attribute_storage = {
    fight_addr: address;
    fighter_addr: address;
    admin: address;
    skin_nodes: nat * (attribute_skin_node list);
    skin_leaves: nat * (attribute_skin_node list);
    attributes: (fighter_id, attribute_data) big_map
}

type attribute_parameter =
| SetFightAddr of address
| SetFighterAddr of address
| SetSkinNodes of nat * (attribute_skin_node list)
| SetSkinLeaves of nat * (attribute_skin_node list)
| EarnXP of fighter_id * nat
| Mint of fighter_id * attribute_value
| Fusion of fighter_id * fighter_id * fighter_id * attribute_value

#endif