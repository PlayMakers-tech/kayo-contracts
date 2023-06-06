#if !ATTRIBUTE_SCHEMA
#define ATTRIBUTE_SCHEMA

type attribute_value = bytes

// Type definition for skin (tree of genes)
type attribute_skin = bytes
type attribute_skin_node = bytes * nat * nat (* id, size, likelihood *)

#include "../fighter/fighter.schema.mligo"

type attribute_data = {
    id: fighter_id;
    xp: nat;
    crypted: attribute_value;
    skin: attribute_skin
}

type attribute_storage = {
    admins: address set;
    managers: address set;
    skin_nodes: nat * (attribute_skin_node list);
    skin_leaves: nat * (attribute_skin_node list);
    attributes: (fighter_id, attribute_data) big_map
}

type attribute_parameter =
| SetAdmins of address set
| SetManagers of address set
| SetSkinNodes of nat * (attribute_skin_node list)
| SetSkinLeaves of nat * (attribute_skin_node list)
| EarnXP of fighter_id * nat
| Mint of fighter_id * attribute_value
| Fusion of fighter_id * fighter_id * fighter_id * attribute_value

#endif