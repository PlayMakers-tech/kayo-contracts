#include "attribute.mligo"
let init_store: attribute_storage = {
    admins = Set.empty; // This needs to be the deployer
    managers = Set.empty; // This needs to have Fighter, Fight, Tournament
    skin_nodes = (1n, [(0x10,1n,1n)]);
    skin_leaves = (1n, [(0x00,2n,1n)]);
    attributes = Big_map.empty
}