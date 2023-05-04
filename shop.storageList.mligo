#include "shop.mligo"
let init_store: shop_storage = {
    admins = Set.empty; // This needs to be the deployer
    managers = Set.empty;
    is_open = true;
    items = Map.empty;
    bundles = Map.empty;
    owned_items = Big_map.empty;
}
