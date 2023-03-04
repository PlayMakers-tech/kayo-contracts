#include "shop.mligo"
let init_store: shop_storage = {
    is_open = true;
    admin = ("tz3QE72w1vC613ZaeAZwW5fxwrBUQAstyaZy": address);
    items = Map.empty;
    bundles = Map.empty;
    owned_items = Big_map.empty;
}
