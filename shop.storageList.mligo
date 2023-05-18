#include "shop.mligo"
let init_store: shop_storage = {
    admins = Set.empty; // This needs to be the deployer
    managers = Set.empty; // We need to have Fight and Tournament in here
    is_open = true;
    items = Map.literal [
        "fighter1", {
            item = "fighter1";
            quantity = 0n;
            consumers = (Set.empty: address set);  // This should have Fighter, Fight, Tournament
            price = 100tez
        };
        "fighter2", {
            item = "fighter2";
            quantity = 0n;
            consumers = (Set.empty: address set);  // This should have Fighter, Fight, Tournament
            price = 100tez
        };
        "fight1", {
            item = "fight1";
            quantity = 9999999999n;
            consumers = (Set.empty: address set);  // This should have Fight, Tournament
            price = 1tez
        };
        "fight2", {
            item = "fight2";
            quantity = 999999999n;
            consumers = (Set.empty: address set);  // This should have Fight, Tournament
            price = 100tez
        };
    ];
    bundles = Map.literal [
        "booster1", {
            bundle = "booster1";
            quantity = 999999999n;
            items = Map.literal [
                "fighter1", 1n;
                "fight1", 50n;
            ];
            price = 20tez
        };
        "booster2", {
            bundle = "booster2";
            quantity = 999999999n;
            items = Map.literal [
                "fighter2", 1n;
                "fight1", 50n;
                "fight2", 1n;
            ];
            price = 30tez
        };
        "tickets1", {
            bundle = "tickets1";
            quantity = 999999999n;
            items = Map.literal [
                "fight1", 25n;
                "fight2", 1n;
            ];
            price = 10tez
        };
        "tickets2", {
            bundle = "tickets2";
            quantity = 999999999n;
            items = Map.literal [
                "fight1", 50n;
                "fight2", 2n;
            ];
            price = 16tez
        };
        "tickets3", {
            bundle = "tickets3";
            quantity = 999999999n;
            items = Map.literal [
                "fight1", 100n;
                "fight2", 5n;
            ];
            price = 30tez
        };

    ];
    owned_items = Big_map.empty;
}
