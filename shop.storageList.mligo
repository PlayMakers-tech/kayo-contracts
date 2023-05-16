#include "shop.mligo"
let init_store: shop_storage = {
    admins = Set.empty; // This needs to be the deployer
    managers = Set.empty;
    is_open = true;
    items = Map.literal [
        "fighter1", {
            item = "fighter1";
            quantity = 0n;
            consumers = Set.empty;  // This needs to be the Fighter contract
            price = 100tez
        };
        "fighter2", {
            item = "fighter2";
            quantity = 0n;
            consumers = Set.empty;  // This needs to be the Fighter contract
            price = 100tez
        };
        "fight1", {
            item = "fight1";
            quantity = 9999999999n;
            consumers = Set.empty;  // This needs to be the Fight contract
            price = 1tez
        };
        "fight2", {
            item = "fight2";
            quantity = 999999999n;
            consumers = Set.empty;  // This needs to be the Fight contract
            price = 100tez
        };
    ];
    bundles = Map.literal [
        "booster1", {
            bundle = "booster1";
            quantity = 999999999n;
            items = Map.literal [
                "fighter1", 1;
                "fight1", 50;
            ];
            price = 20tez
        };
        "booster2", {
            bundle = "booster2";
            quantity = 999999999n;
            items = Map.literal [
                "fighter2", 1;
                "fight1", 50;
                "fight2", 1;
            ];
            price = 30tez
        };
        "tickets1", {
            bundle = "tickets1";
            quantity = 999999999n;
            items = Map.literal [
                "fight1", 25;
                "fight2", 1;
            ];
            price = 10tez
        };
        "tickets2", {
            bundle = "tickets2";
            quantity = 999999999n;
            items = Map.literal [
                "fight1", 50;
                "fight2", 2;
            ];
            price = 16tez
        };
        "tickets3", {
            bundle = "tickets3";
            quantity = 999999999n;
            items = Map.literal [
                "fight1", 100;
                "fight2", 5;
            ];
            price = 30tez
        };

    ];
    owned_items = Big_map.empty;
}
