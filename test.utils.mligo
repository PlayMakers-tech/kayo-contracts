#include "schema.mligo"
#include "error.mligo"
#include "utils.mligo"

//************* Declaration of variables *************//
let _ = Test.reset_state 4n []

let admin_address = Test.nth_bootstrap_account 1

let alice_address = Test.nth_bootstrap_account 2

let bob_address = Test.nth_bootstrap_account 3

let dummy_address = ("tz3QE72w1vC613ZaeAZwW5fxwrBUQAstyaZy" : address)

let mint_fee = 10tez

let fight_fee = 0.5tez

let tournament_fee = 0.7tez

let fusion_fee = 20tez

let listing_fee = 0.1tez

let min_listing_price = 8tez

//************* Declaration of functions *************//
let print_checkmark (given, expected : bool * bool) =
  Test.print (if given = expected then "  v_true" else "  x_false")

let print_step (toPrint : string) = Test.println (" : " ^ toPrint)

let print_topic (toPrint : string) = Test.println (toPrint)

let test_entrypoint (name : string) (a : test_exec_result) (expected : bool) =
  match a with
    Success gas ->
      let _ = print_checkmark (true, expected) in
      let _ = print_step name in
      gas
  | Fail err ->
      let _ = print_checkmark (false, expected) in
      let _ = print_step name in
      let _ =
        Test.print
          "      error_detected The previous test shows the following error : " in
      let _ = Test.log err in
      0n

let get_fighter_data (id,d: fighter_id * fighter_storage) =
    Option.unopt_with_error (Big_map.find_opt id d.fighters) "Invalid fighter_id"
let get_fight_data (id,d: fight_id * fight_storage) =
    Option.unopt_with_error (Big_map.find_opt id d.fights) "Invalid fight_id"
let get_fighter_abilities (id,d: fighter_id * ability_storage) =
    Option.unopt_with_error (Big_map.find_opt id d.fighter_abilities) "Invalid fighter_id"
let get_attribute_data (id, d: fighter_id * attribute_storage) =
    Option.unopt_with_error (Big_map.find_opt id d.attributes) "Invalid fighter_id"


//************* Declaration of contracts and their variables *************//
let _ = Test.set_source admin_address

// Fighter contract
let init_store_fighter : fighter_storage = {
    next_id = 1n;
    mint_fee = mint_fee;
    fusion_fee = fusion_fee;
    fight_addr = dummy_address;
    tournament_addr = dummy_address;
    attribute_addr = dummy_address;
    ability_addr = dummy_address;
    marketfighter_addr = dummy_address;
    admin = (admin_address : address);
    mints = Set.empty;
    fighters = Big_map.empty;
    fighters_by_owner = Big_map.empty
}
let fighter_addr, _, _ = Test.originate_from_file "fighter.mligo" "main" [] (Test.eval init_store_fighter) 0tez
let fighter_typed_addr: (fighter_parameter, fighter_storage) typed_address = Test.cast_address fighter_addr
let fighter_contract = Test.to_contract fighter_typed_addr

let test_fighter (name : string) (addr, op, amount : address * fighter_parameter * tez) (expected : bool) =
  let _ = Test.set_source addr in
  test_entrypoint name (Test.transfer_to_contract fighter_contract op amount) expected

// Fight contract
let init_store_fight : fight_storage = {
    next_id = 1n;
    fight_fee = fight_fee;
    fighter_addr = (fighter_addr: address);
    tournament_addr = dummy_address;
    attribute_addr = dummy_address;
    admin = (admin_address: address);
    fights = Big_map.empty;
    fights_by_fighter = Big_map.empty;
    queues = Big_map.empty
}
let fight_addr, _, _ = Test.originate_from_file "fight.mligo" "main" [] (Test.eval init_store_fight) 0tez
let fight_typed_addr: (fight_parameter, fight_storage) typed_address = Test.cast_address fight_addr
let fight_contract = Test.to_contract fight_typed_addr


let test_fight (name : string) (addr, op, amount : address * fight_parameter * tez) (expected : bool) =
  let _ = Test.set_source addr in
  test_entrypoint name (Test.transfer_to_contract fight_contract op amount) expected

// Tournament contract
let init_store_tournament : tournament_storage = {
    next_id = 1n;
    tournament_fee = tournament_fee;
    fight_addr = (fight_addr: address);
    fighter_addr = (fighter_addr: address);
    admin = (admin_address: address);
    active_tournaments = Set.empty;
    tournaments = Big_map.empty;
}
let tournament_addr, _, _ = Test.originate_from_file "tournament.mligo" "main" [] (Test.eval init_store_tournament) 0tez
let tournament_typed_addr: (tournament_parameter, tournament_storage) typed_address = Test.cast_address tournament_addr
let tournament_contract = Test.to_contract tournament_typed_addr


let test_tournament (name : string) (addr, op, amount : address * tournament_parameter * tez) (expected : bool) =
  let _ = Test.set_source addr in
  test_entrypoint name (Test.transfer_to_contract tournament_contract op amount) expected

// Ability contract
let init_store_ability: ability_storage = {
    next_id = 1n;
    fighter_addr = (fighter_addr: address);
      admin = (admin_address : address);
    available_abilities = Big_map.literal [
        (Common,    (Set.empty: ability_id set));
        (Uncommon,  (Set.empty: ability_id set));
        (Rare,      (Set.empty: ability_id set));
        (Epic,      (Set.empty: ability_id set));
        (Mythic,    (Set.empty: ability_id set));
        (Unique,    (Set.empty: ability_id set))
    ];
    fighter_abilities = Big_map.empty;
    abilities = Big_map.empty;
    proba_rarity = Map.literal [
        (Common,       1n);
        (Uncommon,     0n);
        (Rare,        10n);
        (Epic,        40n);
        (Mythic,       0n);
        (Unique,     160n)
    ]
}
let ability_addr, _, _ = Test.originate_from_file "ability.mligo" "main" [] (Test.eval init_store_ability) 0tez
let ability_typed_addr: (ability_parameter, ability_storage) typed_address = Test.cast_address ability_addr
let ability_contract = Test.to_contract ability_typed_addr

let test_ability (name : string) (addr, op, amount : address * ability_parameter * tez) (expected : bool) =
  let _ = Test.set_source addr in
  test_entrypoint name (Test.transfer_to_contract ability_contract op amount) expected

// Attribute contract
let init_store_attribute: attribute_storage = {
    fight_addr = (fight_addr: address);
    fighter_addr = (fighter_addr: address);
    admin = (admin_address : address);
    skin_nodes = (1n, [(0x10,1n,1n)]);
    skin_leaves = (1n, [(0x00,2n,1n)]);
    attributes = Big_map.empty
}
let attribute_addr, _, _ = Test.originate_from_file "attribute.mligo" "main" [] (Test.eval init_store_attribute) 0tez
let attribute_typed_addr: (attribute_parameter, attribute_storage) typed_address = Test.cast_address attribute_addr
let attribute_contract = Test.to_contract attribute_typed_addr

let test_attribute (name : string) (addr, op, amount : address * attribute_parameter * tez) (expected : bool) =
  let _ = Test.set_source addr in
  test_entrypoint name (Test.transfer_to_contract attribute_contract op amount) expected

// Marketfighter contract
let init_store_marketfighter: marketfighter_storage = {
    is_open = true;
    listing_fee = listing_fee;
    fighter_addr = (fighter_addr: address);
    admin = (admin_address: address);
    min_price = min_listing_price;
    listed_offer = Set.empty;
    listed_sale = Set.empty;
    sells = Big_map.empty;
    buys = Big_map.empty;
}
let marketfighter_addr, _, _ = Test.originate_from_file "marketfighter.mligo" "main" [] (Test.eval init_store_marketfighter) 0tez
let marketfighter_typed_addr: (marketfighter_parameter, marketfighter_storage) typed_address = Test.cast_address marketfighter_addr
let marketfighter_contract = Test.to_contract marketfighter_typed_addr

let test_marketfighter (name : string) (addr, op, amount : address * marketfighter_parameter * tez) (expected : bool) =
  let _ = Test.set_source addr in
  test_entrypoint name (Test.transfer_to_contract marketfighter_contract op amount) expected

// Shop contract
let init_store_shop : shop_storage = {
    is_open = true;
    admin = (admin_address: address);
    items = Map.empty;
    bundles = Map.empty;
    owned_items = Big_map.empty        
}
let shop_addr, _, _ = Test.originate_from_file "shop.mligo" "main" [] (Test.eval init_store_shop) 0tez
let shop_typed_addr: (shop_parameter, shop_storage) typed_address = Test.cast_address shop_addr
let shop_contract = Test.to_contract shop_typed_addr


let test_shop (name : string) (addr, op, amount : address * shop_parameter * tez) (expected : bool) =
  let _ = Test.set_source addr in
  test_entrypoint name (Test.transfer_to_contract shop_contract op amount) expected

// Create abilities
let rl : rarity list =
  [Common;Common;Common;Common;Common;Common;Common;Common;Common;Common;
  Rare;Rare;Rare;Rare;Rare;
  Epic;Epic;Epic;
  Unique]
let _ = 
    (match Test.transfer_to_contract ability_contract (CreateAbility rl) 0tez with
    | Success _ -> true
    | Fail err -> Test.failwith err )
    |> Test.assert 

