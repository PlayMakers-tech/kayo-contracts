#include "schema.mligo"
#include "error.mligo"
#include "utils.mligo"
#include "test.utils.mligo"

let get_fighter_data (id,d: fighter_id * fighter_storage) =
    Option.unopt_with_error (Big_map.find_opt id d.fighters) "Invalid fighter_id"
let get_fight_data (id,d: fight_id * fight_storage) =
    Option.unopt_with_error (Big_map.find_opt id d.fights) "Invalid fight_id"
let get_fighter_abilities (id,d: fighter_id * ability_storage) =
    Option.unopt_with_error (Big_map.find_opt id d.fighter_abilities) "Invalid fighter_id"
let get_attribute_data (id, d: fighter_id * attribute_storage) =
    Option.unopt_with_error (Big_map.find_opt id d.attributes) "Invalid fighter_id"

let test =
    let _ = Test.reset_state 4n [] in
    let admin_address = Test.nth_bootstrap_account 1 in
    let alice_address = Test.nth_bootstrap_account 2 in
    let bob_address   = Test.nth_bootstrap_account 3 in
    let dummy_address = ("tz3QE72w1vC613ZaeAZwW5fxwrBUQAstyaZy": address) in
    let mint_fee = 10tez in
    let fight_fee = 0.5tez in
    let tournament_fee = 0.7tez in
    let fusion_fee = 20tez in
    let listing_fee = 0.1tez in
    let min_listing_price = 8tez in
    let _ = Test.set_source admin_address in

    // Fighter contract
    let init_store : fighter_storage = {
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
    } in
    let fighter_addr, _, _ = Test.originate_from_file "fighter.mligo" "main" [] (Test.eval init_store) 0tez in
    let fighter_typed_addr: (fighter_parameter, fighter_storage) typed_address = Test.cast_address fighter_addr in
    let fighter_contract = Test.to_contract fighter_typed_addr in

    // Fight contract
    let init_store : fight_storage = {
        next_id = 1n;
        fight_fee = fight_fee;
        fighter_addr = (fighter_addr: address);
        tournament_addr = dummy_address;
        attribute_addr = dummy_address;
        admin = (admin_address: address);
        fights = Big_map.empty;
        fights_by_fighter = Big_map.empty;
        queues = Big_map.empty
    } in
    let fight_addr, _, _ = Test.originate_from_file "fight.mligo" "main" [] (Test.eval init_store) 0tez in
    let fight_typed_addr: (fight_parameter, fight_storage) typed_address = Test.cast_address fight_addr in
    let fight_contract = Test.to_contract fight_typed_addr in

    // Tournament contract
    let init_store : tournament_storage = {
        next_id = 1n;
        tournament_fee = tournament_fee;
        fight_addr = (fight_addr: address);
        fighter_addr = (fighter_addr: address);
        admin = (admin_address: address);
        active_tournaments = Set.empty;
        tournaments = Big_map.empty;
    } in
    let tournament_addr, _, _ = Test.originate_from_file "tournament.mligo" "main" [] (Test.eval init_store) 0tez in

    // Ability contract
	let init_store: ability_storage = {
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
	} in
    let ability_addr, _, _ = Test.originate_from_file "ability.mligo" "main" [] (Test.eval init_store) 0tez in
    let ability_typed_addr: (ability_parameter, ability_storage) typed_address = Test.cast_address ability_addr in
    let ability_contract = Test.to_contract ability_typed_addr in

    // Attribute contract
    let init_store: attribute_storage = {
        fight_addr = (fight_addr: address);
        fighter_addr = (fighter_addr: address);
        admin = (admin_address : address);
        skin_nodes = (1n, [(0x10,1n,1n)]);
        skin_leaves = (1n, [(0x00,2n,1n)]);
        attributes = Big_map.empty
    } in
    let attribute_addr, _, _ = Test.originate_from_file "attribute.mligo" "main" [] (Test.eval init_store) 0tez in

    // Marketfighter contract
    let init_store: marketfighter_storage = {
        is_open = true;
        listing_fee = listing_fee;
        fighter_addr = (fighter_addr: address);
        admin = (admin_address: address);
        min_price = min_listing_price;
        listed_offer = Set.empty;
        listed_sale = Set.empty;
        sells = Big_map.empty;
        buys = Big_map.empty;
    } in
    let marketfighter_addr, _, _ = Test.originate_from_file "marketfighter.mligo" "main" [] (Test.eval init_store) 0tez in


    let test_ability (name : string) (addr, op, amount : address * ability_parameter * tez) (expected : bool) =
        let _ = Test.set_source addr in
        test_entrypoint name (Test.transfer_to_contract ability_contract op amount) expected
    in

    let _ = Test.set_source admin_address in
    let _ = Test.transfer_to_contract fighter_contract (SetFightAddr        fight_addr        ) 0tez in
    let _ = Test.transfer_to_contract fighter_contract (SetAbilityAddr      ability_addr     ) 0tez in
    let _ = Test.transfer_to_contract fighter_contract (SetAttributeAddr    attribute_addr   ) 0tez in
    let _ = Test.transfer_to_contract fighter_contract (SetMarketfighterAddr marketfighter_addr) 0tez in
    let _ = Test.transfer_to_contract fighter_contract (SetTournamentAddr   tournament_addr  ) 0tez in
    let _ = Test.transfer_to_contract fight_contract   (SetAttributeAddr    attribute_addr   ) 0tez in
    let _ = Test.transfer_to_contract fight_contract   (SetTournamentAddr   tournament_addr  ) 0tez in

    let _ = print_topic ("SetFighterAddr") in
    let _ = test_ability "Should not allow user to use SetFighterAddr entrypoint"  (alice_address, SetFighterAddr fighter_addr, 0tez) false in
    let _ = test_ability "Should allow admin to use SetFighterAddr entrypoint"     (admin_address, SetFighterAddr fighter_addr, 0tez) true in

    let _ = Test.set_source alice_address in
    let _ =  Test.transfer_to_contract fighter_contract (Mint) mint_fee in
    let token1 : fighter_id = 1n in
    let _ = Test.set_source bob_address in
    let _ =  Test.transfer_to_contract fighter_contract (Mint) mint_fee in
    let token2 : fighter_id = 2n in
    let _ = Test.set_source alice_address in
    let _ =  Test.transfer_to_contract fighter_contract (Mint) mint_fee in
    let token3 : fighter_id = 3n in
    
    let _ = Test.set_source alice_address in
    let _ =  Test.transfer_to_contract fighter_contract (Mint) mint_fee in
    let alice_token : fighter_id = 4n in

    let _ = Test.set_source admin_address in
    let _ = Test.transfer_to_contract fighter_contract (RealMint (token1,0xfb0504030201fb0101010101,[4n;5n;6n])) 0tez in
    let _ = Test.transfer_to_contract fighter_contract (RealMint (token2,0xfb0604030201fb0101010101,[4n;5n;6n])) 0tez in
    let _ = Test.transfer_to_contract fighter_contract (RealMint (token3,0xfb1004030201fb0101010101,[1n;6n;7n])) 0tez in
    // let _ = Test.transfer_to_contract fighter_contract (RealMint (alice_token,0xfb1404030201fb0101010101,[1n;6n;7n])) 0tez in

    // *************** CreateAbility *************** //
    let _ = print_topic ("CreateAbility") in

    // Create abilities
    let rl : rarity list =
    	[Common;
		 Rare;
		 Epic;
		 Unique] in

    let _ = test_ability "Should not allow user to use CreateAbility entrypoint"  (alice_address, CreateAbility rl, 0tez) false in
    
    let _ = test_ability "Should allow admin to use CreateAbility entrypoint"  (admin_address, CreateAbility rl, 0tez) true in

    let d : ability_storage = Test.get_storage_of_address ability_addr |> Test.decompile in
    let abilitiesCheck : bool = Big_map.mem 1n d.abilities && Big_map.mem 2n d.abilities && Big_map.mem 3n d.abilities && Big_map.mem 4n d.abilities in
    let availableAbilitiesCheck : bool = Big_map.mem Common d.available_abilities && Big_map.mem Rare d.available_abilities && Big_map.mem Epic d.available_abilities && Big_map.mem Unique d.available_abilities in
    let probaCheck : bool = Map.mem Common d.proba_rarity && Map.mem Rare d.proba_rarity && Map.mem Epic d.proba_rarity && Map.mem Unique d.proba_rarity in
    let _ = print_checkmark (abilitiesCheck && availableAbilitiesCheck && d.next_id = 5n && probaCheck, true) in
    let _ = print_step "Should have created 4 abilities" in

    // *************** Mint *************** //
    let _ = print_topic ("Mint") in

    let _ = test_ability "Should not allow user to use Mint entrypoint"  (alice_address, Mint (token1, [1n]), 0tez) false in
    
    let _ = test_ability "Should not allow admin to use Mint entrypoint"  (admin_address, Mint (token1, [1n]), 0tez) false in


    // *************** SetProbaRarity *************** //
    let _ = print_topic ("SetProbaRarity") in
    
    let _ = test_ability "Should not allow user to use SetProbaRarity entrypoint"  (alice_address, SetProbaRarity (Common, 1n), 0tez) false in
    
    let _ = test_ability "Should allow admin to use SetProbaRarity entrypoint"  (admin_address, SetProbaRarity (Common, 1000000n), 0tez) true in
    
    let d : ability_storage = Test.get_storage_of_address ability_addr |> Test.decompile in
    let _ = print_checkmark (Map.find Common d.proba_rarity = 1000000n, true) in
    let _ = print_step "Should have set the proba of Common rarity to 1000000n" in


    // *************** Fusion *************** //
    let _ = print_topic ("Fusion") in
    
    let _ = test_ability "Should not allow user to use Fusion entrypoint"  (alice_address, Fusion (token1, token2, alice_token, [1n]), 0tez) false in
    
    let _ = test_ability "Should not allow admin to use Fusion entrypoint"  (admin_address, Fusion (token1, token2, alice_token, [1n]), 0tez) false in

    // *************** LearnAbility *************** //
    let _ = print_topic ("LearnAbility") in

    let _ = test_ability "Should not allow user to use LearnAbility entrypoint"  (alice_address, LearnAbility (token1, 2n), 0tez) false in

    let _ = test_ability "Should not allow admin to use LearnAbility entrypoint"  (admin_address, LearnAbility (token1, 2n), 0tez) false in

    // *************** ForgetAbility *************** //
    let _ = print_topic ("ForgetAbility") in

    let _ = test_ability "Should not allow user to use ForgetAbility entrypoint"  (alice_address, ForgetAbility (token1, 1n), 0tez) false in

    let _ = test_ability "Should not allow admin to use ForgetAbility entrypoint"  (admin_address, ForgetAbility (token1, 1n), 0tez) false in
    
    let _ = Test.println "" in
    ()