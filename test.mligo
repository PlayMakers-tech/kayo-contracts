#include "schema.mligo"
#include "error.mligo"
#include "utils.mligo"

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
    let _ = Test.set_source admin_address in

    // Fighter contract
    let init_store : fighter_storage = {
        next_id = 1n;
        mint_fee = mint_fee;
        fusion_fee = 20tez;
        list_fee = 1tez;
        fight_addr = dummy_address;
        tournament_addr = dummy_address;
        attribute_addr = dummy_address;
        ability_addr = dummy_address;
        admin = (admin_address : address);
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
    let tournament_typed_addr: (tournament_parameter, tournament_storage) typed_address = Test.cast_address tournament_addr in
    let tournament_contract = Test.to_contract tournament_typed_addr in

    // Ability contract
	let init_store: ability_storage = {
	    next_id = 1n;
	    fighter_addr = (fighter_addr: address);
        admin = (admin_address : address);
	    available_abilities = Big_map.literal [
	        (Common,    (Set.empty: ability_id set));
	        (Uncommon,  (Set.empty: ability_id set));
	        (Rare,      (Set.empty: ability_id set));
	        (Legendary, (Set.empty: ability_id set));
	        (Mythic,    (Set.empty: ability_id set));
	        (Unique,    (Set.empty: ability_id set))
	    ];
	    fighter_abilities = Big_map.empty;
	    abilities = Big_map.empty;
	    proba_rarity = Map.literal [
	        (Common,       1n);
	        (Uncommon,     0n);
	        (Rare,        10n);
	        (Legendary,   40n);
	        (Mythic,       0n);
	        (Unique,     160n)
	    ];
	    amount_rarity = Map.literal [
	        (Common,       0n);
	        (Uncommon,     0n);
	        (Rare,      1000n);
	        (Legendary,  100n);
	        (Mythic,       0n);
	        (Unique,       1n)
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
        skin_node_limit = 1n;
	    attributes = Big_map.empty
	} in
    let attribute_addr, _, _ = Test.originate_from_file "attribute.mligo" "main" [] (Test.eval init_store) 0tez in
    let attribute_typed_addr: (attribute_parameter, attribute_storage) typed_address = Test.cast_address attribute_addr in
    let attribute_contract = Test.to_contract attribute_typed_addr in

    // Cross-set contract addresses    
    let _ = 
        (match Test.transfer_to_contract fighter_contract (SetFightAddr fight_addr) 0tez with
        | Success _ -> true
        | _ -> false )
        |> Test.assert 
    in
    let _ = 
        (match Test.transfer_to_contract fighter_contract (SetAbilityAddr ability_addr) 0tez with
        | Success _ -> true
        | _ -> false )
        |> Test.assert 
    in
    let _ = 
        (match Test.transfer_to_contract fighter_contract (SetAttributeAddr attribute_addr) 0tez with
        | Success _ -> true
        | _ -> false )
        |> Test.assert 
    in
    let _ = 
        (match Test.transfer_to_contract fight_contract (SetAttributeAddr attribute_addr) 0tez with
        | Success _ -> true
        | _ -> false )
        |> Test.assert 
    in
    let _ = 
        (match Test.transfer_to_contract fight_contract (SetTournamentAddr tournament_addr) 0tez with
        | Success _ -> true
        | _ -> false )
        |> Test.assert 
    in

    // Dump function for probing    
    let dump (id : fighter_id ) =
        let df : fighter_storage = Test.get_storage_of_address fighter_addr |> Test.decompile in
        let da : ability_storage = Test.get_storage_of_address ability_addr |> Test.decompile in
        let ds : attribute_storage = Test.get_storage_of_address attribute_addr |> Test.decompile in
        let _ = Test.log (get_fighter_data (id, df)) in
        let _ = Test.log (get_fighter_abilities (id, da)) in
        let _ = Test.log (get_attribute_data (id, ds)) in
        () in
    let dump_fight (id : fight_id ) =
        let d : fight_storage = Test.get_storage_of_address fight_addr |> Test.decompile in
        let _ = Test.log (get_fight_data (id, d)) in
        () in



    // Create abilities
    let rl : rarity list =
    	[Common;Common;Common;Common;Common;Common;Common;Common;Common;Common;
		 Rare;Rare;Rare;Rare;Rare;
		 Legendary;Legendary;Legendary;
		 Unique] in
    let _ = 
        (match Test.transfer_to_contract ability_contract (CreateAbility rl) 0tez with
        | Success _ -> true
        | Fail err -> Test.failwith err )
        |> Test.assert 
    in


    // Minting 
    let _ = Test.set_source alice_address in
    let _ = 
        (match Test.transfer_to_contract fighter_contract (Mint) mint_fee with
        | Success _ -> true
        | Fail err -> Test.failwith err )
        |> Test.assert 
    in
    let alice_token : fighter_id = 1n in
    let _ = Test.set_source bob_address in
    let _ = 
        (match Test.transfer_to_contract fighter_contract (Mint) mint_fee with
        | Success _ -> true
        | Fail err -> Test.failwith err )
        |> Test.assert 
    in
    let bob_token : fighter_id = 2n in
    let d : fighter_storage = Test.get_storage_of_address fighter_addr |> Test.decompile in
    let _ = Test.assert (d.next_id = 3n) in

    // Some probing
    let _ = Test.println "Probing Alice and Bob mints" in
    let _ = dump alice_token in
    let _ = dump bob_token in
    let _ = Test.println "" in

    // Add to queue
    let _ = Test.set_source alice_address in
    let _ = 
        (match Test.transfer_to_contract fight_contract (AddToQueue (alice_token,NoStakeQ)) fight_fee with
        | Success _ -> true
        | Fail err -> Test.failwith err )
        |> Test.assert 
    in

    let _ = Test.set_source bob_address in
    let _ = Test.transfer_to_contract fight_contract (AddToQueue (bob_token,NoStakeQ)) fight_fee in

    // Create Fight
    let _ = Test.set_source admin_address in
    let _ = 
        (match Test.transfer_to_contract fight_contract (CreateFight (alice_token,bob_token,3n,NoStake)) 0tez with
        | Success _ -> true
        | Fail err -> Test.failwith err )
        |> Test.assert 
    in
    let d : fight_storage = Test.get_storage_of_address fight_addr |> Test.decompile in
    let fight_id : fight_id = 1n in
    let _ = Test.assert (d.next_id = 2n) in

    let _ = Test.println "Probing Alice and Bob in fight" in
    let _ = dump alice_token in
    let _ = dump bob_token in
    let _ = Test.println "" in

    // Go through rounds
    let _ = 
        (match Test.transfer_to_contract fight_contract (ResolveRound (fight_id, 1n,  1, 0x00)) 0tez with
        | Success _ -> true
        | Fail err -> Test.failwith err )
        |> Test.assert 
    in
    let _ = 
        (match Test.transfer_to_contract fight_contract (ResolveRound (fight_id, 2n, -1, 0x00)) 0tez with
        | Success _ -> true
        | Fail err -> Test.failwith err )
        |> Test.assert 
    in
    let _ = 
        (match Test.transfer_to_contract fight_contract (ResolveRound (fight_id, 3n,  1, 0x00)) 0tez with
        | Success _ -> true
        | Fail err -> Test.failwith err )
        |> Test.assert 
    in

    let _ = Test.println "Probing the fight" in
    let _ = dump_fight fight_id in
    let _ = Test.println "" in

    // Grant free xp to Alice's token
    let _ = 
        (match Test.transfer_to_contract attribute_contract (EarnXP (alice_token, 5n)) 0tez with
        | Success _ -> true
        | Fail err -> Test.failwith err )
        |> Test.assert 
    in

    let _ = Test.println "Probing Alice's level up" in
    let _ = dump alice_token in
    let _ = Test.println "" in

    // Alice goes on a TezStakeQ and cancel
    let stake = 1tez in
    let _ = Test.set_source alice_address in
    let _ = 
        (match Test.transfer_to_contract fight_contract (AddToQueue (alice_token,TezStakeQ stake)) (fight_fee+stake) with
        | Success _ -> true
        | Fail err -> Test.failwith err )
        |> Test.assert 
    in
    let _ = Test.println "Probing Alice in her queue" in
    let _ = dump alice_token in
    let _ = Test.println "" in
    let _ = 
        (match Test.transfer_to_contract fight_contract (CancelQueue alice_token) 0tez with
        | Success _ -> true
        | Fail err -> Test.failwith err )
        |> Test.assert 
    in

    // Admin creates a new tournament
    let now : timestamp = Tezos.get_now () in
    let stamp: timestamp = now + 180 in    
    let _ = Test.set_source admin_address in
    let _ = 
        (match Test.transfer_to_contract tournament_contract (CreateTournament (NoStake,stamp)) 0tez with
        | Success _ -> true
        | Fail err -> Test.failwith err )
        |> Test.assert 
    in

    ()