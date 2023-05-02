#include "schema.mligo"
#include "error.mligo"
#include "utils.mligo"
#include "test.utils.mligo"
#include "event.mligo"

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

    let test_fighter (name : string) (addr, op, amount : address * fighter_parameter * tez) (expected : bool) =
        let _ = Test.set_source addr in
        test_entrypoint name (Test.transfer_to_contract fighter_contract op amount) expected
    in
    
    let test_fight (name : string) (addr, op, amount : address * fight_parameter * tez) (expected : bool) =
        let _ = Test.set_source addr in
        test_entrypoint name (Test.transfer_to_contract fight_contract op amount) expected
    in

    let _ = print_topic "SetFightAddr" in
    let _ = test_fighter "Should not allow user to use SetFightAddr entrypoint"  (alice_address, SetFightAddr fight_addr, 0tez) false in
    let _ = test_fighter "Should allow admin to use SetFightAddr entrypoint"     (admin_address, SetFightAddr fight_addr, 0tez) true in


    let _ = print_topic "SetAbilityAddr" in
    let _ = test_fighter "Should not allow user to use SetAbilityAddr entrypoint"  (alice_address, SetAbilityAddr ability_addr, 0tez) false in
    let _ = test_fighter "Should allow admin to use SetAbilityAddr entrypoint"     (admin_address, SetAbilityAddr ability_addr, 0tez) true in

    let _ = print_topic "SetAttributeAddr" in
    let _ = test_fighter "Should not allow user to use SetAttributeAddr entrypoint"  (alice_address, SetAttributeAddr attribute_addr, 0tez) false in
    let _ = test_fighter "Should allow admin to use SetAttributeAddr entrypoint"     (admin_address, SetAttributeAddr attribute_addr, 0tez) true in

    let _ = print_topic "SetMarketfighterAddr" in
    let _ = test_fighter "Should not allow user to use SetMarketfighterAddr entrypoint"  (alice_address, SetMarketfighterAddr marketfighter_addr, 0tez) false in
    let _ = test_fighter "Should allow admin to use SetMarketfighterAddr entrypoint"     (admin_address, SetMarketfighterAddr marketfighter_addr, 0tez) true in
    
    let _ = print_topic "SetTournamentAddr" in
    let _ = test_fighter "Should not allow user to use SetTournamentAddr entrypoint"  (alice_address, SetTournamentAddr tournament_addr, 0tez) false in
    let _ = test_fighter "Should allow admin to use SetTournamentAddr entrypoint"     (admin_address, SetTournamentAddr tournament_addr, 0tez) true in

    let _ = print_topic "SetAttributeAddr (fight_contract)" in
    let _ = test_fight "Should not allow user to use SetAttributeAddr entrypoint"  (alice_address, SetAttributeAddr attribute_addr, 0tez) false in
    let _ = test_fight "Should allow admin to use SetAttributeAddr entrypoint"     (admin_address, SetAttributeAddr attribute_addr, 0tez) true in

    let _ = print_topic "SetTournamentAddr (fight_contract)" in
    let _ = test_fight "Should not allow user to use SetTournamentAddr entrypoint"  (alice_address, SetTournamentAddr tournament_addr, 0tez) false in
    let _ = test_fight "Should allow admin to use SetTournamentAddr entrypoint"     (admin_address, SetTournamentAddr tournament_addr, 0tez) true in


    // Create abilities
    let rl : rarity list =
    	[Common;Common;Common;Common;Common;Common;Common;Common;Common;Common;
		 Rare;Rare;Rare;Rare;Rare;
		 Epic;Epic;Epic;
		 Unique] in
    let _ = 
        (match Test.transfer_to_contract ability_contract (CreateAbility rl) 0tez with
        | Success _ -> true
        | Fail err -> Test.failwith err )
        |> Test.assert 
    in


    // ******************** Mint test ******************** // 
    let _ = print_topic "Mint" in

    let _ = test_fighter "Should allow user to mint a fighter" (alice_address, Mint, mint_fee) true in
    let alice_token1 : fighter_id = 1n in
    
    let d : fighter_storage = Test.get_storage_of_address fighter_addr |> Test.decompile in
    let _ = print_checkmark (Big_map.mem 1n d.fighters, true) in
    let _ = print_step ("Fighter 1n is indeed present in memory") in

    let event : event_minting list = Test.get_last_events_from fighter_typed_addr "minting" in
    let _ = match (List.head_opt event) with
      | Some (id) -> print_checkmark (id = alice_token1, true)
      | None -> print_checkmark (false, true) in
    let _ = print_step "Should catch minting event during the mint" in

    let _ = test_fighter "Should not allow user to mint a fighter with less tez than expected" (bob_address, Mint, 1tez) false in
    
    let _ = test_fighter "Should not allow user to mint a fighter with more tez than expected" (bob_address, Mint, 20tez) false in

    let _ = print_checkmark (Big_map.mem 2n d.fighters, false) in
    let _ = print_step ("Fighter 2n is indeed absent from the memory") in

    let d : fighter_storage = Test.get_storage_of_address fighter_addr |> Test.decompile in
    let _ = print_checkmark (d.next_id = 2n, true) in
    let _ = print_step ("The next id is indeed 2n") in


    // ******************** RealMint test ******************** // 
    let _ = print_topic "RealMint" in

    let _ = test_fighter "Should not allow user to realMint a fighter" (alice_address, (RealMint (alice_token1, 0xfb0504030201fb0101010101, [])), 0tez) false in
    
    let _ = test_fighter "Should not allow admin to realMint a fighter that is not minted" (admin_address, (RealMint (50n, 0xfb0504030201fb0101010101, [])), 0tez) false in
    
    let d : ability_storage = Test.get_storage_of_address ability_addr |> Test.decompile in
    let _ = print_checkmark (Big_map.mem 1n d.fighter_abilities, false) in
    let _ = print_step ("Fighter 1n is absent in fighter_abilities") in
    
    let d : attribute_storage = Test.get_storage_of_address attribute_addr |> Test.decompile in
    let _ = print_checkmark (Big_map.mem 1n d.attributes, false) in
    let _ = print_step ("Fighter 1n is absent from the attributes") in

    let _ = test_fighter "Should allow admin to realMint a fighter" (admin_address, (RealMint (alice_token1, 0xfb0504030201fb0101010101, [1n])), 0tez) true in
    
    let event : event_minted list = Test.get_last_events_from fighter_typed_addr "minted" in
    let _ = match (List.head_opt event) with
      | Some (id) -> print_checkmark (id = alice_token1, true)
      | None -> print_checkmark (false, true) in
    let _ = print_step "Should catch minted event" in

    let _ = test_fighter "Should not allow admin to realMint agin a fighter" (admin_address, (RealMint (alice_token1, 0xfb0504030201fb0101010101, [])), 0tez) false in

    let d : ability_storage = Test.get_storage_of_address ability_addr |> Test.decompile in
    let _ = print_checkmark (Set.mem 1n (Big_map.find 1n d.fighter_abilities), true) in
    let _ = print_step ("Fighter 1n is present in fighter_abilities and is correct") in
    
    let d : attribute_storage = Test.get_storage_of_address attribute_addr |> Test.decompile in
    let d : attribute_data = Big_map.find 1n d.attributes in
    let _ = print_checkmark (d.id = 1n && d.crypted = 0xfb0504030201fb0101010101 && d.xp = 0n, true) in
    let _ = print_step ("Fighter 1n is present in the attributes and has the right value") in

    let d : ability_storage = Test.get_storage_of_address ability_addr |> Test.decompile in
    let _ = print_checkmark(Set.mem 1n (Big_map.find 1n d.fighter_abilities), true)in
    let _ = print_step ("Fighter 1n is present in fighter_abilities and got the correct abilities") in


    // ******************** SetName test ******************** // 
    let _ = print_topic "SetName" in

    let _ = test_fighter "Should allow user to set a name for his fighter" (alice_address, (SetName (alice_token1,"Alicia")), 0tez) true in

    let d : fighter_storage = Test.get_storage_of_address fighter_addr |> Test.decompile in
    let d : fighter_data = Big_map.find alice_token1 d.fighters in
    let _ = print_checkmark (d.name = "Alicia", true) in
    let _ = print_step ("The name has been updated") in

    let _ = test_fighter "Should not allow user to change the name of another user's fighter" (bob_address, (SetName (alice_token1,"foo")), 0tez) false in

    let _ = test_fighter "Should allow user to set an empty name for his fighter" (alice_address, (SetName (alice_token1,"")), 0tez) true in
    
    let d : fighter_storage = Test.get_storage_of_address fighter_addr |> Test.decompile in
    let d : fighter_data = Big_map.find alice_token1 d.fighters in
    let _ = print_checkmark (d.name = "", true) in
    let _ = print_step ("The name has been updated") in
    
    let _ = test_fighter "Should not allow user to put a name exceeding 32 characters" (alice_address, (SetName (alice_token1,"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")), 0tez) false in


    // ******************** SetMintFee test ******************** // 
    let _ = print_topic "SetMintFee" in

    let _ = test_fighter "Should not allow user to change the price of minting" (alice_address, (SetMintFee mint_fee), 0tez) false in
    
    let _ = test_fighter "Should allow admin to change the price of minting" (admin_address, (SetMintFee 20tez), 0tez) true in

    let d : fighter_storage = Test.get_storage_of_address fighter_addr |> Test.decompile in
    let _ = print_checkmark (d.mint_fee = 20tez, true) in
    let _ = print_step ("The mint fee has been updated") in

    // ******************** SetFusionFee test ******************** // 
    let _ = print_topic "SetFusionFee" in

    let _ = test_fighter "Should not allow user to change the price of fusion fee" (alice_address, (SetFusionFee 0tez), 0tez) false in
    
    let _ = test_fighter "Should allow admin to change the price of fusion fee" (admin_address, (SetFusionFee 30tez), 0tez) true in
   
    let d : fighter_storage = Test.get_storage_of_address fighter_addr |> Test.decompile in
    let _ = print_checkmark (d.fusion_fee = 30tez, true) in
    let _ = print_step ("The fusion fee has been updated") in

    let _ = Test.set_source alice_address in
    let _ = Test.transfer_to_contract fighter_contract (Mint) 20tez in
    let alice_token2 : fighter_id = 2n in
    let _ = Test.set_source admin_address in
    let _ = Test.transfer_to_contract fighter_contract (RealMint (alice_token2, 0xfb0504030201fb0101010101, [])) 0tez in
    
    let _ = Test.set_source bob_address in
    let _ = Test.transfer_to_contract fighter_contract (Mint) 20tez in
    let bob_token : fighter_id = 3n in
    let _ = Test.set_source admin_address in
    let _ = Test.transfer_to_contract fighter_contract (RealMint (bob_token, 0xfb0504032201fb0101010101, [])) 0tez in


    // ******************** Fusion test ******************** // 
    let _ = print_topic "Fusion" in
    
    let alice_token3 : fighter_id = 4n in    

    let _ = test_fighter "Should not allow user to make a fusion with the wrong fee" (alice_address, (Fusion (alice_token1, alice_token2)), fusion_fee) false in
    
    let _ = test_fighter "Should allow user to make a fusion of two fighters with the correct fee" (alice_address, (Fusion (alice_token1, alice_token2)), 30tez) true in
    
    let event : event_minting list = Test.get_last_events_from fighter_typed_addr "minting" in
    let _ = match (List.head_opt event) with
      | Some (id) -> print_checkmark (id = alice_token3, true)
      | None -> print_checkmark (false, true) in
    let _ = print_step "Should catch minting event during fusion" in

    let _ = test_fighter "Should not allow the user to merge two fighters during a fusion" (alice_address, (Fusion (alice_token1, alice_token2)), 30tez) false in

    let d : fighter_storage = Test.get_storage_of_address fighter_addr |> Test.decompile in
    let d1 : fighter_data = Big_map.find alice_token1 d.fighters in
    let d2 : fighter_data = Big_map.find alice_token2 d.fighters in
    let _ = print_checkmark (d1.inactive && d2.inactive, true) in
    let _ = print_step "The father and mother tokens are well inactive" in

    let _ = Test.set_source admin_address in
    let _ = Test.transfer_to_contract fighter_contract (RealMint (alice_token3, 0xfb5554535251fb1111111111, [])) 0tez in

    let d : fighter_storage = Test.get_storage_of_address fighter_addr |> Test.decompile in
    let d : fighter_data = Big_map.find alice_token3 d.fighters in
    let _ = print_checkmark (d.father = 1n && d.mother = 2n, true) in
    let _ = print_step "The child has the right father and mother" in

    let _ = test_fighter "Should not allow the user to perform a fusion with one of the two inactive parents." (alice_address, (Fusion (alice_token1, alice_token3)), 30tez) false in
    
    let _ = test_fighter "Should not allow the user to perform a fusion with one of the two inactive parents." (alice_address, (Fusion (alice_token3, alice_token2)), 30tez) false in

    // ******************** SinkFees test ******************** // 
    let _ = print_topic "SinkFees" in

    let _ = test_fighter "Should not allow user to use SinkFees" (alice_address, (SinkFees (alice_address)), 0tez) false in
    
    let _ = test_fighter "Should allow admin to use SinkFees" (admin_address, (SinkFees (alice_address)), 0tez) true in


    // ******************** SetFighterListed test ******************** // 
    let _ = print_topic "SetFighterListed" in

    let _ = test_fighter "Should not allow user to use SetFighterListed" (alice_address, (SetFighterListed (alice_token1, false)), 0tez) false in
    
    let _ = test_fighter "Should allow admin to use SetFighterListed on an active fighter" (admin_address, (SetFighterListed (alice_token3, true)), 0tez) true in
    
    let _ = test_fighter "Should not allow admin to use SetFighterListed on an inactive fighter." (admin_address, (SetFighterListed (alice_token1, true)), 0tez) false in
    let _ = Test.transfer_to_contract fighter_contract (SetFighterListed (alice_token1, false)) 30tez in
    
    let d : fighter_storage = Test.get_storage_of_address fighter_addr |> Test.decompile in
    let d : fighter_data = Big_map.find alice_token3 d.fighters in
    let _ = print_checkmark (d.listed, true) in
    let _ = print_step "The player is listed in the memory" in

    let _ = Test.transfer_to_contract fighter_contract (SetFighterListed (alice_token3, false)) 30tez in

    // ******************** SetFighterState test ******************** // 
    let _ = print_topic "SetFighterState" in

    let _ = test_fighter "Should not let user use SetFighterState" (alice_address, (SetFighterState (alice_token3,0n,0n,NotQueuing)), 30tez) false in

    let _ = test_fighter "Should let admin use SetFighterState" (admin_address, (SetFighterState (alice_token3,0n,0n,FighterStakeQ)), 30tez) true in

    let d : fighter_storage = Test.get_storage_of_address fighter_addr |> Test.decompile in
    let d : fighter_data = Big_map.find alice_token3 d.fighters in
    let _ = print_checkmark (d.queue = FighterStakeQ, true) in
    let _ = print_step "The queue has been correctly updated" in

    let _ = Test.transfer_to_contract fighter_contract (SetFighterState (alice_token3,0n,0n,NotQueuing)) 30tez in


    // ******************** Transfer test ******************** // 
    let _ = print_topic "Transfer" in

    let _ = test_fighter "Should not allow the user to steal a fighter" (bob_address, (Transfer (alice_token3, bob_address)), 30tez) false in

    let _ = test_fighter "Should allow the user to transfer a fighter" (alice_address, (Transfer (alice_token3, bob_address)), 30tez) true in

    let d : fighter_storage = Test.get_storage_of_address fighter_addr |> Test.decompile in
    let d : fighter_data = Big_map.find alice_token3 d.fighters in
    let _ = print_checkmark (d.owner = bob_address, true) in
    let _ = print_step "The owner as correctly been updated" in

    let event : event_transfer list = Test.get_last_events_from fighter_typed_addr "transfer" in
    let _ = match (List.head_opt event) with
      | Some (id, f, t) -> print_checkmark (id = alice_token3 && f = alice_address && t = bob_address, true)
      | None -> print_checkmark (false, true) in
    let _ = print_step "Should catch transfer event" in

    let _ = test_fighter "Should not allow the user to transfer an inactive fighter" (alice_address, (Transfer (alice_token1, bob_address)), 30tez) false in

    let _ = Test.set_source admin_address in
    let _ = Test.transfer_to_contract fighter_contract (SetFighterListed (alice_token3, true)) 0tez in

    let _ = test_fighter "Should not allow the user to transfer a listed fighter" (bob_address, (Transfer (alice_token3, bob_address)), 30tez) false in
    
    let _ = Test.set_source admin_address in
    let _ = Test.transfer_to_contract fighter_contract (SetFighterListed (alice_token3, false)) 0tez in

    let _ = Test.set_source admin_address in
    let _ = Test.transfer_to_contract fighter_contract (SetFighterState (alice_token3, 0n, 0n, FighterStakeQ)) 30tez in
    let _ = test_fighter "Should not allow the user to transfer a fighter in queue" (bob_address, (Transfer (alice_token3, bob_address)), 30tez) false in
    
    let _ = Test.set_source admin_address in
    let _ = Test.transfer_to_contract fighter_contract (SetFighterState (alice_token3, 0n, 1n, NotQueuing)) 30tez in
    let _ = test_fighter "Should not allow the user to transfer a fighter in tournament" (bob_address, (Transfer (alice_token3, bob_address)), 30tez) false in

    let _ = Test.set_source admin_address in
    let _ = Test.transfer_to_contract fighter_contract (SetFighterState (alice_token3, 1n, 0n, NotQueuing)) 30tez in
    let _ = test_fighter "Should not allow the user to transfer a fighter in fight" (bob_address, (Transfer (alice_token3, bob_address)), 30tez) false in

    let _ = Test.set_source alice_address in
    let _ = Test.transfer_to_contract fighter_contract (Mint) 20tez in
    let token4 : fighter_id = 5n in
    let _ = test_fighter "Should not allow the user to transfer a fighter not fully minted" (alice_address, (Transfer (token4, bob_address)), 30tez) false in


    let _ = Test.println "" in

    ()
