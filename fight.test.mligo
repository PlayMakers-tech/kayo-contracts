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
    let marketfighter_typed_addr: (marketfighter_parameter, marketfighter_storage) typed_address = Test.cast_address marketfighter_addr in
    let marketfighter_contract = Test.to_contract marketfighter_typed_addr in

    let test_fight (name : string) (addr, op, amount : address * fight_parameter * tez) (expected : bool) =
        let _ = Test.set_source addr in
        test_entrypoint name (Test.transfer_to_contract fight_contract op amount) expected
    in

    let _ = Test.set_source admin_address in
    // fighter_contract
    let _ = Test.transfer_to_contract fighter_contract (SetFightAddr        fight_addr        ) 0tez in
    let _ = Test.transfer_to_contract fighter_contract (SetAbilityAddr      ability_addr     ) 0tez in
    let _ = Test.transfer_to_contract fighter_contract (SetAttributeAddr    attribute_addr   ) 0tez in
    let _ = Test.transfer_to_contract fighter_contract (SetMarketfighterAddr marketfighter_addr) 0tez in
    let _ = Test.transfer_to_contract fighter_contract (SetTournamentAddr   tournament_addr  ) 0tez in
    // marketfighter_contract
    let _ = Test.transfer_to_contract marketfighter_contract (SetFighterAddr fighter_addr) 0tez in

    
    // fight_contract
    let _ = print_topic "SetFighterAddr" in
    let _ = test_fight "Should not allow user to use SetFighterAddr entrypoint"  (alice_address, SetFighterAddr fighter_addr, 0tez) false in
    let _ = test_fight "Should allow admin to use SetFighterAddr entrypoint"     (admin_address, SetFighterAddr fighter_addr, 0tez) true in
    let _ = print_topic "SetTournamentAddr" in
    let _ = test_fight "Should not allow user to use SetTournamentAddr entrypoint"  (alice_address, SetTournamentAddr tournament_addr, 0tez) false in
    let _ = test_fight "Should allow admin to use SetTournamentAddr entrypoint"     (admin_address, SetTournamentAddr tournament_addr, 0tez) true in
    let _ = print_topic "SetAttributeAddr" in
    let _ = test_fight "Should not allow user to use SetAttributeAddr entrypoint"  (alice_address, SetAttributeAddr attribute_addr, 0tez) false in
    let _ = test_fight "Should allow admin to use SetAttributeAddr entrypoint"     (admin_address, SetAttributeAddr attribute_addr, 0tez) true in

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
    let _ = Test.set_source bob_address in
    let _ =  Test.transfer_to_contract fighter_contract (Mint) mint_fee in
    let bob_token : fighter_id = 5n in

    let _ = Test.set_source admin_address in
    let _ = Test.transfer_to_contract fighter_contract (RealMint (token1,0xfb0504030201fb0101010101,[4n;5n;6n])) 0tez in
    let _ = Test.transfer_to_contract fighter_contract (RealMint (token2,0xfb0604030201fb0101010101,[4n;5n;6n])) 0tez in
    let _ = Test.transfer_to_contract fighter_contract (RealMint (token3,0xfb1004030201fb0101010101,[1n;6n;7n])) 0tez in
    let _ = Test.transfer_to_contract fighter_contract (RealMint (alice_token,0xfb1404030201fb0101010101,[1n;6n;7n])) 0tez in
    let _ = Test.transfer_to_contract fighter_contract (RealMint (bob_token,0xfb1005030201fb0101010101,[1n;6n;7n])) 0tez in


    // ************ AddToQueue ********* //
    let _ = print_topic "AddToQueue" in
    
    let _ = test_fight "Should not allow the user to queue up another person's fighter" (bob_address, (AddToQueue (token1, NoStakeQ)), fight_fee) false in
    
    let _ = test_fight "Should allow the user to queue up a fighter" (alice_address, (AddToQueue (token1, NoStakeQ)), fight_fee) true in

    let d : fight_storage = Test.get_storage_of_address fight_addr |> Test.decompile in
    let f : fighter_storage = Test.get_storage_of_address fighter_addr |> Test.decompile in
    let f : fighter_data = Big_map.find 1n f.fighters in
    let _ = print_checkmark ((Set.mem 1n (Big_map.find NoStakeQ d.queues) && f.queue = NoStakeQ), true) in
    let _ = print_step "The first fighter is registered in the queue in fight_storage and fighter_storage" in

    let _ = Test.set_source alice_address in
    let _ = Test.transfer_to_contract marketfighter_contract (Sell (token3,16tez)) listing_fee in
    let _ = test_fight "Should not allow the user to AddToQueue a fighter when the fighter is on sale" (alice_address, (AddToQueue (token3, NoStakeQ)), fight_fee) false in
    let _ = Test.transfer_to_contract marketfighter_contract (Cancel token3) 0tez in

    let _ = Test.set_source admin_address in
    let _ = Test.transfer_to_contract fighter_contract (SetFighterState (token3, 0n, 1n, NotQueuing)) 0tez in
    let _ = test_fight "Should not allow the user to AddToQueue a fighter in tournament" (alice_address, (AddToQueue (token3, NoStakeQ)), fight_fee) false in
    
    let _ = test_fight "Should not allow the user to AddToQueue a fighter in queue" (alice_address, (AddToQueue (token1, NoStakeQ)), fight_fee) false in

    let _ = Test.set_source admin_address in
    let _ = Test.transfer_to_contract fighter_contract (SetFighterState (token3, 1n, 0n, NotQueuing)) 0tez in
    let _ = test_fight "Should not allow the user to AddToQueue a fighter in fight" (alice_address, (AddToQueue (token3, NoStakeQ)), fight_fee) false in
    let _ = Test.set_source admin_address in
    let _ = Test.transfer_to_contract fighter_contract (SetFighterState (token3, 0n, 0n, NotQueuing)) 0tez in

    let _ = Test.set_source alice_address in
    let _ = Test.transfer_to_contract fighter_contract (Mint) mint_fee in
    let token4 : fighter_id = 6n in
    let _ = test_fight "Should not allow the user to AddToQueue a fighter not fully minted" (alice_address, (AddToQueue (token4, NoStakeQ)), fight_fee) false in

    let _ = Test.set_source admin_address in
    let _ = Test.transfer_to_contract fighter_contract (RealMint (token4, 0xfb5554535251fb1111111111, [])) 0tez in

    let _ = Test.set_source alice_address in
    let _ = Test.transfer_to_contract fighter_contract (Fusion (token4, token3)) fusion_fee in    
    let _ = test_fight "Should not allow the user to AddToQueue an inactive fighter" (alice_address, (AddToQueue (token3, NoStakeQ)), fight_fee) false in


    // *************** SetFightFee *************** //
    let _ = print_topic "SetFightFee" in
    
    let _ = test_fight "Should not allow the user to SetFightFee" (alice_address, (SetFightFee 10tez), 0tez) false in

    let _ = test_fight "Should allow the admin to SetFightFee" (admin_address, (SetFightFee 10tez), 0tez) true in

    let d : fight_storage = Test.get_storage_of_address fight_addr |> Test.decompile in
    let _ = print_checkmark ((d.fight_fee = 10tez), true) in
    let _ = print_step "The fight fee is set to 10 tez" in 

    // *************** CancelQueue *************** //
    let _ = print_topic "CancelQueue" in

    let _ = test_fight "Should not allow the user to CancelQueue of a non-owned fighter" (bob_address, (CancelQueue token1), 0tez) false in
    
    let _ = test_fight "Should allow the user to CancelQueue" (alice_address, (CancelQueue token1), 0tez) true in
    
    let _ = test_fight "Should not allow the user to CancelQueue when the fighter is not in queue" (alice_address, (CancelQueue token1), 0tez) false in

    let d : fight_storage = Test.get_storage_of_address fight_addr |> Test.decompile in
    let _ = print_checkmark ((not (Set.mem token1 (Big_map.find NoStakeQ d.queues))), true) in
    let _ = print_step "The fighter is removed from the queue" in

    // *************** CreateFight *************** //
    let _ = print_topic "CreateFight" in

    let _ = test_fight "Should not allow the User to CreateFight" (alice_address, (CreateFight (alice_token,bob_token,3n,NoStake,120)), 0tez) false in

    let _ = Test.set_source alice_address in
    let _ = Test.transfer_to_contract fight_contract (AddToQueue (alice_token, NoStakeQ)) 10tez in

    let _ = Test.set_source bob_address in
    let _ = Test.transfer_to_contract fight_contract (AddToQueue (bob_token, FighterStakeQ)) 10tez in
    
    let _ = test_fight "Should not allow the admin to CreateFight with fighter from different queue" (admin_address, (CreateFight (alice_token,bob_token,3n,NoStake,120)), 0tez) false in

    let _ = Test.set_source bob_address in
    let _ = Test.transfer_to_contract fight_contract (CancelQueue bob_token) 0tez in

    let _ = Test.set_source admin_address in
    let _ = Test.transfer_to_contract fighter_contract (SetFighterListed (bob_token, true)) 0tez in
    
    let _ = test_fight "Should not allow the admin to CreateFight when one of the fighter is on sale" (admin_address, (CreateFight (alice_token,bob_token,3n,NoStake,120)), 0tez) false in
    
    let _ = Test.set_source admin_address in
    let _ = Test.transfer_to_contract fighter_contract (SetFighterListed (bob_token, false)) 0tez in

    // TODO Strange error to fix
    // let _ = Test.set_source admin_address in
    // let _ = Test.transfer_to_contract marketfighter_contract (Sell (bob_token,16tez)) listing_fee in
    
    // let _ = test_fight "Should not allow the admin to CreateFight when one of the fighter is on sale" (admin_address, (CreateFight (alice_token,bob_token,3n,NoStake)), 0tez) false in
    
    // let _ = Test.set_source bob_address in
    // let _ = Test.transfer_to_contract marketfighter_contract (Cancel bob_token) 0tez in
    
    let _ = Test.set_source bob_address in
    let _ = Test.transfer_to_contract fight_contract (AddToQueue (bob_token, NoStakeQ)) 10tez in

    let _ = test_fight "Should allow the admin to CreateFight" (admin_address, (CreateFight (alice_token,bob_token,3n,NoStake,120)), 0tez) true in

    let d : fight_storage = Test.get_storage_of_address fight_addr |> Test.decompile in
    let _ = print_checkmark ((Big_map.mem 1n d.fights), true) in
    let _ = print_step "The fight is created" in

    let d : fight_storage = Test.get_storage_of_address fight_addr |> Test.decompile in
    let d : fight_data = Big_map.find 1n d.fights in
    let _ = print_checkmark ((d.state = Initialized), true) in
    let _ = print_step "The fight state is Initialized" in

    // *************** SetStrategy *************** //
    let _ = print_topic "SetStrategy" in

    let _ = test_fight "Should not allow the user to SetStrategy of another fighter" (bob_address, (SetStrategy (1n, alice_token, 0x00)), 0tez) false in
    
    let _ = test_fight "Should allow the user to SetStrategy" (bob_address, (SetStrategy (1n, bob_token, 0x00)), 0tez) true in

    let _ = test_fight "Should allow the user to SetStrategy again" (bob_address, (SetStrategy (1n, bob_token, 0x00)), 0tez) true in
    
    // *************** ResolveRound *************** //
    let _ = print_topic "ResolveRound" in

    let _ = test_fight "Should not allow the user to ResolveRound" (alice_address, (ResolveRound (1n, 1n, 1, 0x00)), 0tez) false in
    
    let _ = test_fight "Should allow the admin to ResolveRound 1" (admin_address, (ResolveRound (1n, 1n, 1, 0x00)), 0tez) true in
    
    let d : fight_storage = Test.get_storage_of_address fight_addr |> Test.decompile in
    let d : fight_data = Big_map.find 1n d.fights in
    let _ = print_checkmark ((d.state = (OnGoing : fight_state)), true) in
    let _ = print_step "The fight state is OnGoing" in

    let _ = test_fight "Should not allow the admin to ResolveRound 3 before round 2" (admin_address, (ResolveRound (1n, 3n, 1, 0x00)), 0tez) false in
    
    let _ = test_fight "Should allow the admin to ResolveRound 2" (admin_address, (ResolveRound (1n, 2n, 1, 0x00)), 0tez) true in
    
    let _ = test_fight "Should allow the admin to ResolveRound 3" (admin_address, (ResolveRound (1n, 3n, 1, 0x00)), 0tez) true in
    
    let d : fight_storage = Test.get_storage_of_address fight_addr |> Test.decompile in
    let d : fight_data = Big_map.find 1n d.fights in
    let _ = print_checkmark ((d.state = (Finished : fight_state)), true) in
    let _ = print_step "The fight state is Finished" in
    
    let _ = test_fight "Should not allow the admin to ResolveRound 4" (admin_address, (ResolveRound (1n, 4n, 1, 0x00)), 0tez) false in

    // *************** SinkFees *************** //
    let _ = print_topic "SinkFees" in

    let _ = test_fight "Should not allow user to use SinkFees" (alice_address, (SinkFees (alice_address)), 0tez) false in
    
    let _ = test_fight "Should allow admin to use SinkFees" (admin_address, (SinkFees (alice_address)), 0tez) true in

    let _ = Test.println "" in
    
    ()