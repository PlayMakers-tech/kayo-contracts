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

    let test_tournament (name : string) (addr, op, amount : address * tournament_parameter * tez) (expected : bool) =
        let _ = Test.set_source addr in
        test_entrypoint name (Test.transfer_to_contract tournament_contract op amount) expected
    in

    let _ = Test.set_source admin_address in
    let _ = Test.transfer_to_contract fighter_contract (SetFightAddr        fight_addr        ) 0tez in
    let _ = Test.transfer_to_contract fighter_contract (SetAbilityAddr      ability_addr     ) 0tez in
    let _ = Test.transfer_to_contract fighter_contract (SetAttributeAddr    attribute_addr   ) 0tez in
    let _ = Test.transfer_to_contract fighter_contract (SetMarketfighterAddr marketfighter_addr) 0tez in
    let _ = Test.transfer_to_contract fighter_contract (SetTournamentAddr   tournament_addr  ) 0tez in
    let _ = Test.transfer_to_contract fight_contract   (SetAttributeAddr    attribute_addr   ) 0tez in
    let _ = Test.transfer_to_contract fight_contract   (SetTournamentAddr   tournament_addr  ) 0tez in

    let _ = print_topic "SetFighterAddr" in
    let _ = test_tournament "Should not allow user to use SetFighterAddr entrypoint"  (alice_address, SetFighterAddr fighter_addr, 0tez) false in
    let _ = test_tournament "Should allow admin to use SetFighterAddr entrypoint"     (admin_address, SetFighterAddr fighter_addr, 0tez) true in

    let _ = print_topic "SetFightAddr" in
    let _ = test_tournament "Should not allow user to use SetFightAddr entrypoint"  (alice_address, SetFightAddr fight_addr, 0tez) false in
    let _ = test_tournament "Should allow admin to use SetFightAddr entrypoint"     (admin_address, SetFightAddr fight_addr, 0tez) true in
    

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

    
    // ***************** SetTournamentFee *************** //
    let _ = print_topic "SetTournamentFee" in

    let _ = test_tournament "Should not allow user to use SetTournamentFee entrypoint"  (alice_address, SetTournamentFee 100tez, 0tez) false in

    let _ = test_tournament "Should allow admin to use SetTournamentFee entrypoint"     (admin_address, SetTournamentFee 100tez, 0tez) true in    

    let d : tournament_storage = Test.get_storage_of_address tournament_addr |> Test.decompile in
    let _ = print_checkmark (d.tournament_fee=100tez, true) in
    let _ = print_step "Tournament fee is updated in the memory" in

    // ***************** CreateTournament *************** //
    let _ = print_topic "CreateTournament" in

    let _ = test_tournament "Should not allow user to use CreateTournament entrypoint"  (alice_address, CreateTournament (NoStake,Tezos.get_now () + 180), 0tez) false in

    let _ = test_tournament "Should allow admin to use CreateTournament entrypoint"     (admin_address, CreateTournament (NoStake,Tezos.get_now () + 180), 0tez) true in

    let event : event_new_tournament list = Test.get_last_events_from tournament_typed_addr "newTournament" in
    let _ = match (List.head_opt event) with
      | Some (id, s, _) -> print_checkmark (id = 1n && s = NoStake, true)
      | None -> print_checkmark (false, true) in
    let _ = print_step "Should catch newTournament event" in

    let d : tournament_storage = Test.get_storage_of_address tournament_addr |> Test.decompile in
    let _ = print_checkmark (Big_map.mem 1n d.tournaments && Set.mem 1n d.active_tournaments && d.next_id = 2n, true) in
    let _ = print_step "Tournament is created in the memory" in

    // ***************** CancelTournament *************** //
    let _ = print_topic "CancelTournament" in

    let _ = test_tournament "Should not allow user to use CancelTournament entrypoint"  (alice_address, CancelTournament 1n, 0tez) false in

    let _ = test_tournament "Should allow admin to use CancelTournament entrypoint"     (admin_address, CancelTournament 1n, 0tez) true in

    let d : tournament_storage = Test.get_storage_of_address tournament_addr |> Test.decompile in
    let _ = print_checkmark (Set.mem 1n d.active_tournaments, false) in
    let _ = print_step "Tournament is cancelled in the memory" in

    // ***************** JoinTournament *************** //
    let _ = print_topic "JoinTournament" in

    let _ = Test.set_source admin_address in
    let _ = Test.transfer_to_contract tournament_contract (CreateTournament (NoStake,Tezos.get_now () + 180)) 0tez in

    let _ = test_tournament "Should not allow user to use JoinTournament with a non-owned fighter"  (alice_address, JoinTournament (2n, token2), 100tez) false in

    let _ = test_tournament "Should not allow user to use JoinTournament with an incorrect fee" (alice_address, JoinTournament (2n, token1), 10tez) false in
    
    let _ = Test.set_source admin_address in
    let _ = Test.transfer_to_contract fighter_contract (SetFighterState (token1, 0n, 0n, FighterStakeQ)) 0tez in
    let _ = test_tournament "Should not allow user to use JoinTournament if the fighter is in queue" (alice_address, JoinTournament (2n, token1), 100tez) false in

    let _ = Test.set_source admin_address in
    let _ = Test.transfer_to_contract fighter_contract (SetFighterState (token1, 0n, 1n, NotQueuing)) 0tez in
    let _ = test_tournament "Should not allow user to use JoinTournament if the fighter is already in tounament" (alice_address, JoinTournament (2n, token1), 100tez) false in

    let _ = Test.set_source admin_address in
    let _ = Test.transfer_to_contract fighter_contract (SetFighterState (token1, 1n, 0n, NotQueuing)) 0tez in
    let _ = test_tournament "Should not allow user to use JoinTournament if the fighter is in fight" (alice_address, JoinTournament (2n, token1), 100tez) false in
    
    let _ = Test.set_source admin_address in
    let _ = Test.transfer_to_contract fighter_contract (SetFighterListed (token1, true)) 0tez in
    let _ = test_tournament "Should not allow user to use JoinTournament if the fighter is listed" (alice_address, JoinTournament (2n, token1), 100tez) false in
    let _ = Test.set_source admin_address in
    let _ = Test.transfer_to_contract fighter_contract (SetFighterListed (token1, false)) 0tez in

    let _ = Test.set_source alice_address in
    let _ = Test.transfer_to_contract fighter_contract (Mint) 20tez in
    let token4 : fighter_id = 6n in
    let _ = test_tournament "Should not allow user to use JoinTournament if the fighter is not fully minted" (alice_address, JoinTournament (2n, token4), 100tez) false in
    
    let _ = Test.set_source admin_address in
    let _ = Test.transfer_to_contract_exn fighter_contract (SetFighterState (token1, 0n, 0n, NotQueuing)) 0tez in
    let _ = test_tournament "Should allow user to use JoinTournament" (alice_address, JoinTournament (2n, token1), 100tez) true in


    // ***************** GenerateTree *************** //
    let _ = print_topic "GenerateTree" in

    let _ = Test.set_source bob_address in
    let _ = Test.transfer_to_contract tournament_contract (JoinTournament (2n, token2)) 100tez in

    let _ = test_tournament "Should not allow user to use GenerateTree entrypoint"  (alice_address, GenerateTree (2n, 1n), 0tez) false in

    let _ = test_tournament "Should allow admin to use GenerateTree entrypoint"     (admin_address, GenerateTree (2n, 1n), 0tez) true in

    let event : event_generate_tree list = Test.get_last_events_from tournament_typed_addr "generatedTree" in
    let _ = match (List.head_opt event) with
      | Some (id) -> print_checkmark (id = 2n, true)
      | None -> print_checkmark (false, true) in
    let _ = print_step "Should catch generatedTree event catch" in

    // ***************** NextPhase *************** //
    let _ = print_topic "NextPhase" in

    // ***************** SinkFees *************** //
    let _ = print_topic "SinkFees" in

    let _ = test_tournament "Should not allow user to use SinkFees entrypoint"  (alice_address, SinkFees alice_address, 0tez) false in

    let _ = test_tournament "Should allow admin to use SinkFees entrypoint"     (admin_address, SinkFees alice_address, 0tez) true in
    
    let _ = Test.println "" in 
    ()