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
    let marketfighter_typed_addr: (marketfighter_parameter, marketfighter_storage) typed_address = Test.cast_address marketfighter_addr in
    let marketfighter_contract = Test.to_contract marketfighter_typed_addr in

    let test_marketfighter (name : string) (addr, op, amount : address * marketfighter_parameter * tez) (expected : bool) =
        let _ = Test.set_source addr in
        test_entrypoint name (Test.transfer_to_contract marketfighter_contract op amount) expected
    in

    let test_fighter (name : string) (addr, op, amount : address * fighter_parameter * tez) (expected : bool) =
        let _ = Test.set_source addr in
        test_entrypoint name (Test.transfer_to_contract fighter_contract op amount) expected
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
    let _ = test_marketfighter "Should not allow user to use SetFighterAddr entrypoint"  (alice_address, SetFighterAddr fighter_addr, 0tez) false in
    let _ = test_marketfighter "Should allow admin to use SetFighterAddr entrypoint"     (admin_address, SetFighterAddr fighter_addr, 0tez) true in

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
    let token4 : fighter_id = 4n in

    let _ = Test.set_source admin_address in
    let _ = Test.transfer_to_contract fighter_contract (RealMint (token1,0xfb0504030201fb0101010101,[4n;5n;6n])) 0tez in
    let _ = Test.transfer_to_contract fighter_contract (RealMint (token2,0xfb0604030201fb0101010101,[4n;5n;6n])) 0tez in
    let _ = Test.transfer_to_contract fighter_contract (RealMint (token3,0xfb1004030201fb0101010101,[1n;6n;7n])) 0tez in
    let _ = Test.transfer_to_contract fighter_contract (RealMint (token4,0xfb1004030201fb0001010101,[1n;6n;7n])) 0tez in


    // ******************** SetMarketOpen test ******************** // 
    let _ = print_topic "SetMarketOpen" in 

    let _ = test_marketfighter "Should allow user to close the market" (alice_address, (SetMarketOpen false), 0tez) false in

    let _ = test_marketfighter "Should allow admin to close the market" (admin_address, (SetMarketOpen false), 0tez) true in

    let d : marketfighter_storage = Test.get_storage_of_address marketfighter_addr |> Test.decompile in
    let _ = print_checkmark (d.is_open = false, true) in
    let _ = print_step "Market is closed in memory" in

    let _ = Test.transfer_to_contract marketfighter_contract (SetMarketOpen true) 0tez in

    // ******************** Buy test ******************** // 
    let _ = print_topic "Buy" in 

    let bal1 = Test.get_balance bob_address in

    // this step cost 1tez gas of fee
    let _ = test_marketfighter "Should allow user to make a buy offer" (bob_address, Buy (token1,12tez), 12tez) true in

    let event : event_buying list = Test.get_last_events_from marketfighter_typed_addr "buying" in
    let _ = match (List.head_opt event) with
      | Some (id) -> print_checkmark (id = token1, true)
      | None -> print_checkmark (false, true) in
    let _ = print_step "Should catch buying event" in

    // this step cost 1tez gas of fee
    let _ = test_marketfighter "Should allow user to make a new buy offer" (bob_address, Buy (token1,15tez), 15tez) true in
    
    let bal2 = Test.get_balance bob_address in

    // In ligo test each transfer cost 1tez of gas
    let expected_fee = 15tez + 1tez + 1tez in
    let _ = print_checkmark (bal1 < bal2 + expected_fee + 50000mutez && bal1 > bal2 + expected_fee, true) in
    let _ = print_step "Correct balance and fee during the transfert" in

    let d : marketfighter_storage = Test.get_storage_of_address marketfighter_addr |> Test.decompile in
    let e = Map.find bob_address (Big_map.find 1n d.buys) in
    let _ = print_checkmark (e.price=15tez && Set.mem 1n d.listed_offer, true) in
    let _ = print_step "Buy offer is stored in memory" in

    let _ = test_marketfighter "Should not allow user to make an offer on their own fighter" (bob_address, Buy (token2,15tez), 15tez) false in

    let _ = test_marketfighter "Should allow user to make an offer even if the market is closed" (alice_address, Buy (token2,15tez), 15tez) true in


    // ******************** Sell test ******************** // 
    let _ = print_topic "Sell" in 

    let _ = test_marketfighter "Should not allow user to sell the fighter of another user" (bob_address, Sell (token3, 15tez), listing_fee) false in

    let _ = test_marketfighter "Should not allow user to sell a fighter below the minimum price" (alice_address, Sell (token3, 1mutez), listing_fee) false in
    
    let _ = test_marketfighter "Should allow user to sell a fighter" (alice_address, Sell (token3, 15tez), listing_fee) true in

    let event : event_selling list = Test.get_last_events_from marketfighter_typed_addr "selling" in
    let _ = match (List.head_opt event) with
      | Some (id, price) -> print_checkmark (id = token3 && price = 15tez, true)
      | None -> print_checkmark (false, true) in
    let _ = print_step "Should catch selling event" in

    let _ = test_marketfighter "Should allow user to sell again a fighter to change the price" (alice_address, Sell (token3, 12tez), listing_fee) true in

    let d : marketfighter_storage = Test.get_storage_of_address marketfighter_addr |> Test.decompile in
    let e = Big_map.find token3 d.sells in
    let _ = print_checkmark (e.price=12tez, true) in
    let _ = print_step "Sell offer is stored in memory" in

    let _ = Test.set_source admin_address in
    let _ = Test.transfer_to_contract fighter_contract (SetFighterState (token1, 0n, 0n, FighterStakeQ)) 30tez in
    let _ = test_marketfighter "Should not allow user to sell a fighter in queue" (alice_address, Sell (token1, 10tez), listing_fee) false in
    
    let _ = Test.set_source admin_address in
    let _ = Test.transfer_to_contract fighter_contract (SetFighterState (token1, 0n, 1n, NotQueuing)) 30tez in
    let _ = test_marketfighter "Should not allow user to sell a fighter in tournament" (alice_address, Sell (token1, 10tez), listing_fee) false in
    
    let _ = Test.set_source admin_address in
    let _ = Test.transfer_to_contract fighter_contract (SetFighterState (token1, 1n, 0n, NotQueuing)) 30tez in
    let _ = test_marketfighter "Should not allow user to sell a fighter in fight" (alice_address, Sell (token1, 10tez), listing_fee) false in


    // ******************** Sell and buy test ******************** // 
    // TODO move this part in another file for the future
    let _ = print_topic "Sell and Buy combination" in

    let _ = Test.set_source bob_address in
    let _ = Test.transfer_to_contract marketfighter_contract (Buy (token3,11tez)) 11tez in
    let d : marketfighter_storage = Test.get_storage_of_address marketfighter_addr |> Test.decompile in
    let _ = print_checkmark (Set.mem token3 d.listed_sale, true) in
    let _ = print_step "Buy at a lower price than an offer don't sell the item" in

    let _ = Test.set_source bob_address in
    let _ = Test.transfer_to_contract marketfighter_contract (Buy (token3,18tez)) 18tez in
    let d : fighter_storage = Test.get_storage_of_address fighter_addr |> Test.decompile in
    let _ = print_checkmark (Big_map.mem token3 d.fighters, true) in
    let _ = print_step "Buy at higher price" in

    let event : event_sold list = Test.get_last_events_from marketfighter_typed_addr "sold" in
    let _ = match (List.head_opt event) with
      | Some (id, price) -> print_checkmark (id = token3 && price = 12tez, true)
      | None -> print_checkmark (false, true) in
    let _ = print_step "Should catch sold event during a buy" in

    let d : marketfighter_storage = Test.get_storage_of_address marketfighter_addr |> Test.decompile in
    let _ = print_checkmark (Set.mem token3 d.listed_sale, false) in
    let _ = print_step "Fighter remove from listed_sale" in    
    let _ = print_checkmark (Set.mem token3 d.listed_offer, false) in
    let _ = print_step "Fighter remove from listed_offer (only offer from bob)" in

    let _ = Test.transfer_to_contract marketfighter_contract (Buy (token1,15tez)) 15tez in
    let d : marketfighter_storage = Test.get_storage_of_address marketfighter_addr |> Test.decompile in
    let _ = print_checkmark (Set.mem token1 d.listed_sale, false) in
    let _ = print_step "Buy at the exact price of an offer" in
    
    let _ = Test.set_source alice_address in
    let _ = Test.transfer_to_contract marketfighter_contract (Buy (token3,15tez)) 15tez in

    let _ = Test.set_source bob_address in
    let _ = Test.transfer_to_contract marketfighter_contract (Sell (token3,16tez)) listing_fee in
    let d : marketfighter_storage = Test.get_storage_of_address marketfighter_addr |> Test.decompile in
    let _ = print_checkmark (Set.mem token3 d.listed_sale, true) in
    let _ = print_step "Sell the fighter at higher price" in

    let _ = Test.set_source bob_address in
    let _ = Test.transfer_to_contract marketfighter_contract (Sell (token3,14tez)) listing_fee in
    let d : marketfighter_storage = Test.get_storage_of_address marketfighter_addr |> Test.decompile in
    let _ = print_checkmark (Set.mem token3 d.listed_sale, false) in
    let _ = print_step "Sell the fighter at lower price" in

    let event : event_sold list = Test.get_last_events_from marketfighter_typed_addr "sold" in
    let _ = match (List.head_opt event) with
      | Some (id, price) -> print_checkmark (id = token3 && price = 14tez, true)
      | None -> print_checkmark (false, true) in
    let _ = print_step "Should catch sold event during a sell" in

    let _ = test_marketfighter "Should allow bob to make an offer on a fighter" (bob_address, Buy (token4,15tez), 15tez) true in
    let _ = test_fighter "Should allow Alice to transfer this fighter at Bob" (alice_address, Transfer (token4, bob_address), 0tez) true in
    let d : marketfighter_storage = Test.get_storage_of_address marketfighter_addr |> Test.decompile in
    let _ = print_checkmark ((Big_map.mem 4n d.buys) && (Set.mem 4n d.listed_offer), false) in
    let _ = print_step "The fighter should be removed from listed_offer and buys in contract." in

    // ******************** Cancel test ******************** //
    let _ = print_topic "Cancel" in

    let _ = Test.set_source alice_address in
    let _ = Test.transfer_to_contract marketfighter_contract (Sell (token3,15tez)) listing_fee in
    
    let _ = test_marketfighter "Should not allow the user to cancel an offer when there is no offer on their side" (bob_address,  Cancel token3, 0tez) false in

    let _ = test_marketfighter "Should allow user to cancel his sell on a fighter" (alice_address,  Cancel token3, 0tez) true in
    
    let event : event_cancel_selling list = Test.get_last_events_from marketfighter_typed_addr "cancelSelling" in
    let _ = match (List.head_opt event) with
      | Some (id) -> print_checkmark (id = token3, true)
      | None -> print_checkmark (false, true) in
    let _ = print_step "Should catch cancelSelling event" in

    let d : marketfighter_storage = Test.get_storage_of_address marketfighter_addr |> Test.decompile in
    let _ = print_checkmark (Set.mem token3 d.listed_sale, false) in
    let _ = print_step "Fighter remove from listed_sale" in    

    let f : fighter_storage = Test.get_storage_of_address fighter_addr |> Test.decompile in
    let f : fighter_data = Big_map.find token3 f.fighters in
    let _ = print_checkmark (f.listed, false) in
    let _ = print_step "Fighter is not listed anymore" in
    
    let _ = Test.set_source bob_address in
    let _ = Test.transfer_to_contract marketfighter_contract (Buy (token3,8tez)) 8tez in    

    let _ = test_marketfighter "Should allow the user to cancel an offer" (bob_address, Cancel token3, 0tez) true in

    let event : event_cancel_buying list = Test.get_last_events_from marketfighter_typed_addr "cancelBuying" in
    let _ = match (List.head_opt event) with
      | Some (id) -> print_checkmark (id = token3, true)
      | None -> print_checkmark (false, true) in
    let _ = print_step "Should catch cancelBuying event" in

    // ******************** SetListingFee test ******************** //
    let _ = print_topic "SetListingFee" in

    let _ = test_marketfighter "Should not allow user to use SetListingFee" (alice_address,  SetListingFee 20tez, 0tez) false in

    let _ = test_marketfighter "Should allow admin to use SetListingFee" (admin_address,  SetListingFee 20tez, 0tez) true in

    let d : marketfighter_storage = Test.get_storage_of_address marketfighter_addr |> Test.decompile in
    let _ = print_checkmark (d.listing_fee = 20tez, true) in
    let _ = print_step "listing_fee updated" in

    // ******************** SetMinPrice test ******************** //
    let _ = print_topic "SetMinPrice" in

    let _ = test_marketfighter "Should not allow user to use SetMinPrice" (alice_address,  SetMinPrice 20tez, 0tez) false in

    let _ = test_marketfighter "Should allow admin to use SetMinPrice" (admin_address,  SetMinPrice 20tez, 0tez) true in

    let d : marketfighter_storage = Test.get_storage_of_address marketfighter_addr |> Test.decompile in
    let _ = print_checkmark (d.min_price = 20tez, true) in
    let _ = print_step "min_price updated" in

    // ******************** SinkFees test ******************** //
    let _ = print_topic "SinkFees" in

    let _ = test_marketfighter "Should not allow user to use SinkFees" (alice_address,  SinkFees alice_address, 0tez) false in

    let _ = test_marketfighter "Should allow admin to use SinkFees" (admin_address,  SinkFees alice_address, 0tez) true in

    let _ = Test.println "" in

    ()