#include "marketfighter.event.mligo"
#include "../test.utils.mligo"

let test =

    // ******************** Init test ******************** //
    let _ = print_topic ("SetFighterAddr") in
    let _ = test_marketfighter "Should not allow user to use SetFighterAddr entrypoint"  (alice_address, SetFighterAddr fighter_addr, 0tez) false in
    let _ = test_marketfighter "Should allow admin to use SetFighterAddr entrypoint"     (admin_address, SetFighterAddr fighter_addr, 0tez) true in

    let _ = print_topic ("SetAdmins") in
    let _ = test_marketfighter "Should not allow user to use SetAdmins entrypoint"  (alice_address, SetAdmins (Set.literal [admin_address]), 0tez) false in
    let _ = test_marketfighter "Should allow admin to use SetAdmins entrypoint"     (admin_address, SetAdmins (Set.literal [admin_address]), 0tez) true in

    let _ = print_topic ("SetManagers") in
    let _ = test_marketfighter "Should not allow user to use SetMinters entrypoint"  (alice_address, SetManagers (Set.literal [manager_address; fighter_addr]), 0tez) false in
    let _ = test_marketfighter "Should allow admin to use SetMinters entrypoint"     (admin_address, SetManagers (Set.literal [manager_address; fighter_addr]), 0tez) true in

    // ******************** Mint some fighter ******************** //

    let _ = Test.set_source alice_address in
    let _ =  Test.transfer_to_contract fighter_contract (Mint) mint_fee in
    let _ = Test.set_source bob_address in
    let _ =  Test.transfer_to_contract fighter_contract (Mint) mint_fee in
    let _ = Test.set_source alice_address in
    let _ =  Test.transfer_to_contract fighter_contract (Mint) mint_fee in
    let _ =  Test.transfer_to_contract fighter_contract (Mint) mint_fee in
    
    let token1 : fighter_id = 1n in
    let token2 : fighter_id = 2n in
    let token3 : fighter_id = 3n in
    let token4 : fighter_id = 4n in

    let _ = Test.set_source minter_address in
    let _ = Test.transfer_to_contract fighter_contract (RealMint (token1,0xfb0504030201fb0101010101,[4n;5n;6n])) 0tez in
    let _ = Test.transfer_to_contract fighter_contract (RealMint (token2,0xfb0604030201fb0101010101,[4n;5n;6n])) 0tez in
    let _ = Test.transfer_to_contract fighter_contract (RealMint (token3,0xfb1004030201fb0101010101,[1n;6n;7n])) 0tez in
    let _ = Test.transfer_to_contract fighter_contract (RealMint (token4,0xfb1004030201fb0001010101,[1n;6n;7n])) 0tez in


    // ******************** SetMarketOpen test ******************** // 
    let _ = print_topic "SetMarketOpen" in 

    let _ = test_marketfighter "Should not allow user to close the market" (alice_address, (SetMarketOpen false), 0tez) false in

    let _ = test_marketfighter "Should allow admin to close the market" (admin_address, (SetMarketOpen false), 0tez) true in

    let d : marketfighter_storage = Test.get_storage_of_address marketfighter_addr |> Test.decompile in
    let _ = print_checkmark (d.is_open = false, true) in
    let _ = print_step "Market is closed in memory" in

    // Source is already admin
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

    let _ = Test.set_source manager_address in
    let _ = Test.transfer_to_contract fighter_contract (SetFighterState (token1, 0n, 0n, Some default_queue)) 30tez in
    let _ = test_marketfighter "Should not allow user to sell a fighter in queue" (alice_address, Sell (token1, 10tez), listing_fee) false in
    
    let _ = Test.transfer_to_contract fighter_contract (SetFighterState (token1, 0n, 1n, None)) 30tez in
    let _ = test_marketfighter "Should not allow user to sell a fighter in tournament" (alice_address, Sell (token1, 10tez), listing_fee) false in
    
    let _ = Test.transfer_to_contract fighter_contract (SetFighterState (token1, 1n, 0n, None)) 30tez in
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
    
    let _ = test_marketfighter "Should not allow admin to use SetListingFee" (admin_address,  SetListingFee 20tez, 0tez) false in

    let _ = test_marketfighter "Should allow manager to use SetListingFee" (manager_address,  SetListingFee 20tez, 0tez) true in

    let d : marketfighter_storage = Test.get_storage_of_address marketfighter_addr |> Test.decompile in
    let _ = print_checkmark (d.listing_fee = 20tez, true) in
    let _ = print_step "listing_fee updated" in

    // ******************** SetMinPrice test ******************** //
    let _ = print_topic "SetMinPrice" in

    // Protect the test from baker problem
    let _ = Test.set_baker bob_address in

    let _ = test_marketfighter "Should not allow user to use SetMinPrice" (alice_address,  SetMinPrice 20tez, 0tez) false in

    let _ = test_marketfighter "Should not allow admin to use SetMinPrice" (admin_address,  SetMinPrice 20tez, 0tez) false in

    let _ = test_marketfighter "Should allow manager to use SetMinPrice" (manager_address,  SetMinPrice 20tez, 0tez) true in

    let d : marketfighter_storage = Test.get_storage_of_address marketfighter_addr |> Test.decompile in
    let _ = print_checkmark (d.min_price = 20tez, true) in
    let _ = print_step "min_price updated" in

    // ******************** SinkFees test ******************** //
    let _ = print_topic "SinkFees" in
 
    let _ = test_marketfighter "Should not allow user to use SinkFees" (alice_address,  SinkFees alice_address, 0tez) false in

    let _ = test_marketfighter "Should allow admin to use SinkFees" (admin_address,  SinkFees alice_address, 0tez) true in

    let _ = Test.println "" in

    ()
