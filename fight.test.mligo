#include "test.utils.mligo"
#include "event.mligo"

let test =
    
    // fight_contract
    let _ = print_topic "SetFighterAddr" in
    let _ = test_fight "Should not allow user to use SetFighterAddr entrypoint"  (alice_address, SetFighterAddr fighter_addr, 0tez) false in
    let _ = test_fight "Should allow admin to use SetFighterAddr entrypoint"     (admin_address, SetFighterAddr fighter_addr, 0tez) true in
    let _ = print_topic "SetAttributeAddr" in
    let _ = test_fight "Should not allow user to use SetAttributeAddr entrypoint"  (alice_address, SetAttributeAddr attribute_addr, 0tez) false in
    let _ = test_fight "Should allow admin to use SetAttributeAddr entrypoint"     (admin_address, SetAttributeAddr attribute_addr, 0tez) true in


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

    let _ = Test.set_source minter_address in
    let _ = Test.transfer_to_contract fighter_contract (RealMint (token1,0xfb0504030201fb0101010101,[4n;5n;6n])) 0tez in
    let _ = Test.transfer_to_contract fighter_contract (RealMint (token2,0xfb0604030201fb0101010101,[4n;5n;6n])) 0tez in
    let _ = Test.transfer_to_contract fighter_contract (RealMint (token3,0xfb1004030201fb0101010101,[1n;6n;7n])) 0tez in
    let _ = Test.transfer_to_contract fighter_contract (RealMint (alice_token,0xfb1404030201fb0101010101,[1n;6n;7n])) 0tez in
    let _ = Test.transfer_to_contract fighter_contract (RealMint (bob_token,0xfb1005030201fb0101010101,[1n;6n;7n])) 0tez in


    // ************ AddToQueue ********* //
    let _ = print_topic "AddToQueue" in
    
    let _ = test_fight "Should not allow the user to queue up another person's fighter" (bob_address, (AddToQueue (token1, default_queue)), fight_fee) false in
    
    let _ = test_fight "Should allow the user to queue up a fighter" (alice_address, (AddToQueue (token1, default_queue)), fight_fee) true in

    let event : event_added_to_queue list = Test.get_last_events_from fight_typed_addr "addedToQueue" in
    let _ = match (List.head_opt event) with
      | Some (id, q) -> print_checkmark (id = token1 && q = default_queue, true)
      | None -> print_checkmark (false, true) in
    let _ = print_step "addedToQueue event catch" in

    let d : fight_storage = Test.get_storage_of_address fight_addr |> Test.decompile in
    let f : fighter_storage = Test.get_storage_of_address fighter_addr |> Test.decompile in
    let f : fighter_data = Big_map.find 1n f.fighters in
    let _ = print_checkmark ((Set.mem 1n (Big_map.find default_queue d.queues) && f.queue = Some default_queue), true) in
    let _ = print_step "The first fighter is registered in the queue in fight_storage and fighter_storage" in

    let _ = Test.set_source alice_address in
    let _ = Test.transfer_to_contract marketfighter_contract (Sell (token3,16tez)) listing_fee in
    let _ = test_fight "Should not allow the user to AddToQueue a fighter when the fighter is on sale" (alice_address, (AddToQueue (token3, default_queue)), fight_fee) false in
    let _ = Test.transfer_to_contract marketfighter_contract (Cancel token3) 0tez in

    let _ = Test.set_source manager_address in
    let _ = Test.transfer_to_contract fighter_contract (SetFighterState (token3, 0n, 1n, None)) 0tez in
    let _ = test_fight "Should not allow the user to AddToQueue a fighter in tournament" (alice_address, (AddToQueue (token3, default_queue)), fight_fee) false in

    let _ = test_fight "Should not allow the user to AddToQueue a fighter in queue" (alice_address, (AddToQueue (token1, default_queue)), fight_fee) false in

    let _ = Test.set_source manager_address in
    let _ = Test.transfer_to_contract fighter_contract (SetFighterState (token3, 1n, 0n, None)) 0tez in
    let _ = test_fight "Should not allow the user to AddToQueue a fighter in fight" (alice_address, (AddToQueue (token3, default_queue)), fight_fee) false in
    let _ = Test.set_source manager_address in
    let _ = Test.transfer_to_contract fighter_contract (SetFighterState (token3, 0n, 0n, None)) 0tez in

    let _ = Test.set_source alice_address in
    let _ = Test.transfer_to_contract fighter_contract (Mint) mint_fee in
    let token4 : fighter_id = 6n in
    let _ = test_fight "Should not allow the user to AddToQueue a fighter not fully minted" (alice_address, (AddToQueue (token4, default_queue)), fight_fee) false in

    let _ = Test.set_source admin_address in
    let _ = Test.transfer_to_contract fighter_contract (RealMint (token4, 0xfb5554535251fb1111111111, [])) 0tez in

    let _ = Test.set_source alice_address in
    let _ = Test.transfer_to_contract fighter_contract (Fusion (token4, token3)) fusion_fee in    
    let _ = test_fight "Should not allow the user to AddToQueue an inactive fighter" (alice_address, (AddToQueue (token3, default_queue)), fight_fee) false in


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
    let _ = print_checkmark ((not (Set.mem token1 (Big_map.find default_queue d.queues))), true) in
    let _ = print_step "The fighter is removed from the queue" in

    // *************** CreateFight *************** //
    let _ = print_topic "CreateFight" in

    let _ = test_fight "Should not allow the User to CreateFight" (alice_address, (CreateFight (alice_token,bob_token,3n,default_queue,120)), 0tez) false in

    let _ = test_fight "Should not allow the admin to CreateFight" (admin_address, (CreateFight (alice_token,bob_token,3n,default_queue,120)), 0tez) false in

    let _ = Test.set_source alice_address in
    let _ = Test.transfer_to_contract fight_contract (AddToQueue (alice_token, default_queue)) 10tez in

    let _ = Test.set_source bob_address in
    let other_queue : fight_queue = ("OtherLeague", NoStake, NoReward) in
    let _ = Test.transfer_to_contract fight_contract (AddToQueue (bob_token, other_queue)) 10tez in
    
    let _ = test_fight "Should not allow the admin to CreateFight with fighter from different queue" (admin_address, (CreateFight (alice_token,bob_token,3n,default_queue,120)), 0tez) false in

    let _ = Test.set_source bob_address in
    let _ = Test.transfer_to_contract fight_contract (CancelQueue bob_token) 0tez in
    
    let _ = Test.set_source bob_address in
    let _ = Test.transfer_to_contract fight_contract (AddToQueue (bob_token, default_queue)) 10tez in

    let _ = test_fight "Should allow the matcher to CreateFight" (matcher_address, (CreateFight (alice_token,bob_token,3n,default_queue,120)), 0tez) true in

    let d : fight_storage = Test.get_storage_of_address fight_addr |> Test.decompile in
    let _ = print_checkmark ((Big_map.mem 1n d.fights), true) in
    let _ = print_step "The fight is created" in

    let d : fight_storage = Test.get_storage_of_address fight_addr |> Test.decompile in
    let d : fight_data = Big_map.find 1n d.fights in
    let _ = print_checkmark ((d.state = Initialized), true) in
    let _ = print_step "The fight state is Initialized" in

    let event : event_new_round list = Test.get_last_events_from fight_typed_addr "newRound" in
    let _ = match (List.head_opt event) with
      | Some (id, a, b, r, _) -> print_checkmark (id = 1n && a = 4n && b = 5n && r = 1n , true)
      | None -> print_checkmark (false, true) in
    let _ = print_step "Should catch newRound event" in
    
  
    // *************** SetStrategy *************** //
    let _ = print_topic "SetStrategy" in

    let _ = test_fight "Should not allow the user to SetStrategy of another fighter" (bob_address, (SetStrategy (1n, alice_token, 0x00)), 0tez) false in
    
    let _ = test_fight "Should allow the user to SetStrategy" (bob_address, (SetStrategy (1n, bob_token, 0x00)), 0tez) true in

    let event : event_strategy list = Test.get_last_events_from fight_typed_addr "strategy" in
    let _ = match (List.head_opt event) with
      | Some (id, f_id, b) -> print_checkmark (id = 1n && f_id = bob_token && b = 0x00 , true)
      | None -> print_checkmark (false, true) in
    let _ = print_step "Should catch strategy event" in

    let _ = test_fight "Should allow the user to SetStrategy again" (bob_address, (SetStrategy (1n, bob_token, 0x00)), 0tez) true in
    
    // *************** ResolveRound *************** //
    let _ = print_topic "ResolveRound" in

    let _ = test_fight "Should not allow the user to ResolveRound" (alice_address, (ResolveRound (1n, 1n, 1, 0x00)), 0tez) false in
    let _ = test_fight "Should not allow the admin to ResolveRound" (admin_address, (ResolveRound (1n, 1n, 1, 0x00)), 0tez) false in
    
    let _ = test_fight "Should allow the resolver to ResolveRound 1" (resolver_address, (ResolveRound (1n, 1n, 1, 0x00)), 0tez) true in

    let event : event_round_resolved list = Test.get_last_events_from fight_typed_addr "roundResolved" in
    let _ = match (List.head_opt event) with
      | Some (id, r, w, b) -> print_checkmark (id = 1n && r = 1n && w = 1 && b = 0x00 , true)
      | None -> print_checkmark (false, true) in
    let _ = print_step "Should catch roundResolved event for round 1" in
    
    let event : event_new_round list = Test.get_last_events_from fight_typed_addr "newRound" in
    let _ = match (List.head_opt event) with
      | Some (id, a, b, r, _) -> print_checkmark (id = 1n && a = 4n && b = 5n && r = 2n , true)
      | None -> print_checkmark (false, true) in
    let _ = print_step "Should catch newRound event for round 2" in
    
    let d : fight_storage = Test.get_storage_of_address fight_addr |> Test.decompile in
    let d : fight_data = Big_map.find 1n d.fights in
    let _ = print_checkmark ((d.state = (OnGoing : fight_state)), true) in
    let _ = print_step "The fight state is OnGoing" in

    let _ = test_fight "Should not allow the resolver to ResolveRound 3 before round 2" (resolver_address, (ResolveRound (1n, 3n, 1, 0x00)), 0tez) false in
    
    let _ = test_fight "Should allow the resolver to ResolveRound 2" (resolver_address, (ResolveRound (1n, 2n, 1, 0x00)), 0tez) true in
    
    let event : event_round_resolved list = Test.get_last_events_from fight_typed_addr "roundResolved" in
    let _ = match (List.head_opt event) with
      | Some (id, r, w, b) -> print_checkmark (id = 1n && r = 2n && w = 1 && b = 0x00 , true)
      | None -> print_checkmark (false, true) in
    let _ = print_step "Should catch roundResolved event for round 2" in
    
    let event : event_new_round list = Test.get_last_events_from fight_typed_addr "newRound" in
    let _ = match (List.head_opt event) with
      | Some (id, a, b, r, _) -> print_checkmark (id = 1n && a = 4n && b = 5n && r = 3n , true)
      | None -> print_checkmark (false, true) in
    let _ = print_step "Should catch newRound event for round 3" in

    let _ = test_fight "Should allow the resolver to ResolveRound 3" (resolver_address, (ResolveRound (1n, 3n, 1, 0x00)), 0tez) true in
    
    let event : event_round_resolved list = Test.get_last_events_from fight_typed_addr "roundResolved" in
    let _ = match (List.head_opt event) with
      | Some (id, r, w, b) -> print_checkmark (id = 1n && r = 3n && w = 1 && b = 0x00 , true)
      | None -> print_checkmark (false, true) in
    let _ = print_step "Should catch roundResolved event for round 3" in
    
    let event : event_new_round list = Test.get_last_events_from fight_typed_addr "newRound" in
    let _ = match (List.head_opt event) with
      | Some (_) -> print_checkmark (false , true)
      | None -> print_checkmark (true, true) in
    let _ = print_step "Should not catch newRound event" in
    
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

    // * TODO Check Queues, Stakes and Rewards * //
    let _ = print_topic "Stakes and rewards" in
    let _ = print_checkmark (false, true) in
    let _ = print_step "todo Should built the stakes and rewards tests" in
    
    ()
