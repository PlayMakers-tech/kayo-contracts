#include "test.utils.mligo"
#include "event.mligo"

let test =

    let _ = Test.set_source alice_address in
    let _ =  Test.transfer_to_contract fighter_contract (Mint) mint_fee in
    let _ =  Test.transfer_to_contract fighter_contract (Mint) mint_fee in
    let _ =  Test.transfer_to_contract fighter_contract (Mint) mint_fee in
    let _ = Test.set_source bob_address in
    let _ =  Test.transfer_to_contract fighter_contract (Mint) mint_fee in
    let _ =  Test.transfer_to_contract fighter_contract (Mint) mint_fee in
    
    let token1 : fighter_id = 1n in
    let token2 : fighter_id = 2n in
    let token3 : fighter_id = 3n in
    let token4 : fighter_id = 4n in
    let token5 : fighter_id = 5n in

    let _ = Test.set_source minter_address in
    let _ = Test.transfer_to_contract fighter_contract (RealMint (token1,0xfb0504030201fb0101010101,[4n;5n;6n])) 0tez in
    let _ = Test.transfer_to_contract fighter_contract (RealMint (token2,0xfb0604030201fb0101010101,[4n;5n;6n])) 0tez in
    let _ = Test.transfer_to_contract fighter_contract (RealMint (token3,0xfb1004030201fb0101010101,[1n;6n;7n])) 0tez in
    let _ = Test.transfer_to_contract fighter_contract (RealMint (token4,0xfb1404030201fb0101010101,[1n;6n;7n])) 0tez in
    let _ = Test.transfer_to_contract fighter_contract (RealMint (token5,0xfb1005030201fb0101010101,[1n;6n;7n])) 0tez in

    // ************ SetAdmins ********* //
    let _ = print_topic "SetAdmins" in
    let _ = test_fight "Should not allow user to use SetAdmins entrypoint" (alice_address, SetAdmins (Set.literal [admin_address]), 0tez) false in
    let _ = test_fight "Should allow admin to use SetAdmins entrypoint" (admin_address, SetAdmins (Set.literal [admin_address]), 0tez) true in

    // ************ SetManagers ********* //
    let _ = print_topic "SetManagers" in
    let _ = test_fight "Should not allow user to use SetManagers entrypoint" (alice_address, SetManagers (Set.literal [manager_address; tournament_addr]), 0tez) false in
    let _ = test_fight "Should allow admin to use SetManagers entrypoint" (admin_address, SetManagers (Set.literal [manager_address; tournament_addr]), 0tez) true in

    // ************ SetMatchers ********* //
    let _ = print_topic "SetMatchers" in
    let _ = test_fight "Should not allow user to use SetMatchers entrypoint" (alice_address, SetMatchers (Set.literal [matcher_address; tournament_addr]), 0tez) false in
    let _ = test_fight "Should allow manager to use SetMatchers entrypoint" (manager_address, SetMatchers (Set.literal [matcher_address; tournament_addr]), 0tez) true in

    // ************ SetResolvers ********* //
    let _ = print_topic "SetResolvers" in
    let _ = test_fight "Should not allow user to use SetResolvers entrypoint" (alice_address, SetResolvers (Set.literal [resolver_address]), 0tez) false in
    let _ = test_fight "Should allow manager to use SetResolvers entrypoint" (manager_address, SetResolvers (Set.literal [resolver_address]), 0tez) true in

    // ************ SetFighterAddr ********* //
    let _ = print_topic "SetFighterAddr" in

    let _ = test_fight "Should not allow user to use SetFighterAddr entrypoint"  (alice_address, SetFighterAddr fighter_addr, 0tez) false in
    
    let _ = test_fight "Should allow admin to use SetFighterAddr entrypoint"     (admin_address, SetFighterAddr fighter_addr, 0tez) true in
    
    // ************ SetAttributeAddr ********* //
    let _ = print_topic "SetAttributeAddr" in
    
    let _ = test_fight "Should not allow user to use SetAttributeAddr entrypoint"  (alice_address, SetAttributeAddr attribute_addr, 0tez) false in
    
    let _ = test_fight "Should allow admin to use SetAttributeAddr entrypoint"     (admin_address, SetAttributeAddr attribute_addr, 0tez) true in

    // ************ AddToQueue ********* //
    let _ = print_topic "AddToQueue" in
    
    let _ = test_fight "Should not allow the user to queue up another person's fighter" (bob_address, (AddToQueue (token1, default_queue)), fight_fee) false in
    
    let _ = test_fight "Should allow the user to queue up a fighter" (alice_address, (AddToQueue (token1, stake_queue)), 10tez + fight_fee) true in

    let event : event_added_to_queue list = Test.get_last_events_from fight_typed_addr "addedToQueue" in
    let _ = match (List.head_opt event) with
      | Some (id, q) -> print_checkmark (id = token1 && q = stake_queue, true)
      | None -> print_checkmark (false, true) in
    let _ = print_step "addedToQueue event catch" in

    let d : fight_storage = Test.get_storage_of_address fight_addr |> Test.decompile in
    let f : fighter_storage = Test.get_storage_of_address fighter_addr |> Test.decompile in
    let f : fighter_data = Big_map.find 1n f.fighters in
    let var = match (Big_map.find_opt stake_queue d.queues) with
        | Some x -> x
        | None -> Set.empty in
    let _ = print_checkmark (var <> (Set.empty : nat set) && (Set.mem 1n var && f.queue = Some stake_queue), true) in
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
    let token6 : fighter_id = 6n in
    let _ = test_fight "Should not allow the user to AddToQueue a fighter not fully minted" (alice_address, (AddToQueue (token6, default_queue)), fight_fee) false in

    let _ = Test.set_source alice_address in
    let _ = Test.transfer_to_contract_exn fighter_contract (Fusion (token2, token3)) fusion_fee in    
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
   
    let data1 : tez = Test.get_balance alice_address in

    let _ = test_fight "Should allow the user to CancelQueue" (alice_address, (CancelQueue token1), 0tez) true in

    let data2 : tez = Test.get_balance alice_address in

    let _ = print_checkmark ((data1 + 9tez = data2), true) in
    let _ = print_step "The user is refunded the bet" in

    let _ = test_fight "Should not allow the user to CancelQueue when the fighter is not in queue" (alice_address, (CancelQueue token1), 0tez) false in

    let d : fight_storage = Test.get_storage_of_address fight_addr |> Test.decompile in
    let var = match (Big_map.find_opt stake_queue d.queues) with
            | Some x -> x
            | None -> Set.empty in
    let _ = print_checkmark (var = (Set.empty : nat set), true) in
    let _ = print_step "The fighter is removed from the queue" in

    // *************** CreateFight *************** //
    let _ = print_topic "CreateFight" in

    let _ = test_fight "Should not allow the User to CreateFight" (alice_address, (CreateFight (token1,token4,3n,default_queue,120)), 0tez) false in

    let _ = test_fight "Should not allow the admin to CreateFight" (admin_address, (CreateFight (token1,token4,3n,default_queue,120)), 0tez) false in

    let _ = Test.set_source alice_address in
    let _ = Test.transfer_to_contract fight_contract (AddToQueue (token1, default_queue)) 10tez in

    let _ = Test.set_source bob_address in
    let other_queue : fight_queue = ("OtherLeague", NoStake, NoReward) in
    let _ = Test.transfer_to_contract fight_contract (AddToQueue (token4, other_queue)) 10tez in
    
    let _ = test_fight "Should not allow the admin to CreateFight with fighter from different queue" (admin_address, (CreateFight (token1,token4,3n,default_queue,120)), 0tez) false in

    let _ = Test.set_source bob_address in
    let _ = Test.transfer_to_contract fight_contract (CancelQueue token4) 0tez in
    
    let _ = Test.transfer_to_contract fight_contract (AddToQueue (token4, default_queue)) 10tez in

    let _ = test_fight "Should allow the matcher to CreateFight" (matcher_address, (CreateFight (token1,token4,3n,default_queue,120)), 0tez) true in

    let d : fight_storage = Test.get_storage_of_address fight_addr |> Test.decompile in
    let _ = print_checkmark ((Big_map.mem 1n d.fights), true) in
    let _ = print_step "The fight is created" in

    let d : fight_storage = Test.get_storage_of_address fight_addr |> Test.decompile in
    let _ = match (Big_map.find_opt 1n d.fights) with
            | Some x -> print_checkmark (x.state = Initialized, true)
            | None   -> print_checkmark (false, true) in
    let _ = print_step "The fight state is Initialized" in

    let event : event_new_round list = Test.get_last_events_from fight_typed_addr "newRound" in
    let _ = match (List.head_opt event) with
      | Some (id, a, b, r, _) -> print_checkmark (id = 1n && a = token1 && b = token4 && r = 1n , true)
      | None -> print_checkmark (false, true) in
    let _ = print_step "Should catch newRound event" in

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
      | Some (id, a, b, r, _) -> print_checkmark (id = 1n && a = token1 && b = token4 && r = 2n , true)
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
      | Some (id, a, b, r, _) -> print_checkmark (id = 1n && a = token1 && b = token4 && r = 3n , true)
      | None -> print_checkmark (false, true) in
    let _ = print_step "Should catch newRound event for round 3" in

    let _ = test_fight "Should allow the resolver to ResolveRound 3" (resolver_address, (ResolveRound (1n, 3n, 1, 0x00)), 0tez) true in
    
    let _ = Test.set_baker minter_address in
    
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

    // *************** Reward *************** //
    let _ = print_topic "Reward" in

    let data1 : attribute_storage = Test.get_storage_of_address attribute_addr |> Test.decompile in
    let data1 : attribute_data = Big_map.find token1 data1.attributes in

    let xp_reward_queue : fight_queue = ("Some_cool_league", NoStake, XPReward 1000n) in
    let fight_nbr : nat = 2n in
    let _ = Test.set_source alice_address in
    let _ = Test.transfer_to_contract_exn fight_contract (AddToQueue (token1, xp_reward_queue)) 10tez in
    let _ = Test.set_source bob_address in
    let _ = Test.transfer_to_contract_exn fight_contract (AddToQueue (token4, xp_reward_queue)) 10tez in
    let _ = Test.set_source matcher_address in
    let _ = Test.transfer_to_contract_exn fight_contract (CreateFight (token1,token4,3n,xp_reward_queue,120)) 0tez in
    let _ = Test.set_source resolver_address in
    let _ = Test.transfer_to_contract_exn fight_contract (ResolveRound (fight_nbr, 1n, 1, 0x00)) 0tez in
    let _ = Test.transfer_to_contract_exn fight_contract (ResolveRound (fight_nbr, 2n, 1, 0x00)) 0tez in
    let _ = Test.transfer_to_contract_exn fight_contract (ResolveRound (fight_nbr, 3n, 1, 0x00)) 0tez in
    
    let data2 : attribute_storage = Test.get_storage_of_address attribute_addr |> Test.decompile in
    let data2 : attribute_data = Big_map.find token1 data2.attributes in

    let _ = print_checkmark ((data2.xp > data1.xp + 1000n), true) in
    let _ = print_step "The fighter has been rewarded with xp" in

    let current_queue : fight_queue = ("Some_cool_league", TezStake 50tez, TezReward 100tez) in
    let fight_nbr : nat = 3n in
    let _ = Test.set_source alice_address in
    let _ = Test.transfer_to_contract fight_contract (AddToQueue (token1, current_queue)) 60tez in
    
    let data1 = Test.get_balance alice_address in
    
    let _ = Test.set_source bob_address in
    let _ = Test.transfer_to_contract fight_contract (AddToQueue (token4, current_queue)) 60tez in
    let _ = Test.set_source matcher_address in
    let _ = Test.transfer_to_contract fight_contract (CreateFight (token1,token4,3n,current_queue,120)) 0tez in
    let _ = Test.set_source resolver_address in
    let _ = Test.transfer_to_contract fight_contract (ResolveRound (fight_nbr, 1n, 1, 0x00)) 0tez in
    let _ = Test.transfer_to_contract fight_contract (ResolveRound (fight_nbr, 2n, 1, 0x00)) 0tez in
    let _ = Test.transfer_to_contract fight_contract (ResolveRound (fight_nbr, 3n, 1, 0x00)) 0tez in

    let data2 = Test.get_balance alice_address in

    let _ = print_checkmark ((data2 = data1 + 100tez), true) in
    let _ = print_step "The fighter has been rewarded with tez" in

    let current_queue : fight_queue = ("Some_cool_league", NoStake, TezReward 10tez) in
    let _ = Test.set_source alice_address in
    let _ = test_fight "The fighter has not join the following queue: (\"Some_cool_league\", NoStake, TezReward 10tez)" (alice_address, (AddToQueue (token1, current_queue)), 0tez) false in

    let current_queue : fight_queue = ("Some_cool_league", TezStake 50tez, TezReward 120tez) in
    let _ = Test.set_source alice_address in
    let _ = test_fight "The fighter has not join the following queue: (\"Some_cool_league\", TezStake 50tez, TezReward 120tez)" (alice_address, (AddToQueue (token1, current_queue)), 0tez) false in

    let _ = Test.println "" in
    ()
