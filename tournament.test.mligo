#include "test.utils.mligo"
#include "event.mligo"

let test =

    let _ = print_topic "SetFighterAddr" in
    let _ = test_tournament "Should not allow user to use SetFighterAddr entrypoint"  (alice_address, SetFighterAddr fighter_addr, 0tez) false in
    let _ = test_tournament "Should allow admin to use SetFighterAddr entrypoint"     (admin_address, SetFighterAddr fighter_addr, 0tez) true in

    let _ = print_topic "SetFightAddr" in
    let _ = test_tournament "Should not allow user to use SetFightAddr entrypoint"  (alice_address, SetFightAddr fight_addr, 0tez) false in
    let _ = test_tournament "Should allow admin to use SetFightAddr entrypoint"     (admin_address, SetFightAddr fight_addr, 0tez) true in

    let _ = print_topic "SetAttributeAddr" in
    let _ = test_tournament "Should not allow user to use SetAttributeAddr entrypoint"  (alice_address, SetAttributeAddr attribute_addr, 0tez) false in
    let _ = test_tournament "Should allow admin to use SetAttributeAddr entrypoint"     (admin_address, SetAttributeAddr attribute_addr, 0tez) true in

    let _ = print_topic "SetShopAddr" in
    let _ = test_tournament "Should not allow user to use SetShopAddr entrypoint"  (alice_address, SetShopAddr shop_addr, 0tez) false in
    let _ = test_tournament "Should allow admin to use SetShopAddr entrypoint"     (admin_address, SetShopAddr shop_addr, 0tez) true in


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

    
    let ticket1 : shop_item_data = {
        item = ("ticket1":shop_item);
        quantity = 99999999n;
        price = 1tez;
        consumers = Set.literal [bob_address]
    } in

    let _ = Test.set_source manager_address in
    let _ = Test.transfer_to_contract shop_contract (NewItem ticket1) 0tez in
    
    // ***************** SetTournamentFee *************** //
    let _ = print_topic "SetTournamentFee" in

    let _ = test_tournament "Should not allow user to use SetTournamentFee entrypoint"  (alice_address, SetTournamentFee 100tez, 0tez) false in

    let _ = test_tournament "Should allow admin to use SetTournamentFee entrypoint"     (admin_address, SetTournamentFee 100tez, 0tez) true in    

    let d : tournament_storage = Test.get_storage_of_address tournament_addr |> Test.decompile in
    let _ = print_checkmark (d.tournament_fee=100tez, true) in
    let _ = print_step "Tournament fee is updated in the memory" in

    // ***************** CreateTournament *************** //
    let _ = print_topic "CreateTournament" in

    let default_tournament_def : tournament_def = ("League", NoStake, [XPReward 1000n; TezReward 100tez], "RoundRobin", 2n, 999n ) in

    let _ = test_tournament "Should not allow user to use CreateTournament entrypoint"  (alice_address, CreateTournament (default_tournament_def,Tezos.get_now () + 180), 0tez) false in
    
    let _ = test_tournament "Should not allow admin to use CreateTournament entrypoint"  (admin_address, CreateTournament (default_tournament_def,Tezos.get_now () + 180), 0tez) false in

    let _ = test_tournament "Should allow scheduler to use CreateTournament entrypoint"     (scheduler_address, CreateTournament (default_tournament_def,Tezos.get_now () + 180), 0tez) true in

    let event : event_new_tournament list = Test.get_last_events_from tournament_typed_addr "newTournament" in
    let _ = match (List.head_opt event) with
      | Some (id, s, _) -> print_checkmark (id = 1n && s = default_tournament_def, true)
      | None -> print_checkmark (false, true) in
    let _ = print_step "Should catch newTournament event" in

    let d : tournament_storage = Test.get_storage_of_address tournament_addr |> Test.decompile in
    let _ = print_checkmark (Big_map.mem 1n d.tournaments && Set.mem 1n d.active_tournaments && d.next_id = 2n, true) in
    let _ = print_step "Tournament is created in the memory" in

    // ***************** CancelTournament *************** //
    let _ = print_topic "CancelTournament" in

    let _ = test_tournament "Should not allow user to use CancelTournament entrypoint"  (alice_address, CancelTournament (1n), 0tez) false in
    
    let _ = test_tournament "Should not allow admin to use CancelTournament entrypoint"  (admin_address, CancelTournament 1n, 0tez) false in

    let _ = test_tournament "Should allow scheduler to use CancelTournament entrypoint"     (scheduler_address, CancelTournament 1n, 0tez) true in

    let d : tournament_storage = Test.get_storage_of_address tournament_addr |> Test.decompile in
    let _ = print_checkmark (Set.mem 1n d.active_tournaments, false) in
    let _ = print_step "Tournament is cancelled in the memory" in

    let stake_tournament_def : tournament_def = ("League", TezStake 10tez, [], "RoundRobin", 2n, 999n ) in
    let _ = Test.set_source scheduler_address in
    let _ = Test.transfer_to_contract tournament_contract (CreateTournament (stake_tournament_def,Tezos.get_now () + 180)) 0tez in


    let bal1 : tez = Test.get_balance alice_address in

    let _ = Test.set_source alice_address in
    let _ = Test.transfer_to_contract tournament_contract (JoinTournament (2n, token1)) 110tez in
    
    let bal2 : tez = Test.get_balance alice_address in

    let _ = Test.set_source scheduler_address in
    let _ = Test.transfer_to_contract tournament_contract (CancelTournament 2n) 0tez in

    let bal3 : tez = Test.get_balance alice_address in
    
    // TODO Better cond
    let cond = ((bal1 - bal2) < (bal1 - bal3)) in
    let _ = print_checkmark (cond, true) in
    let _ = print_step "Should test cancelling after fighters have joined (with stakes)" in

    // ***************** JoinTournament *************** //
    let _ = print_topic "JoinTournament" in

    let _ = Test.set_source scheduler_address in
    let _ = Test.transfer_to_contract tournament_contract (CreateTournament (default_tournament_def,Tezos.get_now () + 180)) 0tez in

    let _ = test_tournament "Should not allow user to use JoinTournament with a non-owned fighter"  (alice_address, JoinTournament (3n, token2), 100tez) false in

    let _ = test_tournament "Should not allow user to use JoinTournament with an incorrect fee" (alice_address, JoinTournament (3n, token1), 10tez) false in
    
    let _ = Test.set_source manager_address in
    let _ = Test.transfer_to_contract fighter_contract (SetFighterState (token1, 0n, 0n, Some default_queue)) 0tez in
    let _ = test_tournament "Should not allow user to use JoinTournament if the fighter is in queue" (alice_address, JoinTournament (3n, token1), 100tez) false in

    let _ = Test.set_source manager_address in
    let _ = Test.transfer_to_contract fighter_contract (SetFighterState (token1, 0n, 1n, None)) 0tez in
    let _ = test_tournament "Should not allow user to use JoinTournament if the fighter is already in tounament" (alice_address, JoinTournament (3n, token1), 100tez) false in

    let _ = Test.set_source manager_address in
    let _ = Test.transfer_to_contract fighter_contract (SetFighterState (token1, 1n, 0n, None)) 0tez in
    let _ = test_tournament "Should not allow user to use JoinTournament if the fighter is in fight" (alice_address, JoinTournament (3n, token1), 100tez) false in
    
    let _ = Test.set_source manager_address in
    let _ = Test.transfer_to_contract fighter_contract (SetFighterListed (token1, true)) 0tez in
    let _ = test_tournament "Should not allow user to use JoinTournament if the fighter is listed" (alice_address, JoinTournament (3n, token1), 100tez) false in
    let _ = Test.set_source manager_address in
    let _ = Test.transfer_to_contract fighter_contract (SetFighterListed (token1, false)) 0tez in

    let _ = Test.set_source alice_address in
    let _ = Test.transfer_to_contract fighter_contract (Mint) 20tez in
    let token4 : fighter_id = 6n in
    let _ = test_tournament "Should not allow user to use JoinTournament if the fighter is not fully minted" (alice_address, JoinTournament (3n, token4), 100tez) false in
    
    let _ = Test.set_source manager_address in
    let _ = Test.transfer_to_contract_exn fighter_contract (SetFighterState (token1, 0n, 0n, None)) 0tez in
    let _ = test_tournament "Should allow user to use JoinTournament" (alice_address, JoinTournament (3n, token1), 100tez) true in

    let event : event_joined_tournament list = Test.get_last_events_from tournament_typed_addr "joinedTournament" in
    let _ = match (List.head_opt event) with
      | Some (id, f_id) -> print_checkmark (id = 3n && f_id = token1, true)
      | None -> print_checkmark (false, true) in
    let _ = print_step "Should catch joinedTournament event" in

    // ***************** GenerateTree *************** //
    let _ = print_topic "GenerateTree" in

    // Avoid the problem of baker
    let _ = Test.set_baker bob_address in

    let _ = Test.set_source bob_address in
    let _ = Test.transfer_to_contract tournament_contract (JoinTournament (3n, token2)) 100tez in

    let _ = test_tournament "Should not allow user to use GenerateTree entrypoint"  (alice_address, GenerateTree (3n, 0x01), 0tez) false in
    
    let _ = test_tournament "Should not allow admin to use GenerateTree entrypoint"  (admin_address, GenerateTree (3n, 0x02), 0tez) false in

    let _ = test_tournament "Should allow scheduler to use GenerateTree entrypoint"     (scheduler_address, GenerateTree (3n, 0x03), 0tez) true in

    let event : event_generate_tree list = Test.get_last_events_from tournament_typed_addr "generatedTree" in
    let _ = match (List.head_opt event) with
      | Some (id,metadata) -> print_checkmark (id = 3n && metadata = 0x03, true)
      | None -> print_checkmark (false, true) in
    let _ = print_step "Should catch generatedTree event catch" in

    // ***************** NextPhase *************** //
    let _ = print_topic "NextPhase" in

    let _ = test_tournament "Should create the fight in tournament with next phase" (scheduler_address, NextPhase (3n, Set.literal([1n, 2n]), 3n, 10), 0tez) true in

    let event : event_next_phase list = Test.get_last_events_from tournament_typed_addr "nextPhase" in
    let _ = match (List.head_opt event) with
      | Some (id, phase) -> print_checkmark (id = 3n && phase = 1n, true)
      | None -> print_checkmark (false, true) in
    let _ = print_step "Should catch nextPhase event" in

    let _ = Test.set_source resolver_address in
    let _ = Test.transfer_to_contract fight_contract (ResolveRound (1n, 1n, 1, 0x00)) 0tez in
    let _ = Test.transfer_to_contract fight_contract (ResolveRound (1n, 2n, 1, 0x00)) 0tez in
    let _ = Test.transfer_to_contract fight_contract (ResolveRound (1n, 3n, 1, 0x00)) 0tez in
    
    let d : tournament_storage = Test.get_storage_of_address tournament_addr |> Test.decompile in
    let tournament_3 = Big_map.find 3n d.tournaments in
    let empty_fight_id_set : fight_id set = Set.empty in
    let scores_cond : bool = tournament_3.scores = Map.literal [(1n, 1); (2n, -1)] in
    let cond : bool = tournament_3.pending_fights = empty_fight_id_set && scores_cond && tournament_3.phase = 1n in
    let _ = print_checkmark (cond , true) in
    let _ = print_step "Should have updated correctly the tournament" in

    // *************** ReportFight ************* //

    // Don't need to be tested
    
    // *************** EndTournament ************* //
    let _ = print_topic "EndTournament" in

    let wallet1 : tez = Test.get_balance alice_address in 

    let attr_str : attribute_storage = Test.get_storage_of_address attribute_addr |> Test.decompile in
    let attr_before = Big_map.find 2n attr_str.attributes in

    let _ = test_tournament "Should allow scheduler to end the tournament" (scheduler_address, EndTournament (3n, [2n; 1n]), 0tez) true in

    let event : event_ended_tournament list = Test.get_last_events_from tournament_typed_addr "endedTournament" in
    let _ = match (List.head_opt event) with
      | Some (id, phase) -> print_checkmark (id = 3n && phase = 1n, true)
      | None -> print_checkmark (false, true) in
    let _ = print_step "Should catch endedTournament event" in
    

    // *************** Rewards ************* //
    let _ = print_topic "Rewards" in

    let wallet2 : tez = Test.get_balance alice_address in

    let _ = print_checkmark (wallet1 + 100tez = wallet2, true) in
    let _ = print_step "Should win tezos after the tournament" in
    
    let attr_str : attribute_storage = Test.get_storage_of_address attribute_addr |> Test.decompile in
    let attr = Big_map.find 2n attr_str.attributes in
    let _ = print_checkmark (attr.xp = 1000n + attr_before.xp, true) in   
    let _ = print_step "Should give xp to the Second" in

    // Make a second tournament to see the two other reward possibilities

    let default_tournament_def : tournament_def = ("League", NoStake, [FighterReward; ItemReward ("ticket1", 1n)], "RoundRobin", 2n, 999n ) in
    
    let _ = Test.set_source scheduler_address in
    let _ = Test.transfer_to_contract tournament_contract (CreateTournament (default_tournament_def,Tezos.get_now () + 180)) 1tez in

    let _ = Test.set_source alice_address in
    let _ = Test.transfer_to_contract tournament_contract (JoinTournament (4n, token1)) 100tez in
    let _ = Test.set_source bob_address in
    let _ = Test.transfer_to_contract tournament_contract (JoinTournament (4n, token2)) 100tez in
    
    let _ = Test.set_source scheduler_address in 
    let _ = Test.transfer_to_contract tournament_contract (GenerateTree (4n, 0x03)) 0tez in

    let _ = Test.transfer_to_contract tournament_contract (NextPhase (4n, Set.literal([1n, 2n]), 3n, 10)) 0tez in

    // Change baker to avoid problem
    let _ = Test.set_baker minter_address in
    
    let _ = Test.set_source resolver_address in
    let _ = Test.transfer_to_contract fight_contract (ResolveRound (2n, 1n, 1, 0x00)) 0tez in
    let _ = Test.transfer_to_contract fight_contract (ResolveRound (2n, 2n, 1, 0x00)) 0tez in
    let _ = Test.transfer_to_contract fight_contract (ResolveRound (2n, 3n, 1, 0x00)) 0tez in
   
    // TODO: Need to be fix (should be able to end the tournament with ticket)
    let _ = Test.set_source scheduler_address in
    let _ = Test.transfer_to_contract tournament_contract (EndTournament (4n, [2n; 1n])) 0tez in

    let fighter_str : fighter_storage = Test.get_storage_of_address fighter_addr |> Test.decompile in
    let alice_fighters = Big_map.find alice_address fighter_str.fighters_by_owner in
    let _ = print_checkmark (alice_fighters = Set.literal([token1; token3; alice_token]), true) in
    let _ = print_step "Should not give new fighter to the tourn winner (FighterReward no effect in tournament)" in

    (* let tournament : tournament_storage = Test.get_storage_of_address tournament_addr |> Test.decompile in *)
    (* let tournament_4 = Big_map.find 4n tournament.tournaments in *)
    (* let _ = Test.log (tournament_4) in *)
    (* let shop_str : shop_storage = Test.get_storage_of_address shop_addr |> Test.decompile in *)
    (* let _ = Test.log shop_str in  *)


    // ***************** SinkFees *************** //
    let _ = print_topic "SinkFees" in

    let _ = test_tournament "Should not allow user to use SinkFees entrypoint"  (alice_address, SinkFees alice_address, 0tez) false in

    let _ = test_tournament "Should allow admin to use SinkFees entrypoint"     (admin_address, SinkFees alice_address, 0tez) true in

    let _ = Test.println "" in 
    ()
