#include "test.utils.mligo"
#include "event.mligo"

let test =

    let _ = Test.set_source admin_address in
    let _ = Test.transfer_to_contract fighter_contract (SetAbilityAddr      ability_addr     ) 0tez in
    let _ = Test.transfer_to_contract fighter_contract (SetAttributeAddr    attribute_addr   ) 0tez in
    let _ = Test.transfer_to_contract fight_contract   (SetAttributeAddr    attribute_addr   ) 0tez in

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

    
    // ***************** SetTournamentFee *************** //
    let _ = print_topic "SetTournamentFee" in

    let _ = test_tournament "Should not allow user to use SetTournamentFee entrypoint"  (alice_address, SetTournamentFee 100tez, 0tez) false in

    let _ = test_tournament "Should allow admin to use SetTournamentFee entrypoint"     (admin_address, SetTournamentFee 100tez, 0tez) true in    

    let d : tournament_storage = Test.get_storage_of_address tournament_addr |> Test.decompile in
    let _ = print_checkmark (d.tournament_fee=100tez, true) in
    let _ = print_step "Tournament fee is updated in the memory" in

    // ***************** CreateTournament *************** //
    let _ = print_topic "CreateTournament" in

    let default_tournament_def : tournament_def = ("League", NoStake, [], "RoundRobin", 2n, 999n ) in

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

    // TODO .
    let _ = print_checkmark (true, false) in
    let _ = print_step "todo Should test cancelling after fighters have joined (with stakes)" in

    // ***************** JoinTournament *************** //
    let _ = print_topic "JoinTournament" in

    let _ = Test.set_source scheduler_address in
    let _ = Test.transfer_to_contract tournament_contract (CreateTournament (default_tournament_def,Tezos.get_now () + 180)) 0tez in

    let _ = test_tournament "Should not allow user to use JoinTournament with a non-owned fighter"  (alice_address, JoinTournament (2n, token2), 100tez) false in

    let _ = test_tournament "Should not allow user to use JoinTournament with an incorrect fee" (alice_address, JoinTournament (2n, token1), 10tez) false in
    
    let _ = Test.set_source manager_address in
    let _ = Test.transfer_to_contract fighter_contract (SetFighterState (token1, 0n, 0n, Some default_queue)) 0tez in
    let _ = test_tournament "Should not allow user to use JoinTournament if the fighter is in queue" (alice_address, JoinTournament (2n, token1), 100tez) false in

    let _ = Test.set_source manager_address in
    let _ = Test.transfer_to_contract fighter_contract (SetFighterState (token1, 0n, 1n, None)) 0tez in
    let _ = test_tournament "Should not allow user to use JoinTournament if the fighter is already in tounament" (alice_address, JoinTournament (2n, token1), 100tez) false in

    let _ = Test.set_source manager_address in
    let _ = Test.transfer_to_contract fighter_contract (SetFighterState (token1, 1n, 0n, None)) 0tez in
    let _ = test_tournament "Should not allow user to use JoinTournament if the fighter is in fight" (alice_address, JoinTournament (2n, token1), 100tez) false in
    
    let _ = Test.set_source manager_address in
    let _ = Test.transfer_to_contract fighter_contract (SetFighterListed (token1, true)) 0tez in
    let _ = test_tournament "Should not allow user to use JoinTournament if the fighter is listed" (alice_address, JoinTournament (2n, token1), 100tez) false in
    let _ = Test.set_source manager_address in
    let _ = Test.transfer_to_contract fighter_contract (SetFighterListed (token1, false)) 0tez in

    let _ = Test.set_source alice_address in
    let _ = Test.transfer_to_contract fighter_contract (Mint) 20tez in
    let token4 : fighter_id = 6n in
    let _ = test_tournament "Should not allow user to use JoinTournament if the fighter is not fully minted" (alice_address, JoinTournament (2n, token4), 100tez) false in
    
    let _ = Test.set_source manager_address in
    let _ = Test.transfer_to_contract_exn fighter_contract (SetFighterState (token1, 0n, 0n, None)) 0tez in
    let _ = test_tournament "Should allow user to use JoinTournament" (alice_address, JoinTournament (2n, token1), 100tez) true in


    // ***************** GenerateTree *************** //
    let _ = print_topic "GenerateTree" in

    let _ = Test.set_source bob_address in
    let _ = Test.transfer_to_contract tournament_contract (JoinTournament (2n, token2)) 100tez in

    let _ = test_tournament "Should not allow user to use GenerateTree entrypoint"  (alice_address, GenerateTree (2n, 0x01), 0tez) false in
    
    let _ = test_tournament "Should not allow admin to use GenerateTree entrypoint"  (admin_address, GenerateTree (2n, 0x02), 0tez) false in

    let _ = test_tournament "Should allow scheduler to use GenerateTree entrypoint"     (scheduler_address, GenerateTree (2n, 0x03), 0tez) true in

    let event : event_generate_tree list = Test.get_last_events_from tournament_typed_addr "generatedTree" in
    let _ = match (List.head_opt event) with
      | Some (id,metadata) -> print_checkmark (id = 2n && metadata = 0x03, true)
      | None -> print_checkmark (false, true) in
    let _ = print_step "Should catch generatedTree event catch" in

    // ***************** NextPhase *************** //
    let _ = print_topic "NextPhase" in

    // TODO .
    let _ = print_checkmark (true, false) in
    let _ = print_step "todo Should test NextPhase" in

    // *************** EndTournament ************* //
    let _ = print_topic "EndTournament" in

    // TODO .
    let _ = print_checkmark (true, false) in
    let _ = print_step "todo Should test EndTournament" in

    // *************** ReportFight ************* //
    let _ = print_topic "ReportFight" in

    // TODO .
    let _ = print_checkmark (true, false) in
    let _ = print_step "todo Should make sure ReportFight works (upon fight start and fight end)" in

    // *************** Rewards ************* //
    let _ = print_topic "Rewards" in

    // TODO .
    let _ = print_checkmark (true, false) in
    let _ = print_step "todo Should test different kind of ranks/rewards combinations" in

    // ***************** SinkFees *************** //
    let _ = print_topic "SinkFees" in

    let _ = test_tournament "Should not allow user to use SinkFees entrypoint"  (alice_address, SinkFees alice_address, 0tez) false in

    let _ = test_tournament "Should allow admin to use SinkFees entrypoint"     (admin_address, SinkFees alice_address, 0tez) true in
    
    let _ = Test.println "" in 
    ()