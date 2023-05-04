#include "test.utils.mligo"

let test =

    let _ = Test.set_source admin_address in
    let _ = Test.transfer_to_contract fighter_contract (SetFightAddr        fight_addr        ) 0tez in
    let _ = Test.transfer_to_contract fighter_contract (SetAbilityAddr      ability_addr     ) 0tez in
    let _ = Test.transfer_to_contract fighter_contract (SetAttributeAddr    attribute_addr   ) 0tez in
    let _ = Test.transfer_to_contract fighter_contract (SetMarketfighterAddr marketfighter_addr) 0tez in
    let _ = Test.transfer_to_contract fighter_contract (SetTournamentAddr   tournament_addr  ) 0tez in
    let _ = Test.transfer_to_contract fight_contract   (SetAttributeAddr    attribute_addr   ) 0tez in
    let _ = Test.transfer_to_contract fight_contract   (SetTournamentAddr   tournament_addr  ) 0tez in

    let _ = print_topic ("SetFighterAddr") in
    let _ = test_ability "Should not allow user to use SetFighterAddr entrypoint"  (alice_address, SetFighterAddr fighter_addr, 0tez) false in
    let _ = test_ability "Should allow admin to use SetFighterAddr entrypoint"     (admin_address, SetFighterAddr fighter_addr, 0tez) true in

    let _ = Test.set_source alice_address in
    let _ =  Test.transfer_to_contract fighter_contract (Mint) mint_fee in
    let token1 : fighter_id = 1n in
    let _ = Test.set_source bob_address in
    let _ =  Test.transfer_to_contract fighter_contract (Mint) mint_fee in
    let token2 : fighter_id = 2n in
    let _ = Test.set_source alice_address in
    let _ =  Test.transfer_to_contract fighter_contract (Mint) mint_fee in
    let token3 : fighter_id = 3n in
    
    // Use to test the fusion entrypoint
    let _ = Test.set_source alice_address in
    let _ =  Test.transfer_to_contract fighter_contract (Mint) mint_fee in
    let alice_token : fighter_id = 4n in

    let _ = Test.set_source admin_address in
    let _ = Test.transfer_to_contract fighter_contract (RealMint (token1,0xfb0504030201fb0101010101,[4n;5n;6n])) 0tez in
    let _ = Test.transfer_to_contract fighter_contract (RealMint (token2,0xfb0604030201fb0101010101,[4n;5n;6n])) 0tez in
    let _ = Test.transfer_to_contract fighter_contract (RealMint (token3,0xfb1004030201fb0101010101,[1n;6n;7n])) 0tez in

    // *************** CreateAbility *************** //
    let _ = print_topic ("CreateAbility") in

    let _ = test_ability "Should not allow user to use CreateAbility entrypoint"  (alice_address, CreateAbility rl, 0tez) false in
    
    let _ = test_ability "Should allow admin to use CreateAbility entrypoint"  (admin_address, CreateAbility rl, 0tez) true in

    let d : ability_storage = Test.get_storage_of_address ability_addr |> Test.decompile in
    let availableAbilitiesCheck : bool = Big_map.mem Common d.available_abilities && Big_map.mem Rare d.available_abilities && Big_map.mem Epic d.available_abilities && Big_map.mem Unique d.available_abilities in
    let probaCheck : bool = Map.mem Common d.proba_rarity && Map.mem Rare d.proba_rarity && Map.mem Epic d.proba_rarity && Map.mem Unique d.proba_rarity in
    let _ = Test.log d in
    let _ = print_checkmark (availableAbilitiesCheck && d.next_id = 39n && probaCheck, true) in
    let _ = print_step "Should have created abilities" in

    // *************** Mint *************** //
    let _ = print_topic ("Mint") in

    let _ = test_ability "Should not allow user to use Mint entrypoint"  (alice_address, Mint (token1, [1n]), 0tez) false in
    
    let _ = test_ability "Should not allow admin to use Mint entrypoint"  (admin_address, Mint (token1, [1n]), 0tez) false in


    // *************** SetProbaRarity *************** //
    let _ = print_topic ("SetProbaRarity") in
    
    let _ = test_ability "Should not allow user to use SetProbaRarity entrypoint"  (alice_address, SetProbaRarity (Common, 1n), 0tez) false in
    
    let _ = test_ability "Should allow admin to use SetProbaRarity entrypoint"  (admin_address, SetProbaRarity (Common, 1000000n), 0tez) true in
    
    let d : ability_storage = Test.get_storage_of_address ability_addr |> Test.decompile in
    let _ = print_checkmark (Map.find Common d.proba_rarity = 1000000n, true) in
    let _ = print_step "Should have set the proba of Common rarity to 1000000n" in


    // *************** Fusion *************** //
    let _ = print_topic ("Fusion") in
    
    let _ = test_ability "Should not allow user to use Fusion entrypoint"  (alice_address, Fusion (token1, token2, alice_token, [1n]), 0tez) false in
    
    let _ = test_ability "Should not allow admin to use Fusion entrypoint"  (admin_address, Fusion (token1, token2, alice_token, [1n]), 0tez) false in

    // *************** LearnAbility *************** //
    let _ = print_topic ("LearnAbility") in

    let _ = test_ability "Should not allow user to use LearnAbility entrypoint"  (alice_address, LearnAbility (token1, 2n), 0tez) false in

    let _ = test_ability "Should not allow admin to use LearnAbility entrypoint"  (admin_address, LearnAbility (token1, 2n), 0tez) false in

    // *************** ForgetAbility *************** //
    let _ = print_topic ("ForgetAbility") in

    let _ = test_ability "Should not allow user to use ForgetAbility entrypoint"  (alice_address, ForgetAbility (token1, 1n), 0tez) false in

    let _ = test_ability "Should not allow admin to use ForgetAbility entrypoint"  (admin_address, ForgetAbility (token1, 1n), 0tez) false in
    
    let _ = Test.println "" in
    ()