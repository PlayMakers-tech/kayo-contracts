#include "../test.utils.mligo"

let test =

    let _ = Test.set_source alice_address in
    let _ =  Test.transfer_to_contract fighter_contract (Mint) mint_fee in
    let _ = Test.set_source bob_address in
    let _ =  Test.transfer_to_contract fighter_contract (Mint) mint_fee in
    let _ = Test.set_source alice_address in
    let _ =  Test.transfer_to_contract fighter_contract (Mint) mint_fee in

    let token1 : fighter_id = 1n in
    let token2 : fighter_id = 2n in
    let token3 : fighter_id = 3n in

    // *************** SetAdmins *************** //
    let _ = print_topic ("SetAdmins") in
    
    let _ = test_ability "Should not allow user to use SetAdmins entrypoint"  (alice_address, SetAdmins (Set.literal [admin_address]), 0tez) false in

    let _ = test_ability "Should not allow manager to use SetAdmins entrypoint"  (manager_address, SetAdmins (Set.literal [admin_address]), 0tez) false in

    let _ = test_ability "Should allow admin to use SetAdmins entrypoint"  (admin_address, SetAdmins (Set.literal [admin_address]), 0tez) true in

    // *************** SetManagers *************** //
    let _ = print_topic ("SetManagers") in

    let _ = test_ability "Should not allow user to use SetManagers entrypoint"  (alice_address, SetManagers (Set.literal [manager_address;(fighter_addr: address)]), 0tez) false in

    let _ = test_ability "Should not allow manager to use SetManagers entrypoint"  (manager_address, SetManagers (Set.literal [manager_address;(fighter_addr: address)]), 0tez) false in

    let _ = test_ability "Should allow admin to use SetManagers entrypoint"  (admin_address, SetManagers (Set.literal [manager_address;(fighter_addr: address)]), 0tez) true in

    // *************** CreateAbility *************** //
    let _ = print_topic ("CreateAbility") in

    // rl is defined in test.utils.mligo
    let _ = test_ability "Should not allow user to use CreateAbility entrypoint"  (alice_address, CreateAbility rl, 0tez) false in
    
    let _ = test_ability "Should not allow admin to use CreateAbility entrypoint"  (admin_address, CreateAbility rl, 0tez) false in
    
    let _ = test_ability "Should allow manager to use CreateAbility entrypoint"  (manager_address, CreateAbility rl, 0tez) true in

    let d : ability_storage = Test.get_storage_of_address ability_addr |> Test.decompile in
    let availableAbilitiesCheck : bool = Big_map.mem Common d.available_abilities && Big_map.mem Rare d.available_abilities && Big_map.mem Epic d.available_abilities && Big_map.mem Unique d.available_abilities in
    let probaCheck : bool = Map.mem Common d.proba_rarity && Map.mem Rare d.proba_rarity && Map.mem Epic d.proba_rarity && Map.mem Unique d.proba_rarity in
    let _ = print_checkmark (availableAbilitiesCheck && d.next_id = 39n && probaCheck, true) in
    let _ = print_step "Should have created abilities" in

    // *************** Mint *************** //
    let _ = print_topic ("Mint") in

    let _ = test_ability "Should not allow user to use Mint entrypoint"  (alice_address, Mint (token1, [1n]), 0tez) false in
    
    let _ = test_ability "Should not allow admin to use Mint entrypoint"  (admin_address, Mint (token1, [1n]), 0tez) false in

    let _ = test_ability "Should allow manager to use Mint entrypoint"  (manager_address, Mint (token1, [1n]), 0tez) true in

    // *************** SetProbaRarity *************** //
    let _ = print_topic ("SetProbaRarity") in
    
    let _ = test_ability "Should not allow user to use SetProbaRarity entrypoint"  (alice_address, SetProbaRarity (Common, 1n), 0tez) false in
    
    let _ = test_ability "Should not allow admin to use SetProbaRarity entrypoint"  (admin_address, SetProbaRarity (Common, 1000000n), 0tez) false in
    
    let _ = test_ability "Should allow manager to use SetProbaRarity entrypoint"  (manager_address, SetProbaRarity (Common, 1000000n), 0tez) true in
    
    let d : ability_storage = Test.get_storage_of_address ability_addr |> Test.decompile in
    let _ = print_checkmark (Map.find Common d.proba_rarity = 1000000n, true) in
    let _ = print_step "Should have set the proba of Common rarity to 1000000n" in


    // *************** Fusion *************** //
    let _ = print_topic ("Fusion") in
    
    let _ = test_ability "Should not allow user to use Fusion entrypoint"  (alice_address, Fusion (token1, token2, token3, [1n]), 0tez) false in
    
    let _ = test_ability "Should not allow admin to use Fusion entrypoint"  (admin_address, Fusion (token1, token2, token3, [1n]), 0tez) false in

    let _ = test_ability "Should allow manager to use Fusion entrypoint"  (manager_address, Fusion (token1, token2, token3, [1n]), 0tez) true in

    // *************** LearnAbility *************** //
    let _ = print_topic ("LearnAbility") in

    let _ = test_ability "Should not allow user to use LearnAbility entrypoint"  (alice_address, LearnAbility (token1, 2n), 0tez) false in

    let _ = test_ability "Should not allow admin to use LearnAbility entrypoint"  (admin_address, LearnAbility (token1, 2n), 0tez) false in

    let _ = test_ability "Should allow manager to use LearnAbility entrypoint"  (manager_address, LearnAbility (token1, 2n), 0tez) true in
    
    // *************** ForgetAbility *************** //
    let _ = print_topic ("ForgetAbility") in

    let _ = test_ability "Should not allow user to use ForgetAbility entrypoint"  (alice_address, ForgetAbility (token1, 1n), 0tez) false in

    let _ = test_ability "Should not allow admin to use ForgetAbility entrypoint"  (admin_address, ForgetAbility (token1, 1n), 0tez) false in
    
    let _ = test_ability "Should allow manager to use ForgetAbility entrypoint"  (manager_address, ForgetAbility (token1, 1n), 0tez) true in
    
    let _ = Test.println "" in
    ()
