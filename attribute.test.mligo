#include "test.utils.mligo"
#include "event.mligo"

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
    let _ = test_attribute "Should not allow user to use SetFighterAddr entrypoint"  (alice_address, SetFighterAddr fighter_addr, 0tez) false in
    let _ = test_attribute "Should allow admin to use SetFighterAddr entrypoint"     (admin_address, SetFighterAddr fighter_addr, 0tez) true in

    let _ = print_topic ("SetFightAddr") in
    let _ = test_attribute "Should not allow user to use SetFightAddr entrypoint"  (alice_address, SetFightAddr fight_addr, 0tez) false in
    let _ = test_attribute "Should allow admin to use SetFightAddr entrypoint"     (admin_address, SetFightAddr fight_addr, 0tez) true in

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

    // ************ SetSkinNodes ******** // 
    let _ = print_topic "SetSkinNodes" in


    let _ = test_attribute "Should not allow user to use SetSkinNodes entrypoint"  (alice_address, SetSkinNodes (token1, [(0x00, 1n, 1n)]), 0tez) false in

    let _ = test_attribute "Should allow admin to use SetSkinNodes entrypoint"     (admin_address, SetSkinNodes (token1, [(0x00, 1n, 1n)]), 0tez) true in

    let d : attribute_storage = Test.get_storage_of_address attribute_addr |> Test.decompile in
    let n, a : nat * (attribute_skin_node list) = d.skin_nodes in
    let _ = print_checkmark (n = 1n && List.size a = 1n, true) in
    let _ = print_step "SetSkinNodes works" in

    // ************ SetSkinLeaves ******** // 
    let _ = print_topic "SetSkinLeaves" in

    let _ = test_attribute "Should not allow user to use SetSkinLeaves entrypoint"  (alice_address, SetSkinLeaves (token1, [(0x00, 1n, 1n)]), 0tez) false in

    let _ = test_attribute "Should allow admin to use SetSkinLeaves entrypoint"     (admin_address, SetSkinLeaves (token1, [(0x00, 1n, 1n)]), 0tez) true in
 
    let d : attribute_storage = Test.get_storage_of_address attribute_addr |> Test.decompile in
    let n, a : nat * (attribute_skin_node list) = d.skin_nodes in
    let _ = print_checkmark (n = 1n && List.size a = 1n, true) in
    let _ = print_step "SetSkinLeaves works" in


    // ************ EarnXP ******** // 
    let _ = print_topic "EarnXP" in

    let _ = test_attribute "Should not allow user to use EarnXP entrypoint"  (alice_address, EarnXP (token1, 100n), 0tez) false in

    let _ = test_attribute "Should allow admin to use EarnXP entrypoint"     (admin_address, EarnXP (token1, 100n), 0tez) true in

    let event : event_level_up list = Test.get_last_events_from attribute_typed_addr "levelUp" in
    let _ = match (List.head_opt event) with
      | Some (id, lv) -> print_checkmark (id = token1 && lv = 3n, true)
      | None -> print_checkmark (false, true) in
    let _ = print_step "Should catch levelUp event" in

    let d : attribute_storage = Test.get_storage_of_address attribute_addr |> Test.decompile in
    let d : attribute_data = Big_map.find token1 d.attributes in
    let _ = print_checkmark (d.xp = 100n, true) in
    let _ = print_step "EarnXP works" in

    // ************ Mint ******** //
    let _ = print_topic "Mint" in

    let _ = test_attribute "Should not allow user to use Mint entrypoint"  (alice_address, Mint (1n, 0x00), 0tez) false in

    let _ = test_attribute "Should not allow admin to use Mint entrypoint" (admin_address, Mint (1n, 0x00), 0tez) false in

    // ************ Fusion ******** // 
    let _ = print_topic "Fusion" in
    
    let _ = test_attribute "Should not allow user to use Fusion entrypoint"  (alice_address, Fusion (1n, 2n, 6n, 0x00), 0tez) false in

    let _ = test_attribute "Should not allow admin to use Fusion entrypoint" (admin_address, Fusion (1n, 2n, 6n, 0x00), 0tez) false in

    let _ = Test.println "" in
    ()