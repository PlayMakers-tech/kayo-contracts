#include "../test.utils.mligo"
#include "attribute.event.mligo"

let test =

    let _ = Test.set_source alice_address in
    let _ =  Test.transfer_to_contract fighter_contract (Mint) mint_fee in
    let _ =  Test.transfer_to_contract fighter_contract (Mint) mint_fee in
    let _ =  Test.transfer_to_contract fighter_contract (Mint) mint_fee in
    
    let token1 : fighter_id = 1n in
    let token2 : fighter_id = 2n in
    let token3 : fighter_id = 3n in

    let _ = Test.set_source minter_address in
    let _ = Test.transfer_to_contract fighter_contract (RealMint (token1,0xfb0504030201fb0101010101,[4n;5n;6n])) 0tez in
    let _ = Test.transfer_to_contract fighter_contract (RealMint (token2,0xfb0604030201fb0101010101,[4n;5n;6n])) 0tez in

    // ************ SetAdmins ************ //
    let _ = print_topic "SetAdmins" in

    let _ = test_attribute "Should not allow user to use SetAdmins entrypoint"  (alice_address, SetAdmins (Set.literal [admin_address]), 0tez) false in

    let _ = test_attribute "Should not allow manager to use SetAdmins entrypoint" (manager_address, SetAdmins (Set.literal [admin_address]), 0tez) false in

    let _ = test_attribute "Should allow admin to use SetAdmins entrypoint"   (admin_address, SetAdmins (Set.literal [admin_address]), 0tez) true in

    // ************ SetManagers ************ //
    let _ = print_topic "SetManagers" in

    let _ = test_attribute "Should not allow user to use SetManagers entrypoint"  (alice_address, SetManagers (Set.literal [manager_address;(fighter_addr: address);(fight_addr: address);(tournament_addr: address)]), 0tez) false in

    let _ = test_attribute "Should not allow manager to use SetManagers entrypoint" (manager_address, SetManagers (Set.literal [manager_address;(fighter_addr: address);(fight_addr: address);(tournament_addr: address)]), 0tez) false in

    let _ = test_attribute "Should allow admin to use SetManagers entrypoint" (admin_address, SetManagers (Set.literal [manager_address;(fighter_addr: address);(fight_addr: address);(tournament_addr: address)]), 0tez) true in
    
    // ************ SetSkinNodes ********** // 
    let _ = print_topic "SetSkinNodes" in

    let _ = test_attribute "Should not allow user to use SetSkinNodes entrypoint"  (alice_address, SetSkinNodes (token1, [(0x00, 1n, 1n)]), 0tez) false in

    let _ = test_attribute "Should not allow admin to use SetSkinNodes entrypoint" (admin_address, SetSkinNodes (token1, [(0x00, 1n, 1n)]), 0tez) false in

    let _ = test_attribute "Should allow manager to use SetSkinNodes entrypoint"   (manager_address, SetSkinNodes (token1, [(0x00, 1n, 1n)]), 0tez) true in

    let d : attribute_storage = Test.get_storage_of_address attribute_addr |> Test.decompile in
    let n, a : nat * (attribute_skin_node list) = d.skin_nodes in
    let _ = print_checkmark (n = 1n && List.size a = 1n, true) in
    let _ = print_step "SetSkinNodes works" in

    // ************ SetSkinLeaves ******** // 
    let _ = print_topic "SetSkinLeaves" in

    let _ = test_attribute "Should not allow user to use SetSkinLeaves entrypoint"  (alice_address, SetSkinLeaves (token1, [(0x00, 1n, 1n)]), 0tez) false in

    let _ = test_attribute "Should not allow admin to use SetSkinLeaves entrypoint"     (admin_address, SetSkinLeaves (token1, [(0x00, 1n, 1n)]), 0tez) false in
    
    let _ = test_attribute "Should allow manager to use SetSkinLeaves entrypoint"   (manager_address, SetSkinLeaves (token1, [(0x00, 1n, 1n)]), 0tez) true in
 
    let d : attribute_storage = Test.get_storage_of_address attribute_addr |> Test.decompile in
    let n, a : nat * (attribute_skin_node list) = d.skin_nodes in
    let _ = print_checkmark (n = 1n && List.size a = 1n, true) in
    let _ = print_step "SetSkinLeaves works" in

    // ************ EarnXP ******** // 
    let _ = print_topic "EarnXP" in

    let _ = test_attribute "Should not allow user to use EarnXP entrypoint"  (alice_address, EarnXP (token1, 100n), 0tez) false in

    let _ = test_attribute "Should not allow admin to use EarnXP entrypoint"     (admin_address, EarnXP (token1, 100n), 0tez) false in

    let _ = test_attribute "Should allow manager to use EarnXP entrypoint"     (manager_address, EarnXP (token1, 100n), 0tez) true in

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

    let _ = test_attribute "Should not allow user to use Mint entrypoint"  (alice_address, Mint (token1, 0x00), 0tez) false in

    let _ = test_attribute "Should not allow admin to use Mint entrypoint" (admin_address, Mint (token1, 0x00), 0tez) false in

    let _ = test_attribute "Should allow manager to use Mint entrypoint"   (manager_address, Mint (token1, 0x00), 0tez) true in

    // ************ Fusion ******** // 
    let _ = print_topic "Fusion" in
    
    let _ = test_attribute "Should not allow user to use Fusion entrypoint"  (alice_address, Fusion (token3, token1, token2, 0x00), 0tez) false in

    let _ = test_attribute "Should not allow admin to use Fusion entrypoint" (admin_address, Fusion (token3, token1, token2, 0x00), 0tez) false in

    let _ = test_attribute "Should allow manager to use Fusion entrypoint"   (manager_address, Fusion (token3, token1, token2, 0x00), 0tez) true in

    let _ = Test.println "" in
    ()
