#include "attribute.schema.mligo"

let test =
    let _ = Test.reset_state 4n [] in
    let admin_address = Test.nth_bootstrap_account 1 in
    let alice_address = Test.nth_bootstrap_account 2 in
    let bob_address   = Test.nth_bootstrap_account 3 in
    let dummy_address = ("tz3QE72w1vC613ZaeAZwW5fxwrBUQAstyaZy": address) in
    let _ = Test.set_source admin_address in

    // Attribute contract
	let init_store: attribute_storage = {
	    fight_addr = (admin_address: address);
	    fighter_addr = (admin_address: address);
        admin = (admin_address : address);
        skin_nodes = (1n, [(0x10,1n,1n)]);
        skin_leaves = (1n, [(0x00,2n,1n)]);
	    attributes = Big_map.empty
	} in
    let attribute_addr, _, _ = Test.originate_from_file "attribute.mligo" "main" [] (Test.eval init_store) 0tez in
    let attribute_typed_addr: (attribute_parameter, attribute_storage) typed_address = Test.cast_address attribute_addr in
    let attribute_contract = Test.to_contract attribute_typed_addr in

	let d : attribute_storage = Test.get_storage_of_address attribute_addr |> Test.decompile in
	let (p, l) : (nat* attribute_skin_node list) = d.skin_leaves in
    let _ = Test.log p in
    let _ = Test.log l in


    // Minting 
    let _ = 
        (match Test.transfer_to_contract attribute_contract (Mint 1n) 0tez with
        | Success _ -> true
        | Fail err -> Test.failwith err )
        |> Test.assert 
    in
    let _ = 
        (match Test.transfer_to_contract attribute_contract (Mint 2n) 0tez with
        | Success _ -> true
        | Fail err -> Test.failwith err )
        |> Test.assert 
    in

    // Fusion
    let _ = 
        (match Test.transfer_to_contract attribute_contract (Fusion (3n,1n,2n)) 0tez with
        | Success _ -> true
        | Fail err -> Test.failwith err )
        |> Test.assert 
    in

    let d : attribute_storage = Test.get_storage_of_address attribute_addr |> Test.decompile in
	let _ = Test.log d.attributes in

    ()