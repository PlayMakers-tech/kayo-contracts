#include "schema.mligo"
#include "error.mligo"
#include "utils.mligo"


let test =
    let _ = Test.reset_state 4n [] in
    let admin_address = Test.nth_bootstrap_account 1 in
    let alice_address = Test.nth_bootstrap_account 2 in
    let bob_address   = Test.nth_bootstrap_account 3 in
    let dummy_address = ("tz3QE72w1vC613ZaeAZwW5fxwrBUQAstyaZy": address) in
    let _ = Test.set_source admin_address in

    // Shop contract
    let init_store : shop_storage = {
        is_open = true;
        admin = (admin_address: address);
        items = Map.empty;
        bundles = Map.empty;
        owned_items = Big_map.empty        
    } in
    let shop_addr, _, _ = Test.originate_from_file "shop.mligo" "main" [] (Test.eval init_store) 0tez in
    let shop_typed_addr: (shop_parameter, shop_storage) typed_address = Test.cast_address shop_addr in
    let shop_contract = Test.to_contract shop_typed_addr in


    // Dump function for probing    
    let dump (addr : address ) =
        let d : shop_storage = Test.get_storage_of_address shop_addr |> Test.decompile in
        let b = Big_map.find_opt addr d.owned_items in
        let _ = Test.log (Option.unopt b) in
        () in



    // Create items
    let ticket1 : shop_item_data = {
        item = ("ticket1":shop_item);
        quantity = 99999999n;
        price = 1tez;
        consumers = Set.literal [bob_address]
    } in
    let _ = 
        (match Test.transfer_to_contract shop_contract (NewItem ticket1) 0tez with
        | Success _ -> true
        | Fail err -> Test.failwith err )
        |> Test.assert 
    in
    let ticket2 : shop_item_data = {
        item = ("ticket2":shop_item);
        quantity = 5n;
        price = 2tez;
        consumers = Set.empty
    } in
    let _ = 
        (match Test.transfer_to_contract shop_contract (NewItem ticket2) 0tez with
        | Success _ -> true
        | Fail err -> Test.failwith err )
        |> Test.assert 
    in

    // Create bundle
    let bundle1 : shop_bundle_data = {
        bundle = ("bundle1":shop_bundle);
        quantity = 99999999n;
        price = 3tez;
        items = Map.literal [("ticket1":shop_item),2n;("ticket2":shop_item),1n]
    } in
    let _ = 
        (match Test.transfer_to_contract shop_contract (NewBundle bundle1) 0tez with
        | Success _ -> true
        | Fail err -> Test.failwith err )
        |> Test.assert 
    in

    // Buy items and bundles 
    let _ = Test.set_source alice_address in
    let _ = 
        (match Test.transfer_to_contract shop_contract (BuyItem ("ticket1",1n)) 1tez with
        | Success _ -> true
        | Fail err -> Test.failwith err )
        |> Test.assert 
    in
    let _ = 
        (match Test.transfer_to_contract shop_contract (BuyItem ("ticket1",2n)) 2tez with
        | Success _ -> true
        | Fail err -> Test.failwith err )
        |> Test.assert 
    in
    let _ = 
        (match Test.transfer_to_contract shop_contract (BuyItem ("ticket2",1n)) 2tez with
        | Success _ -> true
        | Fail err -> Test.failwith err )
        |> Test.assert 
    in
    let _ = 
        (match Test.transfer_to_contract shop_contract (BuyBundle ("bundle1",1n)) 3tez with
        | Success _ -> true
        | Fail err -> Test.failwith err )
        |> Test.assert 
    in

    // Consume items
    let _ = Test.set_source bob_address in
    let _ = 
        (match Test.transfer_to_contract shop_contract (ConsumeItem ("ticket1",4n,alice_address)) 0tez with
        | Success _ -> true
        | Fail err -> Test.failwith err )
        |> Test.assert 
    in

    // Probing
    let _ = dump (alice_address) in

    ()