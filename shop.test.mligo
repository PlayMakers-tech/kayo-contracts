#include "schema.mligo"
#include "error.mligo"
#include "utils.mligo"
#include "test.utils.mligo"
#include "event.mligo"

let test =
    let _ = Test.reset_state 4n [] in
    let admin_address = Test.nth_bootstrap_account 1 in
    let alice_address = Test.nth_bootstrap_account 2 in
    let bob_address   = Test.nth_bootstrap_account 3 in
    // let dummy_address = ("tz3QE72w1vC613ZaeAZwW5fxwrBUQAstyaZy": address) in
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


    let test_shop (name : string) (addr, op, amount : address * shop_parameter * tez) (expected : bool) =
        let _ = Test.set_source addr in
        test_entrypoint name (Test.transfer_to_contract shop_contract op amount) expected
    in

    // *********** Create items ***********
    let _ = print_topic "Create items" in

    let ticket1 : shop_item_data = {
        item = ("ticket1":shop_item);
        quantity = 99999999n;
        price = 1tez;
        consumers = Set.literal [bob_address]
    } in

    let _ = test_shop "Should not allow user to create item" (alice_address, (NewItem ticket1), 0tez) false in
    
    let _ = test_shop "Should allow admin to create item" (admin_address, (NewItem ticket1), 0tez) true in
    
    let _ = test_shop "Should not allow admin to override item" (admin_address, (NewItem ticket1), 0tez) false in
    

    let ticket2 : shop_item_data = {
        item = ("ticket2":shop_item);
        quantity = 5n;
        price = 2tez;
        consumers = Set.empty
    } in
    
    let _ = test_shop "Should allow admin to create a second item" (admin_address, (NewItem ticket2), 0tez) true in

    // *********** Create bundles ***********
    let _ = print_topic "Create bundles" in

    let bundle1 : shop_bundle_data = {
        bundle = ("bundle1":shop_bundle);
        quantity = 10n;
        price = 3tez;
        items = Map.literal [("ticket1":shop_item),2n;("ticket2":shop_item),1n]
    } in

    let _ = test_shop "Should not allow user to create bundle" (alice_address, (NewBundle bundle1), 0tez) false in
    
    let _ = test_shop "Should allow admin to create bundle" (admin_address, (NewBundle bundle1), 0tez) true in
    
    let _ = test_shop "Should not allow admin to override bundle" (admin_address, (NewBundle bundle1), 0tez) false in

    // *********** SetShopOpen *********** //
    let _ = print_topic "SetShopOpen" in

    let _ = test_shop "Should not allow user to set shop closed" (alice_address, (SetShopOpen false), 0tez) false in
    
    let _ = test_shop "Should allow admin to set shop closed" (admin_address, (SetShopOpen false), 0tez) true in

    let d : shop_storage = Test.get_storage shop_typed_addr in
    let _ = print_checkmark (d.is_open, false) in
    let _ = print_step "Shop is closed in memory" in
    
    let _ = Test.set_source admin_address in
    let _ = Test.transfer_to_contract shop_contract (SetShopOpen true) 0tez in

    // *********** Buy items ***********
    let _ = print_topic "Buy items" in

    let _ = test_shop "Should allow user to buy 1 ticket 1" (alice_address, (BuyItem ("ticket1",1n)), 1tez) true in

    let event : event_bought_item list = Test.get_last_events_from shop_typed_addr "boughtItem" in
    let _ = match (List.head_opt event) with
      | Some (item, q, addr) -> print_checkmark (item = "ticket1" && q = 1n && addr = alice_address, true)
      | None -> print_checkmark (false, true) in
    let _ = print_step "Should catch boughtItem event" in
    
    let _ = test_shop "Should allow user to buy 2 ticket 1" (alice_address, (BuyItem ("ticket1",2n)), 2tez) true in
    
    let _ = test_shop "Should allow user to buy 1 ticket 2" (alice_address, (BuyItem ("ticket2",1n)), 2tez) true in
    
    let _ = test_shop "Should not allow user to buy 1 ticket 2 at the wrong price" (alice_address, (BuyItem ("ticket2",1n)), 1tez) false in
    
    let _ = test_shop "Should not allow user to buy 10 ticket 2 if there is not enough ticket 2" (alice_address, (BuyItem ("ticket2",10n)), 20tez) false in

    let d : shop_storage = Test.get_storage shop_typed_addr in
    let alice_tickets = Big_map.find alice_address d.owned_items in
    let n_ticket = Map.find "ticket1" alice_tickets in
    let _ = print_checkmark (n_ticket = 3n, true) in
    let _ = print_step "Alice has 3 ticket 1 in memory" in

    let _ = Test.set_source admin_address in
    let _ = Test.transfer_to_contract shop_contract (SetShopOpen false) 0tez in
    let _ = test_shop "Should not allow user to buy ticket when shop is closed" (alice_address, (BuyItem ("ticket1",1n)), 1tez) false in
    let _ = Test.set_source admin_address in
    let _ = Test.transfer_to_contract shop_contract (SetShopOpen true) 0tez in


    // *********** SetItemPrice *********** //
    let _ = print_topic "SetItemPrice" in

    let _ = test_shop "Should not allow user to set item price" (alice_address, (SetItemPrice ("ticket1",1tez)), 0tez) false in
    
    let _ = test_shop "Should allow admin to set item price" (admin_address, (SetItemPrice ("ticket1",10tez)), 0tez) true in

    let d : shop_storage = Test.get_storage shop_typed_addr in
    let d : shop_item_data = Map.find "ticket1" d.items in
    let _ = print_checkmark (d.price=10tez, true) in
    let _ = print_step "Item price is set to 10 tez in memory" in

    // *********** Buy bundle ***********
    let _ = print_topic "Buy bundles" in

    let _ = test_shop "Should allow user to buy 1 bundle 1" (alice_address, (BuyBundle ("bundle1",1n)), 3tez) true in
    
    let event : event_bought_bundle list = Test.get_last_events_from shop_typed_addr "boughtBundle" in
    let _ = match (List.head_opt event) with
      | Some (item, q, addr) -> print_checkmark (item = "bundle1" && q = 1n && addr = alice_address, true)
      | None -> print_checkmark (false, true) in
    let _ = print_step "Should catch boughtBundle event" in

    let d : shop_storage = Test.get_storage shop_typed_addr in
    let alice_tickets = Big_map.find alice_address d.owned_items in
    let n_ticket1 = Map.find "ticket1" alice_tickets in
    let n_ticket2 = Map.find "ticket2" alice_tickets in
    let _ = print_checkmark (n_ticket1 = 5n && n_ticket2 = 2n, true) in
    let _ = print_step "Alice has 5 ticket 1 and 2 ticket 2 in memory" in
    
    let _ = test_shop "Should not allow the user to purchase more bundles than possible" (alice_address, (BuyBundle ("bundle1",10n)), 30tez) false in

  
    // *********** Consume items ***********
    let _ = print_topic "Consume items" in

    let _ = test_shop "Should allow the correct user to consume 4 ticket 1 of a specific user" (bob_address, (ConsumeItem ("ticket1",4n,alice_address)), 1tez) true in
    
    let _ = test_shop "Should not allow user to consume 1 ticket 1 because he is not in consumers list" (alice_address, (ConsumeItem ("ticket1",1n,alice_address)), 1tez) false in

    let _ = test_shop "Should not allow the correct user to consume more ticket than possible" (bob_address, (ConsumeItem ("ticket1",4n,alice_address)), 1tez) false in

    // *********** SetItemConsumers *********** //
    let _ = print_topic "SetItemConsumers" in

    let _ = test_shop "Should not allow user to set item consumers" (alice_address, (SetItemConsumers ("ticket1",Set.literal [alice_address])), 0tez) false in
    
    let _ = test_shop "Should allow admin to set item consumers" (admin_address, (SetItemConsumers ("ticket1",Set.literal [alice_address; bob_address])), 0tez) true in

    let d : shop_storage = Test.get_storage shop_typed_addr in
    let d : shop_item_data = Map.find "ticket1" d.items in
    let _ = print_checkmark (Set.mem alice_address (d.consumers) && Set.mem bob_address (d.consumers), true) in
    let _ = print_step "Item consumers are set to alice and bob in memory" in

    // *********** SetBundlePrice *********** //
    let _ = print_topic "SetBundlePrice" in

    let _ = test_shop "Should not allow user to set bundle price" (alice_address, (SetBundlePrice ("bundle1",1tez)), 0tez) false in
    
    let _ = test_shop "Should allow admin to set bundle price" (admin_address, (SetBundlePrice ("bundle1",10tez)), 0tez) true in

    let d : shop_storage = Test.get_storage shop_typed_addr in
    let d : shop_bundle_data = Map.find "bundle1" d.bundles in
    let _ = print_checkmark (d.price=10tez, true) in
    let _ = print_step "Bundle price is set to 10 tez in memory" in

    // *********** DeleteBundle *********** //
    let _ = print_topic "DeleteBundle" in

    let _ = test_shop "Should not allow user to delete bundle" (alice_address, (DeleteBundle "bundle1"), 0tez) false in

    let _ = test_shop "Should allow admin to delete bundle" (admin_address, (DeleteBundle "bundle1"), 0tez) true in

    let d : shop_storage = Test.get_storage shop_typed_addr in
    let _ = print_checkmark (Map.mem "bundle1" d.bundles, false) in
    let _ = print_step "Bundle is deleted in memory" in

    // *********** GrantItem *********** //
    let _ = print_topic "GrantItem" in

    let _ = test_shop "Should not allow user to grant item" (alice_address, (GrantItem ("ticket1",1n,bob_address)), 10tez) false in

    let _ = test_shop "Should allow admin to grant item" (admin_address, (GrantItem ("ticket1",10n,bob_address)), 100tez) true in
    
    let _ = test_shop "Should not allow admin to grant item with the incorrect price of the bundle" (admin_address, (GrantItem ("ticket1",10n,bob_address)), 10tez) false in

    let d : shop_storage = Test.get_storage shop_typed_addr in
    let _ = print_checkmark ((Map.find "ticket1" (Big_map.find bob_address d.owned_items))=10n, true) in
    let _ = print_step "Bob owns 10 ticket1 in memory" in

    // *********** SinkFees *********** //
    let _ = print_topic "SinkFees" in

    let _ = test_shop "Should not allow user to use SinkFees" (alice_address, (SinkFees (alice_address)), 0tez) false in
    
    let _ = test_shop "Should allow admin to use SinkFees" (admin_address, (SinkFees (alice_address)), 0tez) true in

    let _ = Test.println "" in

    ()