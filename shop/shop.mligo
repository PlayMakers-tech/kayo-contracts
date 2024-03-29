#include "shop.schema.mligo"
#include "shop.event.mligo"
#include "../error.mligo"

let _get_item_data (a, d: shop_item * shop_storage) =
    (Option.unopt_with_error (Map.find_opt a d.items) ERROR.shop_item : shop_item_data)
let _get_bundle_data (a, d: shop_bundle * shop_storage) =
    (Option.unopt_with_error (Map.find_opt a d.bundles) ERROR.shop_bundle : shop_bundle_data)


(** Private function to check that the caller is admin *)
let _admin_only (d: shop_storage) =
    if not (Set.mem (Tezos.get_sender ()) d.admins) then failwith ERROR.rights_admin

(** Private function to check that the caller is manager *)
let _manager_only (d: shop_storage) =
    if not (Set.mem (Tezos.get_sender ()) d.managers) then failwith ERROR.rights_manager


(** Set the address set of admins
    @caller admin
*)
let set_admins (addrs, d : address set * shop_storage) =
    let _ = _admin_only d in
    [], {d with admins = addrs}

(** Set the address set of managers
    Note that the Fight and Tournament contract need to be manager to grant items
    @caller admin
*)
let set_managers (addrs, d : address set * shop_storage) =
    let _ = _admin_only d in
    [], {d with managers = addrs}

let set_shop_open (v, d : bool * shop_storage) =
    let _ = _admin_only d in
    [], {d with is_open = v}


let sink_fees (addr, d: address * shop_storage) =
    let _ = _admin_only d in
    [Tezos.transaction unit (Tezos.get_balance ()) (Tezos.get_contract addr)], d

let new_item (data, d: shop_item_data * shop_storage) = 
    let _ = _manager_only d in
    let _ = if Map.mem data.item d.items then failwith ERROR.name_taken in
    [], { d with
        items = Map.add data.item data d.items
    }
let new_bundle (data, d: shop_bundle_data * shop_storage) = 
    let _ = _manager_only d in
    let _ = if Map.mem data.bundle d.bundles then failwith ERROR.name_taken in
    [], { d with
        bundles = Map.add data.bundle data d.bundles
    }
let set_item_consumers (item, consumers, d: shop_item * (address set) * shop_storage) =
    let _ = _manager_only d in
    let data = _get_item_data (item,d) in
    let data = { data  with consumers = consumers } in 
    [], { d with
        items = Map.update item (Some data) d.items
    }
let set_item_price (item, price, d: shop_item * tez * shop_storage) =
    let _ = _manager_only d in
    let data = _get_item_data (item,d) in
    let data = { data with price = price } in 
    [], { d with
        items = Map.update item (Some data) d.items
    }
let set_bundle_price (bundle, price, d: shop_bundle * tez * shop_storage) =
    let _ = _manager_only d in
    let data = _get_bundle_data (bundle,d) in
    let data = { data with price = price } in 
    [], { d with
        bundles = Map.update bundle (Some data) d.bundles
    }
let delete_bundle (bundle, d: shop_bundle * shop_storage) = 
    let _ = _manager_only d in
    [], { d with
        bundles = Map.remove bundle d.bundles
    }
let consume_item (item,qty,addr,d: shop_item * nat * address * shop_storage) =
    let data = _get_item_data (item,d) in
    let _ = if not (Set.mem (Tezos.get_sender ()) data.consumers) then failwith ERROR.rights_other in
    let b = Big_map.find_opt addr d.owned_items in
    let b = Option.unopt_with_error b ERROR.item_not_enough in
    let c = (match (Map.find_opt item b) with
        | None -> 0n
        | Some v -> v ) in
    let _ = if c < qty then failwith ERROR.item_not_enough in
    let b = Map.update item (Some (abs ((int qty) - (int c)))) b in
    [], { d with owned_items = Big_map.update addr (Some b) d.owned_items }

let _increment_item (item, qty, addr, d: shop_item * nat * address * shop_storage) : shop_storage =
    let b = (match (Big_map.find_opt addr d.owned_items) with
        | None -> Map.literal [item,qty]
        | Some m -> (match (Map.find_opt item m) with
            | None -> Map.add item qty m
            | Some v -> Map.update item (Some (qty+v)) m)
        ) in
    { d with owned_items = Big_map.update addr (Some b) d.owned_items }

let grant_item (item,qty,addr,d: shop_item * nat * address * shop_storage) =
    let _ = if d.is_open = false then failwith ERROR.shop_closed in
    let _ = _manager_only d in
    let data = _get_item_data (item,d) in
    // Since Fight and Tournament call GrantItem, we comment the line below
    // This is however a slight security right, since any Managers can Grant infinite items
    //let _ = if Tezos.get_amount () <> (data.price * qty) then failwith ERROR.price in
    let _ = if data.quantity < qty then failwith ERROR.item_no_stock in
    let data = { data with quantity = abs ((int data.quantity) - (int qty)) } in
    let d = { d with items = Map.update item (Some data) d.items } in
    let d  = _increment_item (item,qty,addr,d) in
    [], d

let buy_item (item, qty, d: shop_item * nat * shop_storage) =
    let _ = if d.is_open = false then failwith ERROR.shop_closed in
    let data = _get_item_data (item,d) in
    let _ = if Tezos.get_amount () <> (data.price * qty) then failwith ERROR.price in
    let _ = if data.quantity < qty then failwith ERROR.item_no_stock in
    let data = { data with quantity = abs ((int data.quantity) - (int qty)) } in
    let d = { d with items = Map.update item (Some data) d.items } in
    let addr = Tezos.get_sender () in
    let d  = _increment_item (item,qty,addr,d) in
    [Tezos.emit "%boughtItem" ((item,qty,addr): event_bought_item)], d

let buy_bundle (bundle, qty, d: shop_bundle * nat * shop_storage) =
    let _ = if d.is_open = false then failwith ERROR.shop_closed in
    let data = _get_bundle_data (bundle,d) in    
    let _ = if Tezos.get_amount () <> (data.price * qty) then failwith ERROR.price in
    let _ = if data.quantity < qty then failwith ERROR.item_no_stock in
    let data = { data with quantity = abs ((int data.quantity) - (int qty)) } in
    let d = { d with bundles = Map.update bundle (Some data) d.bundles } in
    let addr = Tezos.get_sender () in
    let folded = fun (dd,(i,n): shop_storage * (shop_item * nat)) ->
        _increment_item (i, qty*n, addr, dd) in
    let d = Map.fold folded data.items d in
    [Tezos.emit "%boughtBundle" ((bundle,qty,addr): event_bought_bundle)], d


let main (action, d: shop_parameter * shop_storage) = 
    ( match action with
    | SetAdmins addrs -> set_admins(addrs,d)
    | SetManagers addrs -> set_managers(addrs,d)
    | SetShopOpen value -> set_shop_open(value,d)
    | NewItem data -> new_item(data,d)
    | SetItemPrice (item,value) -> set_item_price(item,value,d)
    | SetItemConsumers (item,consumers) -> set_item_consumers(item,consumers,d)
    | NewBundle data -> new_bundle(data,d)
    | SetBundlePrice (bundle,value) -> set_bundle_price(bundle,value,d)
    | DeleteBundle bundle -> delete_bundle(bundle,d)
    | BuyItem (item,qty) -> buy_item(item,qty,d)
    | BuyBundle (bundle,qty) -> buy_bundle(bundle,qty,d)
    | GrantItem (item,qty,addr) -> grant_item(item,qty,addr,d)
    | ConsumeItem (item,qty,addr) -> consume_item(item,qty,addr,d)
    | SinkFees addr -> sink_fees(addr,d)
    : (operation list * shop_storage))