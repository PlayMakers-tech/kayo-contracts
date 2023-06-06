(**
    Marketfighter smart contract
    This contract offers a secondary market for Fighters.
    This works like stocks, where the owner of a fighter can set any
    sell price, while anyone else can set a buy price.
    If the buy price is greater than the sell price, the transaction
    occurs.
    @author Maxime Niankouri - PlayMakers - 2023
    @version 1.0.0
*)
#include "marketfighter.schema.mligo"
#include "marketfighter.event.mligo"
#include "../error.mligo"

(** Private function to check that the caller is admin *)
let _admin_only (d: marketfighter_storage) =
    if not (Set.mem (Tezos.get_sender ()) d.admins) then failwith ERROR.rights_admin

(** Private function to check that the caller is manager *)
let _manager_only (d: marketfighter_storage) =
    if not (Set.mem (Tezos.get_sender ()) d.managers) then failwith ERROR.rights_manager

(** Private function to get fighter data out of its id from Fighter contract *)
let _get_fighter_data (a, d: fighter_id * marketfighter_storage) =
    (Option.unopt_with_error (Tezos.call_view "get_fighter_data" a d.fighter_addr) ERROR.fighter_id
    : fighter_data)

(** Checking that a fighter is in a listable/sellable state *)
let beforeListing (f: fighter_data) =
    if      Option.is_some f.queue then failwith ERROR.queued
    else if f.fight<>0n            then failwith ERROR.fighting
    else if f.tournament<>0n       then failwith ERROR.tournamenting
    else if f.inactive=true        then failwith ERROR.inactive
    else if f.minting=true         then failwith ERROR.minting

(** Do a transaction at a given price
    @call Fighter Transfer
    @call Fighter SetFighterListed
    @event sold (fighter_id, tez)
*)
let transaction (id, price, seller, buyer, op, d : fighter_id * tez * address * address * operation list * marketfighter_storage) =
    let (buy_map,lo) = (match (Big_map.find_opt id d.buys) with
        | None -> (None, d.listed_offer)
        | Some map -> let map = Map.remove buyer map in
            if Map.size map = 0n then (None, Set.remove id d.listed_offer) else (Some map, d.listed_offer)
    ) in
    (Tezos.transaction (SetFighterListed (id,false)) 0tez (Tezos.get_contract d.fighter_addr))::
    (Tezos.transaction (Transfer (id,buyer)) 0tez (Tezos.get_contract d.fighter_addr))::
    (Tezos.transaction unit price (Tezos.get_contract seller))::
    (Tezos.emit "%sold" ((id,price): event_sold))::
    op,
    { d with
        listed_sale = Set.remove id d.listed_sale;
        listed_offer = lo;
        sells = Big_map.remove id d.sells;
        buys = Big_map.update id buy_map d.buys
    }


(** Set the address set of admins
    @caller admin
*)
let set_admins (addrs, d : address set * marketfighter_storage) =
    let _ = _admin_only d in
    [], {d with admins = addrs}

(** Set the address set of managers
    @caller admin
*)
let set_managers (addrs, d : address set * marketfighter_storage) =
    let _ = _admin_only d in
    [], {d with managers = addrs}

(** Sell entrypoint
    Set an offer for sale, effectively listing a fighter (potentially selling it right away)
    @caller owner
    @call Fighter SetFighterListed
    @event selling (fighter_id, tez)
*)
let sell (id, price, d : fighter_id * tez * marketfighter_storage) =
    let _ = if Tezos.get_amount () <> d.listing_fee then failwith ERROR.fee in
    let _ = if price < d.min_price then failwith ERROR.min_price in
    let f = _get_fighter_data (id,d) in
    let _ = beforeListing f in
    let _ = if Tezos.get_sender () <> f.owner then failwith ERROR.rights_owner in
    (* If there is already an offer, we check if we can trade *)
    let (buyer, buy_price) : address * tez =
        if Set.mem id d.listed_offer then
            (match (Big_map.find_opt id d.buys) with
                | None -> (d.fighter_addr, 0tez)
                | Some buy_map ->        
                    let folded = fun ((buyer,best), (addr,data): ( address * tez ) * (address * marketfighter_data))
                        -> if data.price > best then (addr,data.price) else (buyer,best) in
                     Map.fold folded buy_map (d.fighter_addr, 0tez)
                )
        else (d.fighter_addr, 0tez)
    in 
    if buy_price < price then
        let data : marketfighter_data = {
            date = Tezos.get_now ();
            price = price
        } in
        [Tezos.transaction (SetFighterListed (id,true)) 0tez (Tezos.get_contract d.fighter_addr);
         Tezos.emit "%selling" ((id,price): event_selling)],
        { d with
            listed_sale = Set.add id d.listed_sale;
            sells = Big_map.update id (Some data) d.sells
        }
    else transaction (id, price, f.owner, buyer, [], d)

(** Buy entrypoint
    Bid on a fighter (or directly buy it if the bid is higher than the current sale)
    @caller !owner
    @amount price
    @event buying fighter_id
*)
let buy (id, price, d : fighter_id * tez * marketfighter_storage) =
    let _ = if Tezos.get_amount () <> price then failwith ERROR.price in
    let _ = if price < d.min_price then failwith ERROR.min_price in
    let f = _get_fighter_data (id,d) in
    let _ = if f.inactive then failwith ERROR.inactive in
    let buyer = Tezos.get_sender () in
    let _ = if buyer = f.owner then failwith ERROR.already_owned in
    let (sold, sell_price) : bool * tez = (match (Big_map.find_opt id d.sells) with
        | None -> (false, 0tez)
        | Some data -> ((data.price <= price), data.price)
    ) in    
    let data : marketfighter_data = {
        date = Tezos.get_now ();
        price = price
    } in
    let (buy_map,prev) = (match (Big_map.find_opt id d.buys) with
        | None -> (Map.literal [(buyer,data)], None)
        | Some map -> (Map.update buyer (Some data) map, Map.find_opt buyer map)
    ) in
    let op = (match prev with
        | None -> []
        | Some md -> [Tezos.transaction unit md.price (Tezos.get_contract buyer)]
    ) in
    if sold
    then transaction(id, sell_price, f.owner, buyer, op, d)
    else
        (Tezos.emit "%buying" (id: event_buying))::op,
        { d with
            listed_offer = Set.add id d.listed_offer;
            buys = Big_map.update id (Some buy_map) d.buys
        }

(** Private function for Cancel and CancelFor *)
let _cancel_buying (id, addr, fail_on_error, d: fighter_id * address * bool * marketfighter_storage) =
    if (not Set.mem id d.listed_offer) then
        if fail_on_error then failwith ERROR.not_listed else [], d
    else
        let (buy_map, data, error) : ((address, marketfighter_data) map * marketfighter_data option * bool) =
            (match (Big_map.find_opt id d.buys) with
                | None -> (Map.empty,None,true)
                | Some map -> (Map.update addr None map, Map.find_opt addr map, false)
            ) in
        if error then
            if fail_on_error then failwith ERROR.not_listed else [], d
        else
            let (listed, buy_map) = if Map.size buy_map = 0n
            then (Set.remove id d.listed_offer, None)
            else (d.listed_offer, Some buy_map) in
            let op = [Tezos.emit "%cancelBuying" (id: event_cancel_buying)] in
            let op = (match data with
                | None -> op
                | Some md -> (Tezos.transaction unit md.price (Tezos.get_contract addr))::op
            ) in
            op,
            { d with
                listed_offer = listed;
                buys = Big_map.update id buy_map d.buys
            }

(** Cancel entrypoint
    Cancel a Buy or Sell call.
    TODO Add timing constraints on buyer cancelling ?
    @caller any
    @event cancelSelling fighter_id
    @event cancelBuying fighter_id
*)
let cancel (id, d : fighter_id * marketfighter_storage) =
    let f = _get_fighter_data (id,d) in
    let sender = Tezos.get_sender () in
    if sender = f.owner then
        let _ = if (not Set.mem id d.listed_sale) then failwith ERROR.not_listed in
        [Tezos.transaction (SetFighterListed (id,false)) 0tez (Tezos.get_contract d.fighter_addr);
         Tezos.emit "%cancelSelling" (id: event_cancel_selling)],
        { d with
            listed_sale = Set.remove id d.listed_sale;
            sells = Big_map.update id None d.sells
        }
    else
        _cancel_buying (id, sender, true, d)

(** CancelFor entrypoint
    Cancel a Buy for someone else.
    @caller manager
    @event cancelBuying fighter_id
*)
let cancel_for (id, addr, d : fighter_id * address * marketfighter_storage) =
    let _ = _manager_only d in
    _cancel_buying (id, addr, false, d)

let set_market_open (v, d : bool * marketfighter_storage) =
    let _ = _admin_only d in
    [], {d with is_open = v}
let set_listing_fee (v, d : tez * marketfighter_storage) =
    let _ = _manager_only d in
    [], {d with listing_fee = v}
let set_min_price (v, d : tez * marketfighter_storage) =
    let _ = _manager_only d in
    [], {d with min_price = v}
let set_fighter_addr (addr, d : address * marketfighter_storage) =
    let _ = _admin_only d in
    [], {d with fighter_addr = addr}

(** TODO Should only take out the fees, not the full balance with held buy money *)
let sink_fees (addr, d: address * marketfighter_storage) =
    let _ = _admin_only d in
    [Tezos.transaction unit (Tezos.get_balance ()) (Tezos.get_contract addr)], d



(** Main function of the smart contract *)
let main (action, d: marketfighter_parameter * marketfighter_storage) = 
    ( match action with
    | SetAdmins addrs -> set_admins(addrs,d)
    | SetManagers addrs -> set_managers(addrs,d)
    | SetMarketOpen value -> set_market_open(value,d)
    | SetListingFee value -> set_listing_fee(value,d)
    | SetMinPrice value -> set_min_price(value,d)
    | SetFighterAddr addr -> set_fighter_addr(addr,d)
    | SinkFees addr -> sink_fees(addr,d)
    | Sell (id,price) -> sell(id,price,d)
    | Buy  (id,price) -> buy(id,price,d)
    | Cancel id -> cancel(id,d)
    | CancelFor (id,addr) -> cancel_for(id,addr,d)
    : (operation list * marketfighter_storage))


[@view] let get_fees (_,d: unit * marketfighter_storage) = {
    listing = d.listing_fee
}