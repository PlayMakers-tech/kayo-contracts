#include "marketfighter.schema.mligo"
#include "error.mligo"

let _admin_only (d: marketfighter_storage) =
    if Tezos.get_sender () <> d.admin then failwith ERROR.rights_admin
let _get_fighter_data (a, d: fighter_id * marketfighter_storage) =
    (Option.unopt_with_error (Tezos.call_view "get_fighter_data" a d.fighter_addr) "Invalid fighter_id"
    : fighter_data)

let beforeListing (f: fighter_data) =
    if      f.queue<>NotQueuing then failwith ERROR.queued
    else if f.fight<>0n         then failwith ERROR.fighting
    else if f.tournament<>0n    then failwith ERROR.tournamenting
    else if f.inactive=true     then failwith ERROR.inactive
    else if f.minting=true      then failwith ERROR.minting

let transaction (id, price, seller, buyer, d : fighter_id * tez * address * address * marketfighter_storage) =
    let buy_map = (match (Big_map.find_opt id d.buys) with
        | None -> None
        | Some map -> let map = Map.remove buyer map in
            if Map.size map = 0n then None else Some map 
    ) in
    [Tezos.transaction (SetFighterListed (id,false)) 0tez (Tezos.get_contract d.fighter_addr);
     Tezos.transaction (Transfer (id,buyer)) 0tez (Tezos.get_contract d.fighter_addr);
     Tezos.transaction unit price (Tezos.get_contract seller);
     Tezos.emit "%sold" (id,price)],
    { d with
        listed_sale = Set.remove id d.listed_sale;
        sells = Big_map.remove id d.sells;
        buys = Big_map.update id buy_map d.buys
    }

let sell (id, price, d : fighter_id * tez * marketfighter_storage) =
    if Tezos.get_amount () <> d.listing_fee then failwith ERROR.fee
    else if price < d.min_price then failwith ERROR.min_price
    else let f = _get_fighter_data (id,d) in
    let _ = beforeListing f in
    if Tezos.get_sender () <> f.owner then failwith ERROR.rights_owner
    else
    (* If there is already an offer, we check if we can trade *)
    let (buyer, buy_price) : address * tez =
        if Set.mem id d.listed_offer then
            (match (Big_map.find_opt id d.buys) with
                | None -> (d.admin, 0tez)
                | Some buy_map ->        
                    let folded = fun ((buyer,best), (addr,data): ( address * tez ) * (address * marketfighter_data))
                        -> if data.price > best then (addr,data.price) else (buyer,best) in
                     Map.fold folded buy_map (d.admin, 0tez)
                )
        else (d.admin, 0tez)
    in 
    if buy_price < price then
        let data : marketfighter_data = {
            date = Tezos.get_now ();
            price = price
        } in
        [Tezos.transaction (SetFighterListed (id,true)) 0tez (Tezos.get_contract d.fighter_addr);
         Tezos.emit "%selling" (id,price)],
        { d with
            listed_sale = Set.add id d.listed_sale;
            sells = Big_map.update id (Some data) d.sells
        }
    else transaction (id, price, f.owner, buyer, d)

(* TODO case where the buyer change price without canceling *)
(* TODO case where the fighter is transferred to the buyer via another mean*)
let buy (id, price, d : fighter_id * tez * marketfighter_storage) =
    if Tezos.get_amount () <> price then failwith ERROR.price
    else if price < d.min_price then failwith ERROR.min_price
    else let f = _get_fighter_data (id,d) in
    let buyer = Tezos.get_sender () in
    if buyer = f.owner then failwith ERROR.already_owned
    else
    let (sold, sell_price) : bool * tez = (match (Big_map.find_opt id d.sells) with
        | None -> (false, 0tez)
        | Some data -> ((data.price <= price), data.price)
    ) in
    if sold
    then transaction(id, sell_price, f.owner, buyer, d)
    else
        let data : marketfighter_data = {
            date = Tezos.get_now ();
            price = price
        } in
        let (buy_map,prev) = (match (Big_map.find_opt id d.buys) with
            | None -> (Map.literal [(buyer,data)], None)
            | Some map -> (Map.update buyer (Some data) map, Map.find_opt buyer map)
        ) in
        let op = [Tezos.emit "%buying" id] in
        let op = (match prev with
            | None -> op
            | Some md -> (Tezos.transaction unit md.price (Tezos.get_contract buyer))::op
        ) in
        op,
        { d with
            listed_offer = Set.add id d.listed_offer;
            buys = Big_map.update id (Some buy_map) d.buys
        }

(* TODO Add timing constraints on buyer cancelling *)
let cancel (id, d : fighter_id * marketfighter_storage) =
    let f = _get_fighter_data (id,d) in
    let sender = Tezos.get_sender () in
    if sender = f.owner then
        if Set.mem id d.listed_sale then failwith ERROR.not_listed
        else
        [Tezos.emit "%cancel_selling" id],
        { d with
            listed_sale = Set.remove id d.listed_sale;
            sells = Big_map.update id None d.sells
        }
    else
        if Set.mem id d.listed_offer then failwith ERROR.not_listed
        else
        let (buy_map,data) = (match (Big_map.find_opt id d.buys) with
            | None -> failwith ERROR.not_listed
            | Some map -> (Map.update sender None map, Map.find_opt sender map)
        ) in
        let (listed, buy_map) = if Map.size buy_map = 0n
        then (Set.remove id d.listed_offer, None)
        else (d.listed_offer, Some buy_map) in
        let op = [Tezos.emit "%cancel_buying" id] in
        let op = (match data with
            | None -> op
            | Some md -> (Tezos.transaction unit md.price (Tezos.get_contract sender))::op
        ) in
        op,
        { d with
            listed_offer = listed; 
            buys = Big_map.update id buy_map d.buys
        }

let set_market_open (v, d : bool * marketfighter_storage) =
    let _ = _admin_only d in
    [], {d with is_open = v}
let set_listing_fee (v, d : tez * marketfighter_storage) =
    let _ = _admin_only d in
    [], {d with listing_fee = v}
let set_min_price (v, d : tez * marketfighter_storage) =
    let _ = _admin_only d in
    [], {d with min_price = v}
let set_fighter_addr (addr, d : address * marketfighter_storage) =
    let _ = _admin_only d in
    [], {d with fighter_addr = addr}


let sink_fees (addr, d: address * marketfighter_storage) =
    let _ = _admin_only d in
    [Tezos.transaction unit (Tezos.get_balance ()) (Tezos.get_contract addr)], d



let main (action, d: marketfighter_parameter * marketfighter_storage) = 
    ( match action with
    | SetMarketOpen value -> set_market_open(value,d)
    | SetListingFee value -> set_listing_fee(value,d)
    | SetMinPrice value -> set_min_price(value,d)
    | SetFighterAddr addr -> set_fighter_addr(addr,d)
    | SinkFees addr -> sink_fees(addr,d)
    | Sell (id,price) -> sell(id,price,d)
    | Buy  (id,price) -> buy(id,price,d)
    | Cancel id -> cancel(id,d)
    : (operation list * marketfighter_storage))


[@view] let get_fees (_,d: unit * marketfighter_storage) = {
    listing = d.listing_fee
}
[@view] let get_addr (_,d: unit * marketfighter_storage) = {
    fighter = d.fighter_addr
}