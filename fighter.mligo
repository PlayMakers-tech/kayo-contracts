#include "fighter.schema.mligo"
#include "ability.schema.mligo"
#include "attribute.schema.mligo"
#include "error.mligo"

let _admin_only (d: fighter_storage) =
    if Tezos.get_sender () <> d.admin then failwith ERROR.rights_admin
let _get_fighter_data (id, d: fighter_id * fighter_storage) =
    Option.unopt_with_error (Big_map.find_opt id d.fighters) "Invalid fighter_id"
let _get_fighters_by_owner (owner, d: address * fighter_storage) : fighter_id set =
    match (Big_map.find_opt owner d.fighters_by_owner) with
    | Some x -> x
    | None -> Set.empty


let beforeTransfer (f: fighter_data) =
    if      f.listed=true       then failwith ERROR.listed
    else if f.queue<>NotQueuing then failwith ERROR.queued
    else if f.fight<>0n         then failwith ERROR.fighting
    else if f.tournament<>0n    then failwith ERROR.tournamenting
    else if f.inactive=true     then failwith ERROR.inactive

let new_fighter (id, owner: fighter_id * address) =
    ({
        id = id;
        owner = owner;
        listed_price = 0tez;
        listed = false;
        inactive = false;
        fight = 0n;
        tournament = 0n;
        queue = NotQueuing;
        father = 0n;
        mother = 0n

    } : fighter_data)

let mint (d : fighter_storage) =
    if Tezos.get_amount () <> d.mint_fee then failwith ERROR.fee
    else
    let owner = Tezos.get_sender () in
    let set = Set.add d.next_id (_get_fighters_by_owner (owner, d)) in
    let fbo = Big_map.update owner (Some set) d.fighters_by_owner in
    [Tezos.transaction ((Mint d.next_id):attribute_parameter) 0tez (Tezos.get_contract d.attribute_addr);
     Tezos.transaction ((Mint d.next_id):ability_parameter) 0tez (Tezos.get_contract d.ability_addr);
     Tezos.emit "%minted" d.next_id],
    { d with
        next_id = d.next_id + 1n;
        fighters = Big_map.add d.next_id (new_fighter (d.next_id, owner)) d.fighters;
        fighters_by_owner = fbo;
    }

let set_mint_fee (v, d : tez * fighter_storage) =
    let _ = _admin_only d in
    [], {d with mint_fee = v}
let set_fusion_fee (v, d : tez * fighter_storage) =
    let _ = _admin_only d in
    [], {d with fusion_fee = v}
let set_list_fee (v, d : tez * fighter_storage) =
    let _ = _admin_only d in
    [], {d with list_fee = v}
let set_fight_addr (addr, d : address * fighter_storage) =
    let _ = _admin_only d in
    [], {d with fight_addr = addr}
let set_tournament_addr (addr, d : address * fighter_storage) =
    let _ = _admin_only d in
    [], {d with tournament_addr = addr}
let set_attribute_addr (addr, d : address * fighter_storage) =
    let _ = _admin_only d in
    [], {d with attribute_addr = addr}
let set_ability_addr (addr, d : address * fighter_storage) =
    let _ = _admin_only d in
    [], {d with ability_addr = addr}

let set_fighter_state (id,fight,tournament,queue,d:
        fighter_id * fight_id * tournament_id * fight_queue * fighter_storage) =
    let f  = _get_fighter_data (id, d) in
    if not (Set.mem (Tezos.get_sender ()) (Set.literal [d.admin;d.fight_addr;d.tournament_addr]))
    then failwith ERROR.rights_other
    else [], { d with
            fighters = Big_map.update id 
                (Some { f with 
                    fight = fight;
                    tournament = tournament;
                    queue = queue
                }) d.fighters
        }

// TODO The fusion needs to be reworked (maybe)
let fusion (father, mother, d: fighter_id * fighter_id * fighter_storage) =
    let owner = Tezos.get_sender () in
    let f = _get_fighter_data (father,d) in
    let m = _get_fighter_data (mother,d) in
    if Tezos.get_amount () <> d.fusion_fee then failwith ERROR.fee else
    if father = mother then failwith ERROR.invalid_fighter else
    if owner <> f.owner then failwith ERROR.rights_owner else
    if owner <> m.owner then failwith ERROR.rights_owner else
    let _ = beforeTransfer f in 
    let _ = beforeTransfer m in 
    let set = Set.add d.next_id (_get_fighters_by_owner (owner, d)) in
    let set = Set.remove father set in
    let set = Set.remove mother set in
    let fbo = Big_map.update owner (Some set) d.fighters_by_owner in
    let n = new_fighter (d.next_id, owner) in
    let n = { n with father = father; mother = mother } in
    let f = { f with inactive = true } in
    let m = { m with inactive = true } in
    let fmap = Big_map.add d.next_id n d.fighters in
    let fmap = Big_map.update father (Some f) fmap in
    let fmap = Big_map.update mother (Some m) fmap in
    [Tezos.transaction ((Fusion (d.next_id, father, mother)):attribute_parameter) 0tez (Tezos.get_contract d.attribute_addr);
     Tezos.transaction ((Fusion (d.next_id, father, mother)):ability_parameter) 0tez (Tezos.get_contract d.ability_addr);
     Tezos.emit "%minted" d.next_id],
    { d with
        next_id = d.next_id + 1n;
        fighters = fmap;
        fighters_by_owner = fbo;
    }



let list_fighter (_id, _price, _d: fighter_id * tez * fighter_storage) =
    failwith "Not implemented yet"
let cancel_list (_id, _d: fighter_id * fighter_storage) =
    failwith "Not implemented yet"
let buy (_id, _d: fighter_id * fighter_storage) =
    failwith "Not implemented yet"

let transfer (id, addr, d: fighter_id * address * fighter_storage) =
    let f  = _get_fighter_data (id, d) in
    if not (Set.mem (Tezos.get_sender ()) (Set.literal [f.owner;d.admin;d.fight_addr;d.tournament_addr]))
    then failwith ERROR.rights_owner
    else
    let _ = beforeTransfer f in 
    let set = Set.add id (_get_fighters_by_owner (f.owner, d)) in
    let fbo = Big_map.update addr (Some set) d.fighters_by_owner in
    let old = Set.remove id (_get_fighters_by_owner (f.owner, d)) in
    let fbo = Big_map.update f.owner (if (Set.cardinal old) = 0n then None else Some old) fbo in
    [Tezos.emit "%transfer" (id, f.owner, addr)],
    { d with
        fighters = Big_map.update id (Some {f with owner = addr}) d.fighters;
        fighters_by_owner = fbo;
    }

let sink_fees (addr, d: address * fighter_storage) =
    let _ = _admin_only d in
    [Tezos.transaction unit (Tezos.get_balance ()) (Tezos.get_contract addr)], d



let main (action, d: fighter_parameter * fighter_storage) = 
    ( match action with
    | Mint -> mint(d)
    | SetMintFee value -> set_mint_fee(value,d)
    | SetFusionFee value -> set_fusion_fee(value,d)
    | SetListFee value -> set_list_fee(value,d)
    | SetFightAddr addr -> set_fight_addr(addr,d)
    | SetTournamentAddr addr -> set_tournament_addr(addr,d)
    | SetAttributeAddr addr -> set_attribute_addr(addr,d)
    | SetAbilityAddr addr -> set_ability_addr(addr,d)
    | Fusion (father,mother) -> fusion(father,mother,d)
    | ListFighter (id,price) -> list_fighter(id,price,d)
    | CancelList id -> cancel_list(id,d)
    | Buy id -> buy(id,d)
    | Transfer (id,addr) -> transfer(id,addr,d)
    | SetFighterState (id,fight,tournament,queue) -> set_fighter_state(id,fight,tournament,queue,d)
    | SinkFees addr -> sink_fees(addr,d)
    : (operation list * fighter_storage))



[@view] let get_fighter_data = _get_fighter_data
[@view] let get_fighters_by_owner = _get_fighters_by_owner
[@view] let get_fees (_,d: unit * fighter_storage) = {
    mint = d.mint_fee;
    fusion = d.fusion_fee;
    listing = d.list_fee
}
[@view] let get_addr (_,d: unit * fighter_storage) = {
    fight = d.fight_addr;
    tournament = d.tournament_addr;
    attribute = d.attribute_addr;
    ability = d.ability_addr
}