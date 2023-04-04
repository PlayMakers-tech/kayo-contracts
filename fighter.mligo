(**
    Fighter smart contract
    This contract holds the NFT for KAYO Fighters.
    We have here ids for each fighter, linked to an object where we
    can learn about the current owner, its current state and its name.
    This is also through this contract that a fighter can be transfered,
    but only if in a state where this is allowed.
    More data on the fighters are available through the Ability and the
    Attribute contracts.
    The fighters are used in the Fight, Tournament and Marketfighter
    contracts.
    @author Maxime Niankouri - PlayMakers - 2023
    @version 1.0.0
*)
#include "fighter.schema.mligo"
#include "ability.schema.mligo"
#include "attribute.schema.mligo"
#include "error.mligo"

(** Private function to check that the caller is admin *)
let _admin_only (d: fighter_storage) =
    if Tezos.get_sender () <> d.admin then failwith ERROR.rights_admin

(** Private function to get fighter data out of its id *)
let _get_fighter_data (id, d: fighter_id * fighter_storage) =
    Option.unopt_with_error (Big_map.find_opt id d.fighters) ERROR.fighter_id

(** Private function to get all fighters owned by an address *)
let _get_fighters_by_owner (owner, d: address * fighter_storage) : fighter_id set =
    match (Big_map.find_opt owner d.fighters_by_owner) with
    | Some x -> x
    | None -> Set.empty

(** Checking that a fighter is in a transferrable state *)
let beforeTransfer (f: fighter_data) =
    if      f.listed=true       then failwith ERROR.listed
    else if f.queue<>NotQueuing then failwith ERROR.queued
    else if f.fight<>0n         then failwith ERROR.fighting
    else if f.tournament<>0n    then failwith ERROR.tournamenting
    else if f.inactive=true     then failwith ERROR.inactive
    else if f.minting=true      then failwith ERROR.minting

(** Initializing a new fighter object with default values *)
let new_fighter (id, owner: fighter_id * address) =
    ({
        id = id;
        owner = owner;
        listed = false;
        inactive = false;
        minting = true;
        fight = 0n;
        tournament = 0n;
        queue = NotQueuing;
        father = 0n;
        mother = 0n;
        name = ""

    } : fighter_data)

(** Mint entrypoint
    Note that this only requests a mint, and reserve its id
    @caller any
    @amount mint_fee
    @event minting id
*)
let mint (d : fighter_storage) =
    let _ = if Tezos.get_amount () <> d.mint_fee then failwith ERROR.fee in
    let owner = Tezos.get_sender () in
    [Tezos.emit "%minting" d.next_id],
    { d with
        next_id = d.next_id + 1n;
        fighters = Big_map.add d.next_id (new_fighter (d.next_id, owner)) d.fighters;
        mints = Set.add d.next_id d.mints
    }

(** RealMint entrypoint
    After a Mint or a Fusion, this actually sets all the values to complete the process
    @caller minter
    @call  Attribute Mint|Fusion
    @call  Ability   Mint|Fusion
    @event minting id
*)
let real_mint (id, attr, abil, d: fighter_id * bytes * (ability_id list) * fighter_storage) =
    let _ = _admin_only d in
    let f = _get_fighter_data (id,d) in
    if f.minting=false then failwith ERROR.minted else
    let f = { f with minting = false } in
    let set = Set.add id (_get_fighters_by_owner (f.owner, d)) in
    let fbo = Big_map.update f.owner (Some set) d.fighters_by_owner in
    let ep_attr : attribute_parameter =
        if f.father=0n
        then Mint (id, attr)
        else Fusion (id, f.father, f.mother, attr) in
    let ep_abil : ability_parameter =
        if f.father=0n
        then Mint (id, abil)
        else Fusion (id, f.father, f.mother, abil) in
    [Tezos.transaction ep_attr 0tez (Tezos.get_contract d.attribute_addr);
     Tezos.transaction ep_abil 0tez (Tezos.get_contract d.ability_addr);
     Tezos.emit "%minted" id],
    { d with
        fighters = Big_map.update id (Some f) d.fighters;
        fighters_by_owner = fbo;
        mints = Set.remove id d.mints
    }

(** Set the mint fee to be paid by the user for a Mint
    @caller admin
*)
let set_mint_fee (v, d : tez * fighter_storage) =
    let _ = _admin_only d in
    [], {d with mint_fee = v}

(** Set the mint fee to be paid by the user for a Fusion
    @caller admin
*)
let set_fusion_fee (v, d : tez * fighter_storage) =
    let _ = _admin_only d in
    [], {d with fusion_fee = v}

(** Set the address of the Fight smart contract
    @caller admin
*)
let set_fight_addr (addr, d : address * fighter_storage) =
    let _ = _admin_only d in
    [], {d with fight_addr = addr}

(** Set the address of the Tournament smart contract
    @caller admin
*)
let set_tournament_addr (addr, d : address * fighter_storage) =
    let _ = _admin_only d in
    [], {d with tournament_addr = addr}

(** Set the address of the Attribute smart contract
    @caller admin
*)
let set_attribute_addr (addr, d : address * fighter_storage) =
    let _ = _admin_only d in
    [], {d with attribute_addr = addr}

(** Set the address of the Ability smart contract
    @caller admin
*)
let set_ability_addr (addr, d : address * fighter_storage) =
    let _ = _admin_only d in
    [], {d with ability_addr = addr}

(** Set the address of the Marketfighter smart contract
    @caller admin
*)
let set_marketfighter_addr (addr, d : address * fighter_storage) =
    let _ = _admin_only d in
    [], {d with marketfighter_addr = addr}

(** Set the combat state of a fighter
    @caller admin|Tournament|Fight
*)
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

(** Fusion entrypoint
    Note that this only requests a fusion, reserve its id, and disable the parents
    TODO The fusion needs to be reworked (maybe)
    @caller owner
    @amount fusion_fee
    @event minting id
*)
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
    let set = _get_fighters_by_owner (owner, d) in
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
    [Tezos.emit "%minting" d.next_id],
    { d with
        next_id = d.next_id + 1n;
        fighters = fmap;
        fighters_by_owner = fbo;
        mints = Set.add d.next_id d.mints
    }

(** Set the market state of a fighter
    @caller admin|Marketfighter
*)
let set_fighter_listed (id, state, d: fighter_id * bool * fighter_storage) =
    let f  = _get_fighter_data (id, d) in
    if not (Set.mem (Tezos.get_sender ()) (Set.literal [d.admin;d.marketfighter_addr]))
    then failwith ERROR.rights_other
    else [], { d with
            fighters = Big_map.update id 
                (Some { f with 
                    listed = state
                }) d.fighters
        }

(** Transfer entrypoint
    Transfer the fighter from one owner to the next.
    This can be done on a whim by its owner, through a fight/tournament reward,
    or after a auction/trade from the market.
    @caller owner|admin|Fight|Tournament|Marketfighter
    @event transfer (id, old_owner, new_owner)
*)
let transfer (id, addr, d: fighter_id * address * fighter_storage) =
    let f  = _get_fighter_data (id, d) in
    let _ = if not (Set.mem (Tezos.get_sender ()) (Set.literal [f.owner;d.admin;d.fight_addr;d.tournament_addr;d.marketfighter_addr]))
    then failwith ERROR.rights_owner in
    let _ = beforeTransfer f in 
    let set = Set.add id (_get_fighters_by_owner (addr, d)) in
    let fbo = Big_map.update addr (Some set) d.fighters_by_owner in
    let old = Set.remove id (_get_fighters_by_owner (f.owner, d)) in
    let fbo = Big_map.update f.owner (if (Set.cardinal old) = 0n then None else Some old) fbo in
    [Tezos.emit "%transfer" (id, f.owner, addr)],
    { d with
        fighters = Big_map.update id (Some {f with owner = addr}) d.fighters;
        fighters_by_owner = fbo;
    }

(** SetName entrypoint
    Allow the owner to rename their fighter with a 32 characters limit.
    @caller owner
*)
let set_name (id, name, d: fighter_id * string * fighter_storage) =
    let _ = if String.length name > 32n then failwith ERROR.name_too_long in
    let f  = _get_fighter_data (id, d) in
    let _ = if Tezos.get_sender () <> f.owner then failwith ERROR.rights_owner in
    let _ = beforeTransfer f in 
    [], { d with
        fighters = Big_map.update id (Some {f with name = name}) d.fighters
    }

(** SinkFees entrypoint
    Allow the admin to retrieve the funds stored on the contract
    @caller admin
*)
let sink_fees (addr, d: address * fighter_storage) =
    let _ = _admin_only d in
    [Tezos.transaction unit (Tezos.get_balance ()) (Tezos.get_contract addr)], d


(** Main function of the smart contract *)
let main (action, d: fighter_parameter * fighter_storage) = 
    ( match action with
    | Mint -> mint(d)
    | RealMint (id,attr,abil) -> real_mint(id,attr,abil,d)
    | SetMintFee value -> set_mint_fee(value,d)
    | SetFusionFee value -> set_fusion_fee(value,d)
    | SetFightAddr addr -> set_fight_addr(addr,d)
    | SetTournamentAddr addr -> set_tournament_addr(addr,d)
    | SetAttributeAddr addr -> set_attribute_addr(addr,d)
    | SetAbilityAddr addr -> set_ability_addr(addr,d)
    | SetMarketfighterAddr addr -> set_marketfighter_addr(addr,d)
    | Fusion (father,mother) -> fusion(father,mother,d)
    | SetFighterListed (id,state) -> set_fighter_listed(id,state,d)
    | Transfer (id,addr) -> transfer(id,addr,d)
    | SetName (id,name) -> set_name(id,name,d)
    | SetFighterState (id,fight,tournament,queue) -> set_fighter_state(id,fight,tournament,queue,d)
    | SinkFees addr -> sink_fees(addr,d)
    : (operation list * fighter_storage))



[@view] let get_fighter_data = _get_fighter_data
[@view] let get_fighters_by_owner = _get_fighters_by_owner
[@view] let get_fees (_,d: unit * fighter_storage) = {
    mint = d.mint_fee;
    fusion = d.fusion_fee
}
[@view] let get_addr (_,d: unit * fighter_storage) = {
    fight = d.fight_addr;
    tournament = d.tournament_addr;
    attribute = d.attribute_addr;
    ability = d.ability_addr;
    marketfighter = d.marketfighter_addr
}