(**
    Fight smart contract
    This contract manages battles between Fighters.
    We have here ids for each fight, linked to an object where we
    can learn about the two fighters involved, its current state,
    the data of its rounds and its other metadata.
    This is also through this contract that a fight can be initiated,
    can advance in its state, and where an owner can add a Fighter to
    a queue.
    The fights use the Fighter contract
    The fights are used in the Tournament contract.
    @author Maxime Niankouri - PlayMakers - 2023
    @version 1.0.0
*)
#include "fight.schema.mligo"
#include "fight.event.mligo"
#include "../error.mligo"


(** Private function to check that the caller is admin *)
let _admin_only (d: fight_storage) =
    if not (Set.mem (Tezos.get_sender ()) d.admins) then failwith ERROR.rights_admin

(** Private function to check that the caller is manager *)
let _manager_only (d: fight_storage) =
    if not (Set.mem (Tezos.get_sender ()) d.managers) then failwith ERROR.rights_manager

(** Private function to check that the caller is matcher *)
let _matcher_only (d: fight_storage) =
    if not (Set.mem (Tezos.get_sender ()) d.matchers) then failwith ERROR.rights_matcher

(** Private function to check that the caller is resolver *)
let _resolver_only (d: fight_storage) =
    if not (Set.mem (Tezos.get_sender ()) d.resolvers) then failwith ERROR.rights_resolver

(** Private function to get fighter data out of its id from Fighter contract *)
let _get_fighter_data (a,d: fighter_id * fight_storage) =
    (Option.unopt_with_error (Tezos.call_view "get_fighter_data" a d.fighter_addr) ERROR.fighter_id
    : fighter_data)

(** Private function to get fight data out of its id *)
let _get_fight_data (id, d: fight_id * fight_storage) =
    Option.unopt_with_error (Big_map.find_opt id d.fights) ERROR.fight_id

(** Private function to get list of fighters in a queue *)
let _get_fighters_in_queue (q, d: fight_queue * fight_storage): fighter_id set =
    match (Big_map.find_opt q d.queues) with
    | Some x -> x
    | None -> Set.empty

(** Private function to get the history of fights a fighter was involved in *)
let _get_fights_by_fighter (id, d: fighter_id * fight_storage): fight_id set =
    match (Big_map.find_opt id d.fights_by_fighter) with
    | Some x -> x
    | None -> Set.empty


(** Resolving a fight
    Checks the fight results and rewards fighters/owners accordingly
    Also sets back the fighters in idle state
    @call Fighter SetFighterState
    @call Fighter Transfer
    @call Attribute EarnXP
    @call Tournament ReportFight
*)
let _resolve_fight (id, event, d: fight_id * operation * fight_storage) =
	let f = _get_fight_data (id,d) in
    let fa = _get_fighter_data (f.a,d) in
    let fb = _get_fighter_data (f.b,d) in
    let f = { f with state = Finished } in
    let op = [
    	Tezos.transaction (SetFighterState (f.a,0n,fa.tournament,None)) 0tez (Tezos.get_contract d.fighter_addr);
    	Tezos.transaction (SetFighterState (f.b,0n,fb.tournament,None)) 0tez (Tezos.get_contract d.fighter_addr)
    ] in
    // Grant the reward
    let (_league, _stake, reward) = f.queue in
    let op = (match reward with
    | TezReward v -> 
    	if f.result > 0 then (Tezos.transaction unit v (Tezos.get_contract fa.owner))::op
    	else if f.result < 0 then (Tezos.transaction unit v (Tezos.get_contract fb.owner))::op
    	else (Tezos.transaction unit v (Tezos.get_contract fa.owner))::
    		 (Tezos.transaction unit v (Tezos.get_contract fb.owner))::op
    | FighterReward ->
        if f.result > 0 then (Tezos.transaction (Transfer (f.b,fa.owner)) 0tez (Tezos.get_contract d.fighter_addr))::op
        else if f.result < 0 then (Tezos.transaction (Transfer (f.a,fb.owner)) 0tez (Tezos.get_contract d.fighter_addr))::op
        else op
    | ItemReward (item, qty) -> 
        if f.result > 0 then (Tezos.transaction (GrantItem (item, qty,fa.owner)) 0tez (Tezos.get_contract d.shop_addr))::op
        else if f.result < 0 then (Tezos.transaction (GrantItem (item, qty,fb.owner)) 0tez (Tezos.get_contract d.shop_addr))::op
        else op
    | _ -> op
	) in
    // Calculate XP to grant
    let xpa = abs(f.round_cnt+1+f.result) in
    let xpb = abs(f.round_cnt+1-f.result) in
    let (xpa,xpb) = (match reward with
    | XPReward v -> 
        if f.result > 0 then (xpa+v,xpb)
        else if f.result < 0 then (xpa,xpb+v)
        else (xpa,xpb)
    | _ -> (xpa,xpb)
    ) in
    let op =
    	(Tezos.transaction (EarnXP (f.a,xpa)) 0tez (Tezos.get_contract d.attribute_addr))::
    	(Tezos.transaction (EarnXP (f.b,xpb)) 0tez (Tezos.get_contract d.attribute_addr))::
     	op in
    // If we are in a tournament, we report it
    let op = if fa.tournament = 0n then op else
        (Tezos.transaction (ReportFight id) 0tez (Tezos.get_contract d.tournament_addr))::op in
    let d = { d with fights = Big_map.update id (Some f) d.fights } in
    event::op, d

(** Set the address set of admins
    @caller admin
*)
let set_admins (addrs, d : address set * fight_storage) =
    let _ = _admin_only d in
    [], {d with admins = addrs}

(** Set the address set of managers
    Note that we expect to have Smart contracts in there too
    @caller admin
*)
let set_managers (addrs, d : address set * fight_storage) =
    let _ = _admin_only d in
    [], {d with managers = addrs}

(** Set the address set of matchers
    @caller manager
*)
let set_matchers (addrs, d : address set * fight_storage) =
    let _ = _manager_only d in
    [], {d with matchers = addrs}

(** Set the address set of resolver
    @caller admin
*)
let set_resolvers (addrs, d : address set * fight_storage) =
    let _ = _manager_only d in
    [], {d with resolvers = addrs}

(** Set the fight fee to be paid by the user for a Mint
    @caller admin
*)
let set_fight_fee (v, d : tez * fight_storage) =
    let _ = _admin_only d in
    [], {d with fight_fee = v}

(** Set the address of the Fighter smart contract
    @caller admin
*)
let set_fighter_addr (addr, d : address * fight_storage) =
    let _ = _admin_only d in
    [], {d with fighter_addr = addr}

(** Set the address of the Attribute smart contract
    @caller admin
*)
let set_attribute_addr (addr, d : address * fight_storage) =
    let _ = _admin_only d in
    [], {d with attribute_addr = addr}

(** Set the address of the Shop smart contract
    @caller admin
*)
let set_shop_addr (addr, d : address * fight_storage) =
    let _ = _admin_only d in
    [], {d with shop_addr = addr}

(** Set the address of the Tournament smart contract
    @caller admin
*)
let set_tournament_addr (addr, d : address * fight_storage) =
    let _ = _admin_only d in
    [], {d with tournament_addr = addr}

(** SinkFees entrypoint
    Allow the admin to retrieve the funds stored on the contract
    TODO Should only take out the fees, not the full balance with rewards
    @caller admin
*)
let sink_fees (addr, d: address * fight_storage) =
    let _ = _admin_only d in
    [Tezos.transaction unit (Tezos.get_balance ()) (Tezos.get_contract addr)], d


(** Initializing a new fight object with default values *)
let new_fight (id, a, b, round_cnt, queue, round_duration, tournament:
        fight_id * fighter_id * fighter_id * round_amount * fight_queue * round_duration * tournament_id) : fight_data =
    ({
        id = id;
        a = a;
        b = b;
        rounds = [];
        round_cnt = round_cnt;
        queue = queue;
        state = Initialized;
        result = 0;
        metadata = 0x00;
        start_date = Tezos.get_now ();
        last_update = Tezos.get_now ();
        round_duration = round_duration;
        tournament = tournament
    } : fight_data)

(** CreateFight entrypoint
    Initialize a new fight opposing two fighters named A and B
    @caller admin|Tournament
    @call Fighter SetFighterState
    @event newRound (fight_id, fighter_id, fighter_id, round, deadline)
*)
let create_fight (a, b, round_cnt, queue, round_duration, d: 
        fighter_id * fighter_id * round_amount * fight_queue * round_duration * fight_storage) =
    let _ = _matcher_only d in
    let fa = _get_fighter_data (a,d) in
    let _ = if (fa.listed || fa.fight>0n) then failwith ERROR.unavailable_fighter "a" in
    let fb = _get_fighter_data (b,d) in
    let _ = if (fb.listed || fb.fight>0n) then failwith ERROR.unavailable_fighter "b" in
    let _ = if (fa.queue <> Some queue || fb.queue <> Some queue) &&
               (fa.tournament = 0n || fa.tournament <> fb.tournament) then failwith ERROR.different_queue in
    let queue_set = _get_fighters_in_queue (queue,d) in
    let fbfa = Set.add d.next_id (_get_fights_by_fighter (a, d)) in
    let fbfb = Set.add d.next_id (_get_fights_by_fighter (b, d)) in
    let fbf = d.fights_by_fighter in
    let fbf = Big_map.update a (Some fbfa) fbf in
    let fbf = Big_map.update b (Some fbfb) fbf in
    let op = [Tezos.transaction (SetFighterState (a,d.next_id,fa.tournament,None)) 0tez (Tezos.get_contract d.fighter_addr);
         Tezos.transaction (SetFighterState (b,d.next_id,fb.tournament,None)) 0tez (Tezos.get_contract d.fighter_addr);
         Tezos.emit "%newRound" ((d.next_id, a, b, 1n, (Tezos.get_now ()) + round_duration): event_new_round)] in
    // If we are in a tournament, we report it
    let op = if fa.tournament = 0n then op else
        (Tezos.transaction (ReportFight d.next_id) 0tez (Tezos.get_contract d.tournament_addr))::op in
    op, { d with 
        next_id = d.next_id + 1n;
        fights = Big_map.add d.next_id (new_fight (d.next_id, a, b, round_cnt, queue, round_duration, fa.tournament)) d.fights;
        fights_by_fighter = fbf;
        queues = Big_map.update queue (Some (Set.remove b (Set.remove a queue_set))) d.queues
    }

(** ResolveRound entrypoint
    Set the result of the round of a fight, with relevant data
    If this was the last round, this also resolved the whole fight.
    @caller admin|fightmanager
    @event roundResolved (fight_id, round, result, data)
    @event newRound (fight_id, fighter_id, fighter_id, round, deadline)
*)
let resolve_round (id, round, result, data, d: fight_id * nat * int * round_data * fight_storage) =
    let _ = _resolver_only d in
	let f = _get_fight_data (id,d) in
    let _ = if round <> (List.size f.rounds) +1n then failwith ERROR.invalid_round in
    let _ = if round > f.round_cnt then failwith ERROR.invalid_round in
    let d = { d with
    	fights = Big_map.update id 
                (Some { f with 
                    rounds = data::f.rounds;
                    result = f.result + result;
                    state  = OnGoing;
                    last_update = Tezos.get_now ();
                }) d.fights;
    } in
    let event = Tezos.emit "%roundResolved" ((id, round, result, data): event_round_resolved) in
    if round < f.round_cnt
    then [Tezos.emit "%newRound" ((id, f.a, f.b, round+1n, (Tezos.get_now ())+f.round_duration): event_new_round);event], d
	else _resolve_fight(id,event,d)

(** AddToQueue entrypoint
    Allow the owner of a fighter to queue the fighter for a fight.
    @caller owner
    @call Fighter SetFighterState
    @event addedToQueue (fighter_id, queue)
*)
let add_to_queue (a, queue, d: fighter_id * fight_queue * fight_storage) =
	let fa = _get_fighter_data (a,d) in
    let _ = if Tezos.get_sender () <> fa.owner then failwith ERROR.rights_owner in    
    let _ = if fa.listed || Option.is_some fa.queue || fa.tournament <> 0n
            || fa.fight <> 0n || fa.inactive = true || fa.minting = true
    then failwith ERROR.occupied in
    let (_league, stake, reward) = queue in
    // Check that reward is not higher than twice the stake
    let _ = (match reward with
        | TezReward v -> (match stake with
            | TezStake w -> if v > w * 2n then failwith ERROR.invalid_queue
            | _ -> failwith ERROR.invalid_queue )
        | _ -> unit
    ) in
    // Stake the stake
    let op = (match stake with
        | NoStake -> if Tezos.get_amount () <> d.fight_fee then failwith ERROR.fee else []
        | TezStake v -> if Tezos.get_amount () <> (d.fight_fee + v) then failwith ERROR.stake else []
        | ItemStake (item,qty) -> if Tezos.get_amount () <> d.fight_fee then failwith ERROR.stake else
            [Tezos.transaction (ConsumeItem (item,qty,fa.owner)) 0tez (Tezos.get_contract d.shop_addr)]
    ) in
	let queue_set = _get_fighters_in_queue (queue,d) in
    (Tezos.transaction (SetFighterState (a,0n,0n,Some queue)) 0tez (Tezos.get_contract d.fighter_addr))::
     (Tezos.emit "%addedToQueue" ((a, queue): event_added_to_queue))::op,
    { d with queues = Big_map.update queue (Some (Set.add a queue_set)) d.queues }

(** Cancel entrypoint
    Allow the owner of a fighter to cancel an AddToQueue call.
    @caller owner
    @call Fighter SetFighterState
*)
let cancel_queue (a, d: fighter_id * fight_storage) =
	let fa = _get_fighter_data (a,d) in
    let _ = if Tezos.get_sender () <> fa.owner then failwith ERROR.rights_owner in
    let queue = Option.unopt_with_error fa.queue ERROR.not_in_queue in
    let (_, stake, _) = queue in
    let op = [Tezos.transaction (SetFightersFree (Set.literal [a])) 0tez (Tezos.get_contract d.fighter_addr)] in
	let op = (match stake with
        | TezStake v -> (Tezos.transaction unit v (Tezos.get_contract fa.owner))::op
        | ItemStake (item,qty) -> (Tezos.transaction (GrantItem (item,qty,fa.owner)) 0tez (Tezos.get_contract d.shop_addr))::op
        | _ -> op
    ) in
    let queue_set = _get_fighters_in_queue (queue,d) in
    op, { d with queues = Big_map.update queue (Some (Set.remove a queue_set)) d.queues }

(** Main function of the smart contract *)
let main (action, d: fight_parameter * fight_storage) = 
    ( match action with
    | SetAdmins addrs -> set_admins(addrs,d)
    | SetManagers addrs -> set_managers(addrs,d)
    | SetMatchers addrs -> set_matchers(addrs,d)
    | SetResolvers addrs -> set_resolvers(addrs,d)
    | SetFightFee value -> set_fight_fee(value,d)
    | SetFighterAddr addr -> set_fighter_addr(addr,d)
    | SetAttributeAddr addr -> set_attribute_addr(addr,d)
    | SetShopAddr addr -> set_shop_addr(addr,d)
    | SetTournamentAddr addr -> set_tournament_addr(addr,d)
    | CreateFight (a,b,round_cnt,queue,round_duration) -> create_fight(a,b,round_cnt,queue,round_duration,d)
    | ResolveRound (id,round,result,data) -> resolve_round(id,round,result,data,d)
    | AddToQueue (a,queue) -> add_to_queue(a,queue,d)
    | CancelQueue a -> cancel_queue(a,d)
    | SinkFees addr -> sink_fees(addr,d)
    : (operation list * fight_storage))



[@view] let get_fight_data = _get_fight_data
[@view] let get_fighters_in_queue = _get_fighters_in_queue
[@view] let get_fees (_,d: unit * fight_storage) = {
    fight = d.fight_fee
}