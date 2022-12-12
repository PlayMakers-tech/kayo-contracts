#include "fight.schema.mligo"
#include "attribute.schema.mligo"
#include "error.mligo"

let _admin_only (d: fight_storage) =
    if Tezos.get_sender () <> d.admin then failwith ERROR.rights_admin

let _get_fighter_data (a,d: fighter_id * fight_storage) =
    (Option.unopt_with_error (Tezos.call_view "get_fighter_data" a d.fighter_addr) "Invalid fighter_id"
    : fighter_data)
let _get_fight_data (id, d: fight_id * fight_storage) =
    Option.unopt_with_error (Big_map.find_opt id d.fights) "Invalid fight_id"
let _get_round_data (id, d: round_id * fight_storage) =
    Option.unopt_with_error (Big_map.find_opt id d.rounds) "Invalid round_id"
let _get_fighters_in_queue (q, d: fight_queue * fight_storage): fighter_id set =
    match (Big_map.find_opt q d.queues) with
    | Some x -> x
    | None -> Set.empty


let _resolve_fight (id, d: fight_id * fight_storage) =
	let f = _get_fight_data (id,d) in
    let fa = _get_fighter_data (f.a,d) in
    let fb = _get_fighter_data (f.b,d) in
    let f = { f with state = Finished } in
    let op = [
    	Tezos.transaction (SetFighterState (f.a,0n,fa.tournament,NotQueuing)) 0tez (Tezos.get_contract d.fighter_addr);
    	Tezos.transaction (SetFighterState (f.b,0n,fb.tournament,NotQueuing)) 0tez (Tezos.get_contract d.fighter_addr)
    ] in
    let op = (match f.stake with
    | TezStake v -> 
    	if f.result > 0 then (Tezos.transaction unit (2n*v) (Tezos.get_contract fa.owner))::op
    	else if f.result < 0 then (Tezos.transaction unit (2n*v) (Tezos.get_contract fb.owner))::op
    	else (Tezos.transaction unit v (Tezos.get_contract fa.owner))::
    		 (Tezos.transaction unit v (Tezos.get_contract fb.owner))::op
    | _ -> op
	) in
    let op = (
    	if f.stake = (FighterStake : fight_stake)
    	then
	    	if f.result > 0
	    	then (Tezos.transaction (Transfer (f.b,fa.owner)) 0tez (Tezos.get_contract d.fighter_addr))::op
	    	else if f.result < 0
	    	then (Tezos.transaction (Transfer (f.a,fb.owner)) 0tez (Tezos.get_contract d.fighter_addr))::op
	    	else op
    	else op
    ) in
    let op =
    	(Tezos.transaction (EarnXP (f.a,abs(f.round_cnt+1+f.result))) 0tez (Tezos.get_contract d.attribute_addr))::
    	(Tezos.transaction (EarnXP (f.b,abs(f.round_cnt+1-f.result))) 0tez (Tezos.get_contract d.attribute_addr))::
     	op in
    op,d

let set_fight_fee (v, d : tez * fight_storage) =
    let _ = _admin_only d in
    [], {d with fight_fee = v}
let set_fighter_addr (addr, d : address * fight_storage) =
    let _ = _admin_only d in
    [], {d with fighter_addr = addr}
let set_tournament_addr (addr, d : address * fight_storage) =
    let _ = _admin_only d in
    [], {d with tournament_addr = addr}
let set_attribute_addr (addr, d : address * fight_storage) =
    let _ = _admin_only d in
    [], {d with attribute_addr = addr}

let sink_fees (addr, d: address * fight_storage) =
    let _ = _admin_only d in
    [Tezos.transaction unit (Tezos.get_balance ()) (Tezos.get_contract addr)], d


let new_fight (id, a, b, round_cnt, stake: fight_id * fighter_id * fighter_id * round_amount * fight_stake) =
    ({
        id = id;
        a = a;
        b = b;
        round = 0n;
        round_cnt = round_cnt;
        stake = stake;
        state = Initialized;
        result = 0;
        metadata = ""
    } : fight_data)


let create_fight (a, b, round_cnt, stake, d: 
		fighter_id * fighter_id * round_amount * fight_stake * fight_storage) =
    if not (Set.mem (Tezos.get_sender ()) (Set.literal [d.admin;d.tournament_addr]))
    then failwith ERROR.rights_other
    else let fa = _get_fighter_data (a,d) in
    if (fa.listed || fa.fight>0n) then failwith ERROR.unavailable_fighter "a"
    else let fb = _get_fighter_data (b,d) in
    if (fb.listed || fb.fight>0n) then failwith ERROR.unavailable_fighter "b"
    else
	// EVENTS TO BE ADDED
	[Tezos.transaction (SetFighterState (a,d.next_id,fa.tournament,NotQueuing)) 0tez (Tezos.get_contract d.fighter_addr);
	 Tezos.transaction (SetFighterState (b,d.next_id,fb.tournament,NotQueuing)) 0tez (Tezos.get_contract d.fighter_addr)],
	{ d with 
        next_id = d.next_id + 1n;
        fights = Big_map.add d.next_id (new_fight (d.next_id, a, b, round_cnt, stake)) d.fights
	}

let resolve_round (id, round, result, data, d: fight_id * nat * int * round_data * fight_storage) =
    let _ = _admin_only d in
	let f = _get_fight_data (id,d) in
    if round <> f.round+1n
    then failwith ERROR.invalid_round
    else let d = { d with
    	next_roundid = d.next_roundid + 1n;
    	fights = Big_map.update id 
                (Some { f with 
                    round = f.round + 1n;
                    result = f.result + result
                }) d.fights;
    	rounds = Big_map.add d.next_roundid data d.rounds
    } in
    if round < f.round_cnt
    then [], d
	else _resolve_fight(id,d)


let set_strategy (_id, _a, _data, _d: fight_id * fighter_id * strategy_data * fight_storage) =
    failwith "Not implemented yet"

let add_to_queue (a, queue, d: fighter_id * fight_queue * fight_storage) =
	let fa = _get_fighter_data (a,d) in
    if Tezos.get_sender () <> fa.owner
    then failwith ERROR.rights_owner
    else let _ = (match queue with
    	| NoStakeQ -> if Tezos.get_amount () <> d.fight_fee then failwith ERROR.fee
    	| FighterStakeQ -> if Tezos.get_amount () <> d.fight_fee then failwith ERROR.fee
    	| TezStakeQ v -> if Tezos.get_amount () <> (d.fight_fee + v) then failwith ERROR.stake
        | _ -> failwith ERROR.invalid_queue
    ) in
    if fa.listed || fa.queue <> NotQueuing || fa.tournament <> 0n || fa.fight <> 0n
    then failwith ERROR.occupied
	else let queue_set = _get_fighters_in_queue (queue,d) in
    [Tezos.transaction (SetFighterState (a,0n,0n,queue)) 0tez (Tezos.get_contract d.fighter_addr)],
    { d with queues = Big_map.update queue (Some (Set.add a queue_set)) d.queues }

let cancel_queue (a, d: fighter_id * fight_storage) =
	let fa = _get_fighter_data (a,d) in
    if Tezos.get_sender () <> fa.owner
    then failwith ERROR.rights_owner
	else let op = (match fa.queue with
		| NotQueuing -> failwith ERROR.not_in_queue
		| NoStakeQ -> [Tezos.transaction (SetFighterState (a,0n,0n,NotQueuing)) 0tez (Tezos.get_contract d.fighter_addr)]	
		| FighterStakeQ -> [Tezos.transaction (SetFighterState (a,0n,0n,NotQueuing)) 0tez (Tezos.get_contract d.fighter_addr)]	
    	| TezStakeQ v -> [
    		Tezos.transaction (SetFighterState (a,0n,0n,NotQueuing)) 0tez (Tezos.get_contract d.fighter_addr);
    		Tezos.transaction unit v (Tezos.get_contract fa.owner)
    		]) in
    let queue_set = _get_fighters_in_queue (fa.queue,d) in
    op, { d with queues = Big_map.update fa.queue (Some (Set.remove a queue_set)) d.queues }

let main (action, d: fight_parameter * fight_storage) = 
    ( match action with
    | SetFightFee value -> set_fight_fee(value,d)
    | SetFighterAddr addr -> set_fighter_addr(addr,d)
    | SetTournamentAddr addr -> set_tournament_addr(addr,d)
    | SetAttributeAddr addr -> set_attribute_addr(addr,d)
    | CreateFight (a,b,round_cnt,stake) -> create_fight(a,b,round_cnt,stake,d)
    | ResolveRound (id,round,result,data) -> resolve_round(id,round,result,data,d)
    | SetStrategy (id,a,data) -> set_strategy(id,a,data,d)
    | AddToQueue (a,queue) -> add_to_queue(a,queue,d)
    | CancelQueue a -> cancel_queue(a,d)
    | SinkFees addr -> sink_fees(addr,d)
    : (operation list * fight_storage))



[@view] let get_fight_data = _get_fight_data
[@view] let get_round_data = _get_round_data
[@view] let get_fighters_in_queue = _get_fighters_in_queue
[@view] let get_fees (_,d: unit * fight_storage) = {
    fight = d.fight_fee
}
[@view] let get_addr (_,d: unit * fight_storage) = {
    fighter = d.fighter_addr;
    tournament = d.tournament_addr;
    attribute = d.attribute_addr
}