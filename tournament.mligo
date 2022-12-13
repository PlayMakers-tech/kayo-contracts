#include "tournament.schema.mligo"
#include "error.mligo"

let _admin_only (d: tournament_storage) =
    if Tezos.get_sender () <> d.admin then failwith ERROR.rights_admin

let _get_fighter_data (a,d: fighter_id * tournament_storage) =
    (Option.unopt_with_error (Tezos.call_view "get_fighter_data" a d.fighter_addr) "Invalid fighter_id"
    : fighter_data)
let _get_tournament_data (id, d: tournament_id * tournament_storage) =
    Option.unopt_with_error (Big_map.find_opt id d.tournaments) "Invalid tournament_id"

let set_tournament_fee (v, d : tez * tournament_storage) =
    let _ = _admin_only d in
    [], {d with tournament_fee = v}
let set_fight_addr (addr, d : address * tournament_storage) =
    let _ = _admin_only d in
    [], {d with fight_addr = addr}
let set_fighter_addr (addr, d : address * tournament_storage) =
    let _ = _admin_only d in
    [], {d with fighter_addr = addr}

let sink_fees (addr, d: address * tournament_storage) =
    let _ = _admin_only d in
    [Tezos.transaction unit (Tezos.get_balance ()) (Tezos.get_contract addr)], d

let new_tournament (id, stake, stamp: tournament_id * tournament_stake * timestamp) : tournament_data =
	{
		id = id;
		fighters = Set.empty;
		fights = Set.empty;
		pending_fights = Set.empty;
		scores = Map.empty;
		phase = 0n;
		start_time = stamp;
		stake = stake;
		state = Open
	}

let create_tournament (stake, stamp, d: tournament_stake * timestamp * tournament_storage) =
    let _ = _admin_only d in
    [], { d with
            next_id = d.next_id + 1n;
            tournaments = Big_map.add d.next_id (new_tournament (d.next_id, stake, stamp)) d.tournaments;
        	active_tournaments = Set.add d.next_id d.active_tournaments
        }


let cancel_tournament (id, d: tournament_id * tournament_storage) =
    let _ = _admin_only d in
    let t = _get_tournament_data (id,d) in
    if t.state <> Open
    then failwith ERROR.cancel_not_open
	else let _free_fighters (op, fid : operation list * fighter_id) : operation list =
		(Tezos.transaction (SetFighterState (fid,0n,0n,NotQueuing)) 0tez (Tezos.get_contract d.fighter_addr))::op
	in
	(Set.fold _free_fighters t.fighters []), { d with
		active_tournaments = Set.remove id d.active_tournaments;
		tournaments = Big_map.update id (Some {t with state = Cancelled}) d.tournaments
	}

let join_tournament (id, a, d: tournament_id * fighter_id * tournament_storage) =
	let t = _get_tournament_data (id,d) in
    if t.state <> Open
    then failwith ERROR.join_not_open
	else let fa = _get_fighter_data (a,d) in
	if Tezos.get_sender () <> fa.owner
	then failwith ERROR.rights_owner
	else if fa.listed || fa.tournament <> 0n || fa.fight <> 0n || fa.queue <> NotQueuing
	then failwith ERROR.occupied
	else let _ = (match t.stake with
		| NoStake -> if Tezos.get_amount () <> d.tournament_fee then failwith ERROR.fee
		| FighterStake -> if Tezos.get_amount () <> d.tournament_fee then failwith ERROR.fee
		| TezStake v -> if Tezos.get_amount () <> (d.tournament_fee + v)  then failwith ERROR.stake
		| CustomStake -> unit
	) in
	let t = _get_tournament_data (id,d) in
	[Tezos.transaction (SetFighterState (a,0n,id,NotQueuing)) 0tez (Tezos.get_contract d.fighter_addr)],
	{ d with tournaments = Big_map.update id (Some {t with fighters = Set.add a t.fighters}) d.tournaments }


let generate_tree (id, seed, d: tournament_id * nat * tournament_storage) =
    let _ = _admin_only d in
    let t = _get_tournament_data (id,d) in
    if t.state <> Open
    then failwith ERROR.close_not_open
	else let _score_map (map, fid: (fighter_id, int) map * fighter_id) : (fighter_id, int) map =
		Map.add fid 0 map
	in
	let t = { t with 
		state = Closed;
		scores = Set.fold _score_map t.fighters t.scores
	} in
	[], { d with tournaments = Big_map.update id (Some t) d.tournaments }

let next_phase (id, d: tournament_id * tournament_storage) =
    let _ = _admin_only d in
    let t = _get_tournament_data (id,d) in
    if Set.cardinal t.pending_fights <> 0n
    then failwith ERROR.pending_fights
    else if t.state <> (Closed: tournament_state) && t.state <> (OnGoing: tournament_state)
    then failwith ERROR.cant_start_next_phase
    else failwith "Not implemented yet"
    // Don't forget to remove from active_tournaments


let main (action, d: tournament_parameter * tournament_storage) = 
    ( match action with
    | SetTournamentFee value -> set_tournament_fee(value,d)
    | SetFighterAddr addr -> set_fighter_addr(addr,d)
    | SetFightAddr addr -> set_fight_addr(addr,d)
    | CreateTournament (stake,stamp) -> create_tournament(stake,stamp,d)
    | CancelTournament id -> cancel_tournament(id,d)
    | JoinTournament (id,a) -> join_tournament(id,a,d)
    | GenerateTree (id,seed) -> generate_tree(id,seed,d)
    | NextPhase id -> next_phase(id,d)
    | SinkFees addr -> sink_fees(addr,d)
    : (operation list * tournament_storage))



[@view] let get_tournament_data = _get_tournament_data
[@view] let get_fees (_,d: unit * tournament_storage) = {
    tournament = d.tournament_fee
}
[@view] let get_addr (_,d: unit * tournament_storage) = {
    fight = d.fight_addr;
    fighter = d.fighter_addr
}