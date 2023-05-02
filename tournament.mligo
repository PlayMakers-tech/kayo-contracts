(**
    Tournament smart contract
    This contract manages battles tournament, compiling Fights
    between Fighters.
    We have here ids for each tournament, linked to an object where we
    can learn about the fighters and fights involved, its current state,
    the data of its phases and its other metadata.
    This is also through this contract that a tournament can be initiated,
    can advance in its state, and where an owner can register a Fighter.
    The tournaments use the Fighter and Fight contracts.
    @author Maxime Niankouri - PlayMakers - 2023
    @version 1.0.0
*)
#include "tournament.schema.mligo"
#include "error.mligo"
#include "event.mligo"

(** Private function to check that the caller is admin *)
let _admin_only (d: tournament_storage) =
    if Tezos.get_sender () <> d.admin then failwith ERROR.rights_admin

(** Private function to get fighter data out of its id from Fighter contract *)
let _get_fighter_data (a,d: fighter_id * tournament_storage) =
    (Option.unopt_with_error (Tezos.call_view "get_fighter_data" a d.fighter_addr) ERROR.fighter_id
    : fighter_data)

(** Private function to get tournament data out of its id *)
let _get_tournament_data (id, d: tournament_id * tournament_storage) =
    Option.unopt_with_error (Big_map.find_opt id d.tournaments) ERROR.tournament_id

let set_tournament_fee (v, d : tez * tournament_storage) =
    let _ = _admin_only d in
    [], {d with tournament_fee = v}
let set_fight_addr (addr, d : address * tournament_storage) =
    let _ = _admin_only d in
    [], {d with fight_addr = addr}
let set_fighter_addr (addr, d : address * tournament_storage) =
    let _ = _admin_only d in
    [], {d with fighter_addr = addr}

(** SinkFees entrypoint
    Allow the admin to retrieve the funds stored on the contract
    TODO Should only take out the fees, not the full balance with rewards
    @caller admin
*)
let sink_fees (addr, d: address * tournament_storage) =
    let _ = _admin_only d in
    [Tezos.transaction unit (Tezos.get_balance ()) (Tezos.get_contract addr)], d

(** Initializing a new tournament object with default values *)
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

(** CreateTournament entrypoint
	@caller tournament_manager
	@event newTournament (id, stake, timestamp)
*)
let create_tournament (stake, stamp, d: tournament_stake * timestamp * tournament_storage) =
    let _ = _admin_only d in
    [Tezos.emit "%newTournament" ((d.next_id, stake, stamp): event_new_tournament)],
    { d with
        next_id = d.next_id + 1n;
        tournaments = Big_map.add d.next_id (new_tournament (d.next_id, stake, stamp)) d.tournaments;
    	active_tournaments = Set.add d.next_id d.active_tournaments
    }

(** CancelTournament entrypoint
	@caller tournament_manager
*)
let cancel_tournament (id, d: tournament_id * tournament_storage) =
    let _ = _admin_only d in
    let t = _get_tournament_data (id,d) in
    let _ = if t.state <> Open then failwith ERROR.cancel_not_open in
	let _free_fighters (op, fid : operation list * fighter_id) : operation list =
		(Tezos.transaction (SetFighterState (fid,0n,0n,NotQueuing)) 0tez (Tezos.get_contract d.fighter_addr))::op
	in
	(Set.fold _free_fighters t.fighters []), { d with
		active_tournaments = Set.remove id d.active_tournaments;
		tournaments = Big_map.update id (Some {t with state = Cancelled}) d.tournaments
	}

(** JoinTournament entrypoint
	Allow a fighter to register for a tournament in Open state
	@caller owner
*)
let join_tournament (id, a, d: tournament_id * fighter_id * tournament_storage) =
	let t = _get_tournament_data (id,d) in
    let _ = if t.state <> Open then failwith ERROR.join_not_open in
	let fa = _get_fighter_data (a,d) in
	let _ = if Tezos.get_sender () <> fa.owner then failwith ERROR.rights_owner in
	let _ = if fa.listed || fa.tournament <> 0n || fa.fight <> 0n || fa.queue <> NotQueuing
	then failwith ERROR.occupied in
	let _ = (match t.stake with
		| NoStake -> if Tezos.get_amount () <> d.tournament_fee then failwith ERROR.fee
		| FighterStake -> if Tezos.get_amount () <> d.tournament_fee then failwith ERROR.fee
		| TezStake v -> if Tezos.get_amount () <> (d.tournament_fee + v)  then failwith ERROR.stake
		| CustomStake -> unit
	) in
	let t = _get_tournament_data (id,d) in
	[Tezos.transaction (SetFighterState (a,0n,id,NotQueuing)) 0tez (Tezos.get_contract d.fighter_addr)],
	{ d with tournaments = Big_map.update id (Some {t with fighters = Set.add a t.fighters}) d.tournaments }


(** GenerateTree entrypoint
	Go from the Open to the Closed phase: it is not possible for fighters to register
	any more, but the fights are not starting just yet.
	We compute the brackets.
	@caller tournament_manager
	@event generatedTree tournament_id
*)
let generate_tree (id, _seed, d: tournament_id * nat * tournament_storage) =
    let _ = _admin_only d in
    let t = _get_tournament_data (id,d) in
    let _ = if t.state <> Open then failwith ERROR.close_not_open in
	let _score_map (map, fid: (fighter_id, int) map * fighter_id) : (fighter_id, int) map =
		Map.add fid 0 map
	in
	let t = { t with 
		state = Closed;
		scores = Set.fold _score_map t.fighters t.scores
	} in
	[Tezos.emit "%generatedTree" (id: event_generate_tree)], { d with tournaments = Big_map.update id (Some t) d.tournaments }

(** NextPhase entrypoint
	TODO Not implemented yet
	@caller tournament_manager
*)
let next_phase (id, d: tournament_id * tournament_storage) =
    let _ = _admin_only d in
    let t = _get_tournament_data (id,d) in
    let _ = if Set.cardinal t.pending_fights <> 0n then failwith ERROR.pending_fights in
    let _ = if t.state <> (Closed: tournament_state) && t.state <> (OnGoing: tournament_state)
    then failwith ERROR.cant_start_next_phase in
    failwith "Not implemented yet"
    // Don't forget to remove from active_tournaments


(** Main function of the smart contract *)
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