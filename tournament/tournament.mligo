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
#include "tournament.event.mligo"
#include "../error.mligo"

(** Private function to check that the caller is admin *)
let _admin_only (d: tournament_storage) =
    if not (Set.mem (Tezos.get_sender ()) d.admins) then failwith ERROR.rights_admin

(** Private function to check that the caller is manager *)
let _manager_only (d: tournament_storage) =
    if not (Set.mem (Tezos.get_sender ()) d.managers) then failwith ERROR.rights_manager

(** Private function to check that the caller is scheduler *)
let _scheduler_only (d: tournament_storage) =
    if not (Set.mem (Tezos.get_sender ()) d.schedulers) then failwith ERROR.rights_scheduler

(** Private function to get fighter data out of its id from Fighter contract *)
let _get_fighter_data (a,d: fighter_id * tournament_storage) =
    (Option.unopt_with_error (Tezos.call_view "get_fighter_data" a d.fighter_addr) ERROR.fighter_id
    : fighter_data)

(** Private function to get fight data out of its id from Fight contract *)
let _get_fight_data (a,d: fight_id * tournament_storage) =
    (Option.unopt_with_error (Tezos.call_view "get_fight_data" a d.fight_addr) ERROR.fight_id
    : fight_data)

(** Private function to get tournament data out of its id *)
let _get_tournament_data (id, d: tournament_id * tournament_storage) =
    Option.unopt_with_error (Big_map.find_opt id d.tournaments) ERROR.tournament_id


(** Set the address set of admins
    @caller admin
*)
let set_admins (addrs, d : address set * tournament_storage) =
    let _ = _admin_only d in
    [], {d with admins = addrs}

(** Set the address set of managers
    @caller admin
*)
let set_managers (addrs, d : address set * tournament_storage) =
    let _ = _admin_only d in
    [], {d with managers = addrs}

(** Set the address set of schedulers
    @caller manager
*)
let set_schedulers (addrs, d : address set * tournament_storage) =
    let _ = _manager_only d in
    [], {d with schedulers = addrs}

let set_tournament_fee (v, d : tez * tournament_storage) =
    let _ = _admin_only d in
    [], {d with tournament_fee = v}
let set_fight_addr (addr, d : address * tournament_storage) =
    let _ = _admin_only d in
    [], {d with fight_addr = addr}
let set_fighter_addr (addr, d : address * tournament_storage) =
    let _ = _admin_only d in
    [], {d with fighter_addr = addr}
let set_shop_addr (addr, d : address * tournament_storage) =
    let _ = _admin_only d in
    [], {d with shop_addr = addr}
let set_attribute_addr (addr, d : address * tournament_storage) =
    let _ = _admin_only d in
    [], {d with attribute_addr = addr}

(** SinkFees entrypoint
    Allow the admin to retrieve the funds stored on the contract
    TODO Should only take out the fees, not the full balance with rewards
    @caller admin
*)
let sink_fees (addr, d: address * tournament_storage) =
    let _ = _admin_only d in
    [Tezos.transaction unit (Tezos.get_balance ()) (Tezos.get_contract addr)], d

(** Initializing a new tournament object with default values *)
let new_tournament (id, def, stamp: tournament_id * tournament_def * timestamp) : tournament_data =
	({
		id = id;
		fighters = Set.empty;
		fights = Set.empty;
		pending_fights = Set.empty;
		scores = Map.empty;
		phase = 0n;
		start_time = stamp;
		def = def;
		state = Open;
        ranks = [];
        refund = [];
        metadata = 0x00
	} : tournament_data)

(** Recursive utility to affect rewards to top ranked fighters *)
let rec _grant_rewards (ranks, rewards, d: fighter_id list * tournament_reward * tournament_storage) : operation list =
    (match ranks with
        | fid::ft -> (match rewards with
            | reward::rt ->
                let f = _get_fighter_data (fid, d) in
                (match reward with
                    | TezReward v -> (Tezos.transaction unit v (Tezos.get_contract f.owner))::(_grant_rewards (ft,rt,d))
                    | ItemReward (item, qty) -> (Tezos.transaction (GrantItem (item, qty, f.owner)) 0tez (Tezos.get_contract d.shop_addr))::(_grant_rewards (ft,rt,d))
                    | XPReward v -> (Tezos.transaction (EarnXP (fid,v)) 0tez (Tezos.get_contract d.attribute_addr))::(_grant_rewards (ft,rt,d))
                    | _ -> _grant_rewards (ft,rt,d))
            | _ -> [])
        | _ -> [])

(** CreateTournament entrypoint
	@caller tournament_manager
	@event newTournament (id, stake, timestamp)
*)
let create_tournament (def, stamp, d: tournament_def * timestamp * tournament_storage) =
    let _ = _scheduler_only d in
    [Tezos.emit "%newTournament" (d.next_id, def, stamp: event_new_tournament)],
    { d with
        next_id = d.next_id + 1n;
        tournaments = Big_map.add d.next_id (new_tournament (d.next_id, def, stamp)) d.tournaments;
    	active_tournaments = Set.add d.next_id d.active_tournaments
    }

(** CancelTournament entrypoint
    If, for example, we do not reach the minimum amount of fighters required,
    we could take the decision to cancel a tournament
    In that case, we can, externally grant some sorry XP.
    But we also need to refund the stakes back to the owners of fighters, and we
    do so by filling the refund property with the list of owners.
    Those owners will be able to get back their stakes by calling GetRefund.
    Note that an owner can be several times in the list.
    @caller tournament_manager
    @event cancelledTournament tournament_id
*)
let cancel_tournament (id, d: tournament_id * tournament_storage) =
    let _ = _scheduler_only d in
    let t = _get_tournament_data (id,d) in
    let _ = if t.state <> Open then failwith ERROR.cancel_not_open in
    let (_, stake, _, _, _, _) = t.def in
    let add_owner (l, fid: address list * fighter_id) : address list =
        let f = _get_fighter_data (fid,d) in (f.owner)::l
    in
    let t = { t with
        state = Cancelled;
        refund = if stake <> NoStake then Set.fold add_owner t.fighters [] else []
    } in
    [Tezos.transaction (SetFightersFree t.fighters) 0tez (Tezos.get_contract d.fighter_addr);
     Tezos.emit "%cancelledTournament" (id: event_cancelled_tournament)],
    { d with
        active_tournaments = Set.remove id d.active_tournaments;
        tournaments = Big_map.update id (Some t) d.tournaments
    }

(** GetRefund entrypoint
    If a tournament was cancelled, owners can get a refund on their stakes
    @caller owner
*)
let get_refund (id, d: tournament_id * tournament_storage) =
    let t = _get_tournament_data (id,d) in
    let _ = if t.state <> (Cancelled: tournament_state) then failwith ERROR.no_refund in
    let sender = Tezos.get_sender () in
    let (_, stake, _, _, _, _) = t.def in
    let recurse (l, addr: (operation list * address list) * address) : (operation list * address list) =
        let (op, r) = l in
        if addr <> sender then (op, addr::r) else
        let op = (match stake with
        | TezStake v -> (Tezos.transaction unit v (Tezos.get_contract addr))::op
        | ItemStake (item,qty) -> (Tezos.transaction (GrantItem (item,qty,addr)) 0tez (Tezos.get_contract d.shop_addr))::op
        | _ -> op
        ) in (op, r)
    in
    let (op, r) = List.fold recurse t.refund ([],[]) in
    let _ = if List.length op = 0n then failwith ERROR.no_refund in
    op, { d with tournaments = Big_map.update id (Some {t with refund = r}) d.tournaments }

(** JoinTournament entrypoint
	Allow a fighter to register for a tournament in Open state
	@caller owner
    @event joinedTournament (tournament_id, fighter_id)
*)
let join_tournament (id, a, d: tournament_id * fighter_id * tournament_storage) =
	let t = _get_tournament_data (id,d) in
    let _ = if t.state <> Open then failwith ERROR.join_not_open in
	let fa = _get_fighter_data (a,d) in
	let _ = if Tezos.get_sender () <> fa.owner then failwith ERROR.rights_owner in
	let _ = if fa.listed || fa.tournament <> 0n || fa.fight <> 0n || Option.is_some fa.queue
            || fa.inactive || fa.minting then failwith ERROR.occupied in
	let (_, stake, _, _, _, max_size) = t.def in
    // Stake the stake
    let op = (match stake with
        | NoStake -> if Tezos.get_amount () <> d.tournament_fee then failwith ERROR.fee else []
        | TezStake v -> if Tezos.get_amount () <> (d.tournament_fee + v) then failwith ERROR.stake else []
        | ItemStake (item,qty) -> if Tezos.get_amount () <> d.tournament_fee then failwith ERROR.stake else
            [Tezos.transaction (ConsumeItem (item,qty,fa.owner)) 0tez (Tezos.get_contract d.shop_addr)]
    ) in
    let op = (Tezos.transaction (SetFighterState (a,0n,id,None)) 0tez (Tezos.get_contract d.fighter_addr))::op in
    let op = (Tezos.emit "%joinedTournament" (id, a: event_joined_tournament))::op in
    let t = { t with
        fighters = Set.add a t.fighters;
        state = if max_size = (1n + Set.cardinal t.fighters) then Closed else t.state
        } in
    op,
	{ d with tournaments = Big_map.update id (Some t) d.tournaments }


(** GenerateTree entrypoint
	Go from the Open to the Closed phase: it is not possible for fighters to register
	any more, but the fights are not starting just yet.
	We provide the metadata to generate the brackets
	@caller scheduler
	@event generatedTree tournament_id
*)
let generate_tree (id, metadata, d: tournament_id * tournament_metadata * tournament_storage) =
    let _ = _scheduler_only d in
    let t = _get_tournament_data (id,d) in
    let _ = if t.state <> Open && t.state <> Closed then failwith ERROR.close_not_open in
    let (_, _, _, _, min_size, _) = t.def in
    let _ = if min_size > Set.cardinal t.fighters then failwith ERROR.not_enough_participants in
	let _score_map (map, fid: (fighter_id, int) map * fighter_id) : (fighter_id, int) map =
		Map.add fid 0 map
	in
	let t = { t with 
		state = Closed;
		scores = Set.fold _score_map t.fighters t.scores;
        metadata = metadata
	} in
	[Tezos.emit "%generatedTree" (id, metadata: event_generate_tree)], { d with tournaments = Big_map.update id (Some t) d.tournaments }

(** NextPhase entrypoint
	This is called by the scheduler at the start of every fight phase, the first call
    being after GenerateTree.
    It will specify the array of pairings, which is used to force the matchup of the surviving
    fighters for this phase. The same fighter can't paired twice in the same phase.
	@caller scheduler
    @event nextPhase tournament_id phase
*)
let next_phase (id, pairings, round_amount, round_duration, d: tournament_id * (fighter_id * fighter_id) set * round_amount * round_duration * tournament_storage) =
    let _ = _scheduler_only d in
    let t = _get_tournament_data (id,d) in
    let _ = if t.state <> Closed && t.state <> (OnGoing: tournament_state)
    then failwith ERROR.cant_start_next_phase in
    let _ = if Set.cardinal t.pending_fights <> 0n then failwith ERROR.pending_fights in
    let t = { t with
        state = (OnGoing: tournament_state);
        phase = t.phase + 1n
    } in
    let (league, _, _, _, _, _) = t.def in
    let create_fight (op,(a,b): operation list * (fighter_id * fighter_id)): operation list =
        (Tezos.transaction (CreateFight (a,b,round_amount,(league,NoStake,NoReward),round_duration)) 0tez (Tezos.get_contract d.fight_addr))::op in
    let op = Set.fold create_fight pairings [] in
    let op = (Tezos.emit "%nextPhase" (id, t.phase: event_next_phase))::op in
    op, { d with tournaments = Big_map.update id (Some t) d.tournaments }

(** EndTournament entrypoint
    After all the phases have been completed, this is called by the scheduler in order to provide
    the ranks of the winners.
    Each reward in the rewards list will be awarded to the respective rank in the ranks list.
    @caller scheduler
    @event endedTournament tournament_id
*)
let end_tournament (id, ranks, d: tournament_id * fighter_id list * tournament_storage) =
    let _ = _scheduler_only d in
    let t = _get_tournament_data (id,d) in
    let _ = if t.state <> (OnGoing: tournament_state) then failwith ERROR.end_not_ongoing in
    let _ = if Set.cardinal t.pending_fights <> 0n then failwith ERROR.pending_fights in
    let t = { t with state = (Finished: tournament_state); ranks = ranks } in
    let (_, _, rewards, _, _, _) = t.def in
    let op = _grant_rewards (ranks, rewards, d) in
    let op = (Tezos.transaction (SetFightersFree t.fighters) 0tez (Tezos.get_contract d.fighter_addr))::op in
    let op = (Tezos.emit "%endedTournament" (id: event_ended_tournament))::op in
    op, { d with 
        tournaments = Big_map.update id (Some t) d.tournaments;
        active_tournaments = Set.remove id d.active_tournaments
    }

(** ReportFight entrypoint
    This is called by the Fight contract to report the start or the end of a relevant fight
    We then gather the results to update the scores
    @caller Fight
*)
let report_fight (fid, d: fight_id * tournament_storage) =
    let _ = if Tezos.get_sender () <> d.fight_addr then failwith ERROR.rights_other in
    let f = _get_fight_data (fid,d) in
    let t = _get_tournament_data (f.tournament,d) in
    let t =
        // We report the end of a fight
        if f.state = (Finished: fight_state) then
            let (sa,sb) = if f.result > 0 then (1,-1) else if f.result = 0 then (0,0) else (-1,1) in
            let scores = t.scores in
            let sa : int option = (match (Map.find_opt f.a scores) with
                | Some k -> Some (k + sa)
                | None -> Some sa ) in
            let scores = Map.update f.a sa scores in
            let sb : int option = (match (Map.find_opt f.b scores) with
                | Some k -> Some (k + sb)
                | None -> Some sb ) in
            let scores = Map.update f.b sb scores in    
            { t with 
                pending_fights = Set.remove fid t.pending_fights;
                scores = scores
            }
        // We report the start of a fight
        else
            { t with 
                pending_fights = Set.add fid t.pending_fights;
                fights = Set.add fid t.fights;
            }
        in
    [], { d with tournaments = Big_map.update f.tournament (Some t) d.tournaments }


(** Main function of the smart contract *)
let main (action, d: tournament_parameter * tournament_storage) = 
    ( match action with
    | SetAdmins addrs -> set_admins(addrs,d)
    | SetManagers addrs -> set_managers(addrs,d)
    | SetSchedulers addrs -> set_schedulers(addrs,d)
    | SetTournamentFee value -> set_tournament_fee(value,d)
    | SetFighterAddr addr -> set_fighter_addr(addr,d)
    | SetFightAddr addr -> set_fight_addr(addr,d)
    | SetShopAddr addr -> set_shop_addr(addr,d)
    | SetAttributeAddr addr -> set_attribute_addr(addr,d)
    | CreateTournament (def,stamp) -> create_tournament(def,stamp,d)
    | CancelTournament id -> cancel_tournament(id,d)
    | JoinTournament (id,a) -> join_tournament(id,a,d)
    | GenerateTree (id,metadata) -> generate_tree(id,metadata,d)
    | NextPhase (id,pairings,ra,rd) -> next_phase(id,pairings,ra,rd,d)
    | EndTournament (id,ranks) -> end_tournament(id,ranks,d)
    | ReportFight fid -> report_fight(fid,d)
    | GetRefund id -> get_refund(id,d)
    | SinkFees addr -> sink_fees(addr,d)
    : (operation list * tournament_storage))



[@view] let get_tournament_data = _get_tournament_data
[@view] let get_fees (_,d: unit * tournament_storage) = {
    tournament = d.tournament_fee
}