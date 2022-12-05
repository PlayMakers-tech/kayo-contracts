#include "ability.schema.mligo"
#include "error.mligo"

let _admin_only (d: ability_storage) =
    if Tezos.get_sender () <> d.admin then failwith ERROR.rights_admin

let _get_ability_data (id, d: ability_id * ability_storage) =
    Option.unopt_with_error (Big_map.find_opt id d.abilities) "Invalid ability_id"
let _get_fighter_abilities (id, d: fighter_id * ability_storage) =
    Option.unopt_with_error (Big_map.find_opt id d.fighter_abilities) "Invalid fighter_id"
let _get_available_abilities (r, d: rarity * ability_storage) =
    Option.unopt_with_error (Big_map.find_opt r d.available_abilities) "Invalid rarity"
let _get_proba_rarity (r, d: rarity * ability_storage) =
    Option.unopt_with_error (Map.find_opt r d.proba_rarity) "Invalid rarity"


let set_fighter_addr (addr, d : address * ability_storage) =
    let _ = _admin_only d in
    [], {d with fighter_addr = addr}

let set_proba_rarity (r, p, d : rarity * nat * ability_storage) =
    let _ = _admin_only d in
    [], {d with proba_rarity = Map.update r (Some p) d.proba_rarity }

let set_amount_rarity (r, n, d : rarity * nat * ability_storage) =
    let _ = _admin_only d in
    [], {d with amount_rarity = Map.update r (Some n) d.amount_rarity }

let new_ability (id,r: ability_id * rarity) : ability_data = {
	id = id;
	rarity = r;
	cnt = 0n
}

let _learn_ability (fid, aid, d: fighter_id * ability_id * ability_storage) =
	let a = _get_ability_data (aid, d) in
	let amount = Option.unopt (Map.find_opt a.rarity d.amount_rarity) in
	if amount > 0n && a.cnt >= amount
	then failwith ERROR.rarity_overload
	else let known = _get_fighter_abilities (fid, d) in
	if Set.mem aid known
	then failwith ERROR.ability_known
	else let d = { d with
		fighter_abilities = Big_map.update fid (Some (Set.add aid known)) d.fighter_abilities;
		abilities = Big_map.update aid (Some {a with cnt = a.cnt + 1n}) d.abilities
	} in
	if a.cnt + 1n = amount
	then let avail = _get_available_abilities (a.rarity,d) in
	{ d with available_abilities = Big_map.update a.rarity (Some (Set.remove aid avail)) d.available_abilities }
	else d

let rec _select_rand_ability (rl, fid, d: rarity list * fighter_id * ability_storage) : ability_id =
	match rl with
	| rarity::rl -> (
		let r : nat = 0n in // TODO Random here
		let available = _get_available_abilities (rarity, d) in
		let known = _get_fighter_abilities (fid, d) in
		let exclude (a, aid: ability_id set * ability_id) : ability_id set = Set.remove aid a in
		let available = Set.fold exclude known available in
		let available_size = Set.cardinal available in
		if available_size = 0n then _select_rand_ability (rl, fid, d)
		else let proba = _get_proba_rarity (rarity, d) in
		if (r mod proba) <> 0n then _select_rand_ability (rl, fid, d)
		else type acc = nat * ability_id in
		let r = (r/proba) mod available_size in
		let pick (acc, aid: acc * ability_id) : acc =
			let (i,sel) = acc in
			if i=r
			then (i+1n,aid)
			else (i+1n,sel)
			in
		let ( _ , sel) = Set.fold pick available (0n, 0n: acc) in
		sel

	)
	| _ -> failwith ERROR.no_ability_left

let _learn_rand_ability (fid, d: fighter_id * ability_storage) =
	let rl : rarity list = [UNIQUE; MYTHIC; LEGENDARY; RARE; UNCOMMON; COMMON] in 
	let aid : ability_id = _select_rand_ability (rl, fid, d) in
	_learn_ability (fid, aid, d)

let rec _create_ability (rl, d : rarity list * ability_storage) : ability_storage =
	match rl with
	| r::t -> let avail = _get_available_abilities (r,d) in
		_create_ability (t, { d with 
	    	next_id = d.next_id + 1n;
	    	abilities = Big_map.add d.next_id (new_ability (d.next_id,r)) d.abilities;
	    	available_abilities = Big_map.update r (Some (Set.add d.next_id avail)) d.available_abilities
	    })
	| _ -> d

let create_ability (rl, d : rarity list * ability_storage) =
    let _ = _admin_only d in
    [], _create_ability (rl, d)

let mint (id, d: fighter_id * ability_storage) =
    if Tezos.get_sender () <> d.fighter_addr
    then failwith ERROR.rights_other
	else let d = { d with fighter_abilities = Big_map.add id Set.empty d.fighter_abilities } in
	let d = _learn_rand_ability (id, d) in
	let d = _learn_rand_ability (id, d) in
	let d = _learn_rand_ability (id, d) in
	let d = _learn_rand_ability (id, d) in
	[], d


let fusion (_id,_father,_mother, d: fighter_id * fighter_id * fighter_id * ability_storage) =
    if Tezos.get_sender () <> d.fighter_addr
    then failwith ERROR.rights_other
	else failwith "Not implemented yet (fusion)"

let learn_ability (fid, aid, d: fighter_id * ability_id * ability_storage) =
    if Tezos.get_sender () <> d.fighter_addr
    then failwith ERROR.rights_other
	else [], (_learn_ability (fid, aid, d))

let forget_ability (fid, aid, d: fighter_id * ability_id * ability_storage) =
    if Tezos.get_sender () <> d.fighter_addr
    then failwith ERROR.rights_other
	else let known = _get_fighter_abilities (fid, d) in
	if not (Set.mem aid known)
	then failwith ERROR.ability_unknown
	else [], { d with
		fighter_abilities = Big_map.update fid (Some (Set.remove aid known)) d.fighter_abilities
	}

let main (action, d: ability_parameter * ability_storage) = 
    ( match action with
    | SetFighterAddr addr -> set_fighter_addr(addr,d)
    | SetProbaRarity (r,p) -> set_proba_rarity(r,p,d)
    | SetAmountRarity (r,n) -> set_amount_rarity(r,n,d)
    | CreateAbility rl -> create_ability(rl,d)
    | Mint id -> mint(id,d)
    | Fusion (id,father,mother) -> fusion(id,father,mother,d)
    | LearnAbility (fid,aid) -> learn_ability(fid,aid,d)
    | ForgetAbility (fid,aid) -> forget_ability(fid,aid,d)
    : (operation list * ability_storage))


[@view] let get_ability_data = _get_ability_data
[@view] let get_fighter_abilites = _get_fighter_abilities
[@view] let get_available_abilites = _get_available_abilities
[@view] let get_addr (_,d: unit * ability_storage) = {
    fighter = d.fighter_addr
}
[@view] let get_proba (_,d: unit * ability_storage) = d.proba_rarity
[@view] let get_amount (_,d: unit * ability_storage) = d.amount_rarity