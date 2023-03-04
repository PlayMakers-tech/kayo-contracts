#include "ability.schema.mligo"
#include "error.mligo"
#include "utils.mligo"

let _admin_only (d: ability_storage) =
    if Tezos.get_sender () <> d.admin then failwith ERROR.rights_admin

let _get_ability_data (id, d: ability_id * ability_storage) =
    Option.unopt_with_error (Big_map.find_opt id d.abilities) ERROR.ability_id
let _get_fighter_abilities (id, d: fighter_id * ability_storage) =
    Option.unopt_with_error (Big_map.find_opt id d.fighter_abilities) ERROR.fighter_id
let _get_available_abilities (r, d: rarity * ability_storage) =
    Option.unopt_with_error (Big_map.find_opt r d.available_abilities) ERROR.rarity
let _get_proba_rarity (r, d: rarity * ability_storage) =
    Option.unopt_with_error (Map.find_opt r d.proba_rarity) ERROR.rarity


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
	let _ = if amount > 0n && a.cnt >= amount then failwith ERROR.rarity_overload in
	let known = _get_fighter_abilities (fid, d) in
	let _ = if Set.mem aid known then failwith ERROR.ability_known in
	let d = { d with
		fighter_abilities = Big_map.update fid (Some (Set.add aid known)) d.fighter_abilities;
		abilities = Big_map.update aid (Some {a with cnt = a.cnt + 1n}) d.abilities
	} in
	if a.cnt + 1n = amount
	then let avail = _get_available_abilities (a.rarity,d) in
	{ d with available_abilities = Big_map.update a.rarity (Some (Set.remove aid avail)) d.available_abilities }
	else d

let rec _select_rand_ability (rl, fid, rand, d: rarity list * fighter_id * nat * ability_storage)
		: (ability_id * nat) =
	match rl with
	| rarity::rl -> (
		let available = _get_available_abilities (rarity, d) in
		let known = _get_fighter_abilities (fid, d) in
		let exclude (a, aid: ability_id set * ability_id) : ability_id set = Set.remove aid a in
		let available = Set.fold exclude known available in
		let available_size = Set.cardinal available in
		if available_size = 0n then _select_rand_ability (rl, fid, rand, d)
		else let proba = _get_proba_rarity (rarity, d) in		
		let topick : bool = (rand mod proba) <> 0n in
		let rand = rand / proba in		
		if topick then _select_rand_ability (rl, fid, rand, d)
		else type acc = nat * ability_id in
		let tosel = rand mod available_size in
		let rand = rand / available_size in
		let pick (acc, aid: acc * ability_id) : acc =
			let (i,sel) = acc in
			if i=tosel
			then (i+1n,aid)
			else (i+1n,sel)
			in
		let ( _ , sel) = Set.fold pick available (0n, 0n: acc) in
		(sel,rand)

	)
	| _ -> failwith ERROR.no_ability_left

let _learn_rand_ability (fid, d: fighter_id * ability_storage) =
	let rl : rarity list = [Unique; Mythic; Legendary; Rare; Uncommon; Common] in 
	let rand : nat = rand_hash_as_nat () in
	let (aid, _) : (ability_id * nat) = _select_rand_ability (rl, fid, rand, d) in
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

let mint (id, data, d: fighter_id * bytes * ability_storage) =
    let _ = if Tezos.get_sender () <> d.fighter_addr then failwith ERROR.rights_other in
	let d = { d with fighter_abilities = Big_map.add id Set.empty d.fighter_abilities } in
	let _ = if data<>0x00 then failwith "Minting with forced abilities not implemented yet" in
	let d = _learn_rand_ability (id, d) in
	let d = _learn_rand_ability (id, d) in
	let d = _learn_rand_ability (id, d) in
	let d = _learn_rand_ability (id, d) in
	let d = _learn_rand_ability (id, d) in
	let d = _learn_rand_ability (id, d) in
	[], d

// TODO Fusion most likely to be reworked (this currently ignores rarity count)
let fusion (id, father, mother, data, d: fighter_id * fighter_id * fighter_id * bytes * ability_storage) =
    let _ = if Tezos.get_sender () <> d.fighter_addr then failwith ERROR.rights_other in
	let set = _get_fighter_abilities (father, d) in
	let _ = if data<>0x00 then failwith "Fusion with forced abilities not implemented yet" in
	let merge (a, aid: ability_id set * ability_id) : ability_id set = Set.add aid a in
	let set = Set.fold merge (_get_fighter_abilities (mother, d)) set in
	[], { d with fighter_abilities = Big_map.add id set d.fighter_abilities }

let learn_ability (fid, aid, d: fighter_id * ability_id * ability_storage) =
    let _ = if Tezos.get_sender () <> d.fighter_addr then failwith ERROR.rights_other in
	[], (_learn_ability (fid, aid, d))

let forget_ability (fid, aid, d: fighter_id * ability_id * ability_storage) =
    let _ = if Tezos.get_sender () <> d.fighter_addr then failwith ERROR.rights_other in
	let known = _get_fighter_abilities (fid, d) in
	let _ = if not (Set.mem aid known) then failwith ERROR.ability_unknown in
	[], { d with
		fighter_abilities = Big_map.update fid (Some (Set.remove aid known)) d.fighter_abilities
	}

let main (action, d: ability_parameter * ability_storage) = 
    ( match action with
    | SetFighterAddr addr -> set_fighter_addr(addr,d)
    | SetProbaRarity (r,p) -> set_proba_rarity(r,p,d)
    | SetAmountRarity (r,n) -> set_amount_rarity(r,n,d)
    | CreateAbility rl -> create_ability(rl,d)
    | Mint (id,data) -> mint(id,data,d)
    | Fusion (id,father,mother,data) -> fusion(id,father,mother,data,d)
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