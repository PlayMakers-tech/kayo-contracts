#include "attribute.schema.mligo"
#include "utils.mligo"
#include "error.mligo"

let _admin_only (d: attribute_storage) =
    if Tezos.get_sender () <> d.admin then failwith ERROR.rights_admin

let _get_attribute_data (id, d: fighter_id * attribute_storage) =
    Option.unopt_with_error (Big_map.find_opt id d.attributes) "Invalid fighter_id"

let _get_xp_from_lvl (lvl: nat) =
	lvl*lvl*10n
let _get_lvl_from_xp (xp: nat) =
	abs (sqrt (xp/10))

let rec _pick_with_proba (a, l: nat * attribute_skin_node list) : attribute_skin_node =
	match l with
	| (id,size,llh)::t ->
		if a <= llh
		then (id,size,llh)
		else _pick_with_proba (a+llh, t)
	| _ -> failwith ERROR.wrong_proba_skin

let _random_skin_leaf (r, d:  bytes * attribute_storage) : attribute_skin =
	let (p, l) : (nat * attribute_skin_node list) = d.skin_leaves in
	let a : nat = (byte_to_nat (Bytes.sub 0n 1n r)) * 256n + (byte_to_nat (Bytes.sub 1n 1n r)) in
	let a : nat = a mod p in
	let (id, size, _) : attribute_skin_node = _pick_with_proba (a, l) in
	if size = 0n
	then id
	else Bytes.concat id (Bytes.sub 2n size r)

let _random_skin_node (r, n1, n2, d:  bytes * attribute_skin * attribute_skin * attribute_storage) : attribute_skin =
	let (p, l) = d.skin_nodes in
	let a : nat = (byte_to_nat (Bytes.sub 0n 1n r)) * 256n + (byte_to_nat (Bytes.sub 1n 1n r)) in
	let a : nat = a mod p in
	let (id, size, _) : attribute_skin_node = _pick_with_proba (a, l) in
	if size = 0n
	then Bytes.concat id (Bytes.concat n1 n2)
	else Bytes.concat id (Bytes.concat (Bytes.sub 2n size r) (Bytes.concat n1 n2))

let new_attribute (id, data, d: fighter_id * bytes * attribute_storage) : attribute_data =
	let r : bytes = rand_hash () in
	let c : bytes = Bytes.sub 0n 10n r in
	let val_s : nat = byte_to_nat (Bytes.sub 0n 1n data) in
	let val_p : nat = byte_to_nat (Bytes.sub (val_s+1n) 1n data) in
    {
        id  = id;
        xp  = 0n;
        val = Bytes.sub 1n val_s data;
        pot = Bytes.sub (val_s+2n) val_p data;
        skin = _random_skin_leaf (c, d)
    }

let fuse_attribute (id, father, mother, data, d: fighter_id * fighter_id * fighter_id * bytes * attribute_storage) : attribute_data =
	let r : bytes = rand_hash () in
	let f : attribute_data = _get_attribute_data (father, d) in
	let m : attribute_data = _get_attribute_data (mother, d) in
	let c : bytes = Bytes.sub 0n 10n r in
	let val_s : nat = byte_to_nat (Bytes.sub 0n 1n data) in
	let val_p : nat = byte_to_nat (Bytes.sub (val_s+1n) 1n data) in
    {
        id  = id;
        xp  = 0n;
        val = Bytes.sub 1n val_s data;
        pot = Bytes.sub (val_s+2n) val_p data;
        skin = _random_skin_node (c, f.skin, m.skin, d)
    }

let set_fighter_addr (addr, d : address * attribute_storage) =
    let _ = _admin_only d in
    [], {d with fighter_addr = addr}
let set_fight_addr (addr, d : address * attribute_storage) =
    let _ = _admin_only d in
    [], {d with fight_addr = addr}
let set_skin_nodes (proba, l, d : nat * (attribute_skin_node list) * attribute_storage) =
    let _ = _admin_only d in
    [], {d with skin_nodes = (proba, l)}
let set_skin_leaves (proba, l, d : nat * (attribute_skin_node list) * attribute_storage) =
    let _ = _admin_only d in
    [], {d with skin_leaves = (proba, l)}

let rec _add_xp_and_lvl_up (attr, xp, lvl: attribute_data * nat * nat) : (attribute_data * nat) =
	let current_lvl = _get_lvl_from_xp attr.xp in
	let next_lvl_xp = _get_xp_from_lvl (current_lvl+1n) in
	if attr.xp + xp < next_lvl_xp
	then {attr with xp = attr.xp + xp}, lvl
	else let xp = abs(xp + attr.xp - next_lvl_xp) in
	let attr = { attr with xp = next_lvl_xp } in
	_add_xp_and_lvl_up (attr, xp, lvl+1n)

let earn_xp (id, xp, d: fighter_id * nat * attribute_storage) =
    if not (Set.mem (Tezos.get_sender ()) (Set.literal [d.admin;d.fight_addr]))
    then failwith ERROR.rights_other
	else let attr = _get_attribute_data (id,d) in
	let (attr, lvl) = _add_xp_and_lvl_up (attr,xp,0n) in
	let op = if lvl = 0n then [] else [Tezos.emit "%levelUp" (id, lvl)] in
	op, { d with attributes = Big_map.update id (Some attr) d.attributes }

let mint (id, data, d: fighter_id * bytes * attribute_storage) =
    if Tezos.get_sender () <> d.fighter_addr
	then failwith ERROR.rights_other
	else [], { d with attributes = Big_map.add id (new_attribute (id, data, d)) d.attributes }

// TODO The fusion needs to be reworked
let fusion (id, father, mother, data, d: fighter_id * fighter_id * fighter_id * bytes * attribute_storage) =
    if Tezos.get_sender () <> d.fighter_addr
	then failwith ERROR.rights_other
	else [], { d with attributes = Big_map.add id (fuse_attribute (id, father, mother, data, d)) d.attributes }

let main (action, d: attribute_parameter * attribute_storage) = 
    ( match action with
    | SetFighterAddr addr -> set_fighter_addr(addr,d)
    | SetFightAddr addr -> set_fight_addr(addr,d)
    | SetSkinNodes (proba,l) -> set_skin_nodes(proba,l,d)
    | SetSkinLeaves (proba,l) -> set_skin_leaves(proba,l,d)
    | EarnXP (id,xp) -> earn_xp(id,xp,d)
    | Mint (id,data) -> mint(id,data,d)
    | Fusion (id,father,mother,data) -> fusion(id,father,mother,data,d)
    : (operation list * attribute_storage))


[@view] let get_attribute_data = _get_attribute_data
[@view] let get_addr (_,d: unit * attribute_storage) = {
    fighter = d.fighter_addr;
    fight = d.fight_addr
}