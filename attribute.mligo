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

(*let _random_skin (children, r, d: (attribute_leaf list) * nat * attribute_storage) : attribute_tree =
	if List.size children = 0n
	then Leaf "fc5"
	else Node (d.skin_node_limit,children)*)

let new_attribute (id: fighter_id) : attribute_data =
	let r : bytes = rand_hash () in
	let a : nat = byte_to_nat (Bytes.sub 0n 1n r) in
	let b : nat = byte_to_nat (Bytes.sub 1n 1n r) in
	let c : bytes = Bytes.sub 2n 3n r in
	let max : nat = 16n in
    {
        id  = id;
        xp  = 0n;
        str = a mod max;
        agi = (a / max) mod max;
        con = b mod max;
        spd = (b / max) mod max;
        skin = Leaf c
    }

let set_fighter_addr (addr, d : address * attribute_storage) =
    let _ = _admin_only d in
    [], {d with fighter_addr = addr}
let set_fight_addr (addr, d : address * attribute_storage) =
    let _ = _admin_only d in
    [], {d with fight_addr = addr}
let set_skin_node_limit (limit, d : attribute_node * attribute_storage) =
    let _ = _admin_only d in
    [], {d with skin_node_limit = limit}

let rec _add_xp_and_lvl_up (attr, xp, lvl: attribute_data * nat * nat) : (attribute_data * nat) =
	let current_lvl = _get_lvl_from_xp attr.xp in
	let next_lvl_xp = _get_xp_from_lvl (current_lvl+1n) in
	if attr.xp + xp < next_lvl_xp
	then {attr with xp = attr.xp + xp}, lvl
	else let xp = abs(xp + attr.xp - next_lvl_xp) in
	let attr = { attr with
		xp = next_lvl_xp;
		str = attr.str+1n;
		agi = attr.agi+1n;
		con = attr.con+1n;
		spd = attr.spd+1n
	} in
	_add_xp_and_lvl_up (attr, xp, lvl+1n)

let earn_xp (id, xp, d: fighter_id * nat * attribute_storage) =
    if not (Set.mem (Tezos.get_sender ()) (Set.literal [d.admin;d.fight_addr]))
    then failwith ERROR.rights_other
	else let attr = _get_attribute_data (id,d) in
	let (attr, lvl) = _add_xp_and_lvl_up (attr,xp,0n) in
	let op = if lvl = 0n then [] else [Tezos.emit "%levelUp" (id, lvl)] in
	op, { d with attributes = Big_map.update id (Some attr) d.attributes }

let mint (id, d: fighter_id * attribute_storage) =
    if Tezos.get_sender () <> d.fighter_addr
	then failwith ERROR.rights_other
	else [], { d with attributes = Big_map.add id (new_attribute id) d.attributes }


let fusion (_id, _father, _mother, _d: fighter_id * fighter_id * fighter_id * attribute_storage) =
	failwith "Not implemented yet"

let main (action, d: attribute_parameter * attribute_storage) = 
    ( match action with
    | SetFighterAddr addr -> set_fighter_addr(addr,d)
    | SetFightAddr addr -> set_fight_addr(addr,d)
    | SetSkinNodeLimit limit -> set_skin_node_limit(limit,d)
    | EarnXP (id,xp) -> earn_xp(id,xp,d)
    | Mint id -> mint(id,d)
    | Fusion (id,father,mother) -> fusion(id,father,mother,d)
    : (operation list * attribute_storage))


[@view] let get_attribute_data = _get_attribute_data
[@view] let get_addr (_,d: unit * attribute_storage) = {
    fighter = d.fighter_addr;
    fight = d.fight_addr
}