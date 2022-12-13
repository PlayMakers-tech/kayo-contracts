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


let new_attribute (id: fighter_id) : attribute_data =
    {
        id  = id;
        xp  = 0n;
        str = 1n;
        agi = 1n;
        con = 1n;
        spd = 1n;
        skin = Leaf "fc5"
    }

let set_fighter_addr (addr, d : address * attribute_storage) =
    let _ = _admin_only d in
    [], {d with fighter_addr = addr}
let set_fight_addr (addr, d : address * attribute_storage) =
    let _ = _admin_only d in
    [], {d with fight_addr = addr}

let rec _add_xp_and_lvl_up (attr, xp: attribute_data * nat) : attribute_data =
	let current_lvl = _get_lvl_from_xp attr.xp in
	let next_lvl_xp = _get_xp_from_lvl (current_lvl+1n) in
	if attr.xp + xp < next_lvl_xp
	then {attr with xp = attr.xp + xp}
	else let xp = abs(xp + attr.xp - next_lvl_xp) in
	let attr = { attr with
		xp = next_lvl_xp;
		str = attr.str+1n;
		agi = attr.agi+1n;
		con = attr.con+1n;
		spd = attr.spd+1n
	} in
	_add_xp_and_lvl_up (attr,xp)

let earn_xp (id, xp, d: fighter_id * nat * attribute_storage) =
    if not (Set.mem (Tezos.get_sender ()) (Set.literal [d.admin;d.fight_addr]))
    then failwith ERROR.rights_other
	else let attr = _get_attribute_data (id,d) in
	let attr = _add_xp_and_lvl_up (attr,xp) in
	[], { d with attributes = Big_map.update id (Some attr) d.attributes }

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
    | EarnXP (id,xp) -> earn_xp(id,xp,d)
    | Mint id -> mint(id,d)
    | Fusion (id,father,mother) -> fusion(id,father,mother,d)
    : (operation list * attribute_storage))


[@view] let get_attribute_data = _get_attribute_data
[@view] let get_addr (_,d: unit * attribute_storage) = {
    fighter = d.fighter_addr;
    fight = d.fight_addr
}