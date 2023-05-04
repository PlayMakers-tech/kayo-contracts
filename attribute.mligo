(**
    Attribute smart contract
    This contract hold Attribute data for Fighters.
    We have here ids for each fighters, linked to an object where we
    can get the skin, the xp and encrypted data about a Fighter.
    The attributes are mainly static after minted via the Fighter contract,
    besides the XP which can be incremented by rewards from other contracts.
    Fighter and any contracts granting XP must have manager rights.
    @author Maxime Niankouri - PlayMakers - 2023
    @version 1.0.0
*)
#include "attribute.schema.mligo"
#include "utils.mligo"
#include "error.mligo"
#include "event.mligo"

(** Private function to check that the caller is admin *)
let _admin_only (d: attribute_storage) =
    if not (Set.mem (Tezos.get_sender ()) d.admins) then failwith ERROR.rights_admin

(** Private function to check that the caller is manager *)
let _manager_only (d: attribute_storage) =
    if not (Set.mem (Tezos.get_sender ()) d.managers) then failwith ERROR.rights_manager

(** Private function to get attribute data out of a fighter id *)
let _get_attribute_data (id, d: fighter_id * attribute_storage) =
    Option.unopt_with_error (Big_map.find_opt id d.attributes) ERROR.fighter_id

(** Private function to calculate XP out of level *)
let _get_xp_from_lvl (lvl: nat) =
	lvl*lvl*10n

(** Private function to calculate level out of XP *)
let _get_lvl_from_xp (xp: nat) =
	abs (sqrt (xp/10))

(** Private function to pick a random skin node *)
let rec _pick_with_proba (a, l: nat * attribute_skin_node list) : attribute_skin_node =
	match l with
	| (id,size,llh)::t ->
		if a <= llh
		then (id,size,llh)
		else _pick_with_proba (a+llh, t)
	| _ -> failwith ERROR.wrong_proba_skin

(** Private function to pick a random skin leaf *)
let _random_skin_leaf (r, d:  bytes * attribute_storage) : attribute_skin =
	let (p, l) : (nat * attribute_skin_node list) = d.skin_leaves in
	let a : nat = bytes_to_nat (Bytes.sub 0n 2n r) in
	let a : nat = a mod p in
	let (id, size, _) : attribute_skin_node = _pick_with_proba (a, l) in
	if size = 0n
	then id
	else Bytes.concat id (Bytes.sub 2n size r)

(** Private function to pick a random skin node *)
let _random_skin_node (r, n1, n2, d:  bytes * attribute_skin * attribute_skin * attribute_storage) : attribute_skin =
	let (p, l) = d.skin_nodes in
	let a : nat = bytes_to_nat (Bytes.sub 0n 2n r) in
	let a : nat = a mod p in
	let (id, size, _) : attribute_skin_node = _pick_with_proba (a, l) in
	if size = 0n
	then Bytes.concat id (Bytes.concat n1 n2)
	else Bytes.concat id (Bytes.concat (Bytes.sub 2n size r) (Bytes.concat n1 n2))

(** Initializing a new attribute object with random leaf skin for new mints *)
let new_attribute (id, data, d: fighter_id * attribute_value * attribute_storage) : attribute_data =
	let r : bytes = rand_hash () in
	let c : bytes = Bytes.sub 0n 10n r in
    {
        id  = id;
        xp  = 0n;
        crypted = data;
        skin = _random_skin_leaf (c, d)
    }

(** Initializing a new attribute object with ranbdom leaf node for fusion *)
let fuse_attribute (id, father, mother, data, d: fighter_id * fighter_id * fighter_id * attribute_value * attribute_storage) : attribute_data =
	let r : bytes = rand_hash () in
	let f : attribute_data = _get_attribute_data (father, d) in
	let m : attribute_data = _get_attribute_data (mother, d) in
	let c : bytes = Bytes.sub 0n 10n r in
    {
        id  = id;
        xp  = f.xp + m.xp;
        crypted = data;
        skin = _random_skin_node (c, f.skin, m.skin, d)
    }

(** Set the address set of admins
    @caller admin
*)
let set_admins (addrs, d : address set * attribute_storage) =
    let _ = _admin_only d in
    [], {d with admins = addrs}

(** Set the address set of managers
    Note that we expect to have Smart contracts in there too
    @caller admin
*)
let set_managers (addrs, d : address set * attribute_storage) =
    let _ = _admin_only d in
    [], {d with managers = addrs}

let set_skin_nodes (proba, l, d : nat * (attribute_skin_node list) * attribute_storage) =
    let _ = _manager_only d in
    [], {d with skin_nodes = (proba, l)}

let set_skin_leaves (proba, l, d : nat * (attribute_skin_node list) * attribute_storage) =
    let _ = _manager_only d in
    [], {d with skin_leaves = (proba, l)}

(** Private function to add XP to a fighter and compute the induced level up if any *)
let rec _add_xp_and_lvl_up (attr, xp, lvl: attribute_data * nat * nat) : (attribute_data * nat) =
	let current_lvl = _get_lvl_from_xp attr.xp in
	let next_lvl_xp = _get_xp_from_lvl (current_lvl+1n) in
	if attr.xp + xp < next_lvl_xp
	then {attr with xp = attr.xp + xp}, lvl
	else let xp = abs(xp + attr.xp - next_lvl_xp) in
	let attr = { attr with xp = next_lvl_xp } in
	_add_xp_and_lvl_up (attr, xp, lvl+1n)

(** EarnXP entrypoint
	Grants some XP to the selected fighter
	@caller manager
	@event levelUp (fighter_id, nat)
*)
let earn_xp (id, xp, d: fighter_id * nat * attribute_storage) =
    let _ = _manager_only d in
	let attr = _get_attribute_data (id,d) in
	let (attr, lvl) = _add_xp_and_lvl_up (attr,xp,0n) in
	let op = if lvl = 0n then [] else [Tezos.emit "%levelUp" ((id, lvl): event_level_up)] in
	op, { d with attributes = Big_map.update id (Some attr) d.attributes }

(** Mint entrypoint
	Called through Fighter RealMint when a new fighter is minted
	@caller manager
*)
let mint (id, data, d: fighter_id * attribute_value * attribute_storage) =
    let _ = _manager_only d in
	[], { d with attributes = Big_map.add id (new_attribute (id, data, d)) d.attributes }

(** Fusion entrypoint
	Called through Fighter RealMint when a fused fighter is minted
	TODO The fusion needs to be reworked
	@caller manager
*)
let fusion (id, father, mother, data, d: fighter_id * fighter_id * fighter_id * attribute_value * attribute_storage) =
    let _ = _manager_only d in
	[], { d with attributes = Big_map.add id (fuse_attribute (id, father, mother, data, d)) d.attributes }

(** Main function of the smart contract *)
let main (action, d: attribute_parameter * attribute_storage) = 
    ( match action with
    | SetAdmins addrs -> set_admins(addrs,d)
    | SetManagers addrs -> set_managers(addrs,d)
    | SetSkinNodes (proba,l) -> set_skin_nodes(proba,l,d)
    | SetSkinLeaves (proba,l) -> set_skin_leaves(proba,l,d)
    | EarnXP (id,xp) -> earn_xp(id,xp,d)
    | Mint (id,data) -> mint(id,data,d)
    | Fusion (id,father,mother,data) -> fusion(id,father,mother,data,d)
    : (operation list * attribute_storage))


[@view] let get_attribute_data = _get_attribute_data