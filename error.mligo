#if !ERROR_MODULE
#define ERROR_MODULE

module ERROR =
  struct
	type t = string

	let listed        = "The fighter is listed in a marketplace"
	let inactive      = "The fighter is inactive"
	let minting       = "The fighter is not fully minted yet"
	let queued        = "The fighter is in a queue"
	let fighting      = "The fighter is in a fight"
	let tournamenting = "The fighter is in a tournament"
	let occupied      = "Fighter already occupied elsewhere"
	let not_in_queue  = "Fighter not in a queue"
	let minted        = "The fighter is already fully minted"

	let invalid_fighter = "Invalid fighter"

	let fee           = "Missing expected fee value"
	let stake         = "Missing expected stake value"
	let price         = "Missing expected price value"

	let rights_admin  = "Missing admin rights"
	let rights_other  = "Missing admin or contract rights"
	let rights_owner  = "Missing owner rights"

	let rarity_overload = "Rarity overload"
	let ability_known   = "Ability already known"
	let ability_unknown = "Can't forget Ability not known"
	let no_ability_left = "No ability available for given fighter"

	let unavailable_fighter (str:string) = "Unavaiable fighter " ^ str
	let invalid_round   = "Invalid round number"
	let invalid_queue   = "Invalid queue specified"
	let different_queue = "Both fighters are not in the same queue"

	let cancel_not_open = "Can only cancel open tournaments"
	let join_not_open   = "Can only join open tournaments"
	let close_not_open  = "Can only close open tournaments"
	let pending_fights  = "Pending fights"
	let cant_start_next_phase = "Can't start next phase of this tournament"

	let wrong_proba_skin = "Issue with probabilities in skin_nodes/leaves setup"

	let min_price     = "Minimum price not reached"
	let already_owned = "Fighter already owned"
	let not_listed    = "Fighter not listed"

end

#endif