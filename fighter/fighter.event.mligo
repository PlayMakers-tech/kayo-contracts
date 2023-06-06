#include "fighter.schema.mligo"

(** Minting event, whenever a new user requests a new mint
    @param fighter_id: the id of the new minting fighter
    @param shop_item:  the item used to request the mint (can be used for special treatment)
*)
type event_minting = fighter_id * (shop_item option)

(** Inactive event, whenever a fighter becomes inactive (likely due to a fusion)
    @param fighter_id: the id of the fighter
*)
type event_inactive = fighter_id


(** Minted event, once a fighter is fully minted
    @param fighter_id: the id of the fighter
*)
type event_minted = fighter_id

(** Transfer event, whenever a fighter changes owner
    @param fighter_id: the id of the fighter
    @param address:    the former owner
    @param address:    the new owner
*)
type event_transfer = fighter_id * address * address
