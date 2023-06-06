#include "marketfighter.schema.mligo"

(** Sold event, whenever a transaction occurs
    @param fight_id: the id of the fighter
    @param tez:      the price of the sale
*)
type event_sold = fighter_id * tez

(** Selling event, whenever a fighter is placed on sale
    @param fight_id: the id of the fighter
    @param tez:      the price of the sale
*)
type event_selling = fighter_id * tez

(** Buying event, whenever a bid is placed on a fighter
    @param fight_id: the id of the fighter
*)
type event_buying = fighter_id

(** CancelSelling event, whenever a user cancels a sale
    @param fight_id: the id of the fighter
*)
type event_cancel_selling = fighter_id

(** CancelBuying event, whenever a user cancels a bid
    @param fight_id: the id of the fighter
*)
type event_cancel_buying = fighter_id
