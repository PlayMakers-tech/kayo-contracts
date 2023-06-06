#include "shop.schema.mligo"

(** Bought item event, whenever an item is bought
    @param shop_item: the name of the bought item
    @param nat:       the bought quantity
    @param address:   the address of the buyer
*)
type event_bought_item = shop_item * nat * address 

(** Bought bundle event, whenever a bundle is bought
    @param shop_bundle: the name of the bought bundle
    @param nat:         the bought quantity
    @param address:     the address of the buyer
*)
type event_bought_bundle = shop_bundle * nat * address

