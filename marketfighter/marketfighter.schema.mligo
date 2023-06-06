#if !MARKETFIGHTER_SCHEMA
#define MARKETFIGHTER_SCHEMA

type marketfighter_data = {
    date: timestamp;
    price: tez
}

#include "../fighter/fighter.schema.mligo"

type marketfighter_storage = {
    admins: address set;
    managers: address set;
    is_open: bool;
    listing_fee: tez;
    fighter_addr: address;
    min_price: tez;
    listed_sale: fighter_id set;
    listed_offer: fighter_id set;
    sells: (fighter_id, marketfighter_data) big_map;
    buys: (fighter_id, (address, marketfighter_data) map) big_map;
}

type marketfighter_parameter =
| SetAdmins of address set
| SetManagers of address set
| SetMarketOpen of bool
| SetListingFee of tez
| SetMinPrice of tez
| SetFighterAddr of address
| SinkFees of address
| Sell of fighter_id * tez
| Buy of fighter_id * tez 
| Cancel of fighter_id 
| CancelFor of fighter_id * address 


#endif