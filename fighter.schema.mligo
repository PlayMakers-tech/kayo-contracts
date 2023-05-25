#if !FIGHTER_SCHEMA
#define FIGHTER_SCHEMA

type fighter_id = nat

#include "fight.schema.mligo"
#include "tournament.schema.mligo"
#include "ability.schema.mligo"
#include "shop.schema.mligo"

type fighter_data = {
    id: fighter_id;
    owner: address;
    minting: bool;
    listed: bool;
    inactive: bool;
    fight: fight_id;
    tournament: tournament_id;
    queue: fight_queue option;
    father: fighter_id;
    mother: fighter_id;
    source: shop_item option;
    name: string
}

type fighter_storage = {
    admins: address set;
    managers: address set;
    minters: address set;
    ability_addr: address;
    attribute_addr: address;
    marketfighter_addr: address;
    shop_addr: address;
    next_id: fighter_id;
    mint_fee: tez;
    fusion_fee: tez;
    mints: fighter_id set;
    fighters: (fighter_id, fighter_data) big_map;
    fighters_by_owner: (address, fighter_id set) big_map;
}

type fighter_parameter =
| SetAdmins of address set
| SetManagers of address set
| SetMinters of address set
| SetAttributeAddr of address
| SetAbilityAddr of address
| SetMarketfighterAddr of address
| SetShopAddr of address
| SetMintFee of tez
| SetFusionFee of tez
| RealMint of fighter_id * bytes * (ability_id list)
| SetFighterListed of fighter_id * bool
| SetFighterState of fighter_id * fight_id * tournament_id * (fight_queue option)
| SetFightersFree of fighter_id set
| SinkFees of address
// User entrypoints:
| Mint
| MintFromShop of shop_item
| Fusion of fighter_id * fighter_id
| SetName of fighter_id * string
| Transfer of fighter_id * address

#endif