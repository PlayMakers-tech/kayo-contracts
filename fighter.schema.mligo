#if !FIGHTER_SCHEMA
#define FIGHTER_SCHEMA

type fighter_id = nat

#include "fight.schema.mligo"
#include "tournament.schema.mligo"

type fighter_data = {
    id: fighter_id;
    owner: address;
    listed_price: tez;
    listed: bool;
    fight: fight_id;
    tournament: tournament_id;
    queue: fight_queue;
    father: fighter_id;
    mother: fighter_id
}


type fighter_storage = {
    next_id: fighter_id;
    mint_fee: tez;
    fusion_fee: tez;
    list_fee: tez;
    fight_addr: address;
    tournament_addr: address;
    attribute_addr: address;
    ability_addr: address;
    admin: address;
    fighters: (fighter_id, fighter_data) big_map
}

type fighter_parameter =
| Mint
| SetMintFee of tez
| SetFusionFee of tez
| SetListFee of tez
| SetFightAddr of address
| SetTournamentAddr of address
| SetAttributeAddr of address
| SetAbilityAddr of address
| Fusion of fighter_id * fighter_id
| ListFighter of fighter_id * tez
| CancelList of fighter_id
| Buy of fighter_id
| Transfer of fighter_id * address
| SetFighterState of fighter_id * fight_id * tournament_id * fight_queue
| SinkFees of address

#endif