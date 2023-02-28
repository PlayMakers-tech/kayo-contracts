#if !FIGHTER_SCHEMA
#define FIGHTER_SCHEMA

type fighter_id = nat

#include "fight.schema.mligo"
#include "tournament.schema.mligo"

type fighter_data = {
    id: fighter_id;
    owner: address;
    minting: bool;
    listed: bool;
    inactive: bool;
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
    fight_addr: address;
    tournament_addr: address;
    attribute_addr: address;
    ability_addr: address;
    marketfighter_addr: address;
    admin: address;
    mints: fighter_id set;
    fighters: (fighter_id, fighter_data) big_map;
    fighters_by_owner: (address, fighter_id set) big_map;
}

type fighter_parameter =
| Mint
| RealMint of fighter_id * bytes * bytes
| SetMintFee of tez
| SetFusionFee of tez
| SetFightAddr of address
| SetTournamentAddr of address
| SetAttributeAddr of address
| SetAbilityAddr of address
| SetMarketfighterAddr of address
| Fusion of fighter_id * fighter_id
| SetFighterListed of fighter_id * bool
| Transfer of fighter_id * address
| SetFighterState of fighter_id * fight_id * tournament_id * fight_queue
| SinkFees of address

#endif