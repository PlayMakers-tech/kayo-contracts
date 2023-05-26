#if !TOURNAMENT_SCHEMA
#define TOURNAMENT_SCHEMA

type tournament_id = nat
type tournament_state =
| Cancelled
| Open
| Closed
| OnGoing
| Finished

#include "fight.schema.mligo"
#include "fighter.schema.mligo"

type tournament_league = fight_league
type tournament_stake = fight_stake
type tournament_reward = fight_reward list
type tournament_pairing = string
type tournament_size = nat
type tournament_def = tournament_league * tournament_stake * tournament_reward *
                      tournament_pairing * tournament_size * tournament_size
type tournament_metadata = bytes

type tournament_data = {
    id: tournament_id;
    fighters: fighter_id set;
    fights: fight_id set;
    pending_fights: fight_id set;
    scores: (fighter_id, int) map;
    phase: nat;
    start_time: timestamp;
    def: tournament_def;
    state: tournament_state;
    ranks: fighter_id list;
    refund: address list;
    metadata: tournament_metadata
}

type tournament_storage = {
    admins: address set;
    managers: address set;
    schedulers: address set;
    next_id: tournament_id;
    tournament_fee: tez;
    fighter_addr: address;
    fight_addr: address;
    shop_addr: address;
    attribute_addr: address;
    active_tournaments: tournament_id set;
    tournaments: (tournament_id, tournament_data) big_map
}

type tournament_parameter =
| SetAdmins of address set
| SetManagers of address set
| SetSchedulers of address set
| SetTournamentFee of tez
| SetFighterAddr of address
| SetFightAddr of address
| SetShopAddr of address
| SetAttributeAddr of address
| CreateTournament of tournament_def * timestamp
| CancelTournament of tournament_id
| GenerateTree of tournament_id * tournament_metadata
| NextPhase of tournament_id * (fighter_id * fighter_id) set * round_amount * round_duration
| EndTournament of tournament_id * fighter_id list
| ReportFight of fight_id
| SinkFees of address
// User entrypoints:
| JoinTournament of tournament_id * fighter_id
| GetRefund of tournament_id

#endif