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

type tournament_stake = fight_stake

type tournament_data = {
    id: tournament_id;
    fighters: fighter_id set;
    fights: fight_id set;
    pending_fights: fight_id set;
    scores: (fighter_id, int) map;
    phase: nat;
    start_time: timestamp;
    stake: tournament_stake;
    state: tournament_state
}

type tournament_storage = {
    admins: address set;
    managers: address set;
    schedulers: address set;
    next_id: tournament_id;
    tournament_fee: tez;
    fighter_addr: address;
    fight_addr: address;
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
| CreateTournament of tournament_stake * timestamp
| CancelTournament of tournament_id
| JoinTournament of tournament_id * fighter_id
| GenerateTree of tournament_id * nat
| NextPhase of tournament_id
| SinkFees of address

#endif