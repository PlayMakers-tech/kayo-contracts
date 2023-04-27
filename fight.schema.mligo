#if !FIGHT_SCHEMA
#define FIGHT_SCHEMA

type fight_queue =
| NotQueuing
| NoStakeQ
| FighterStakeQ
| TezStakeQ of tez

type fight_stake =
| NoStake
| CustomStake
| FighterStake
| TezStake of tez

type fight_state =
| Cancelled
| Initialized
| OnGoing
| Finished


type fight_metadata = bytes
type fight_id = nat
type round_amount = nat
type round_data = bytes
type round_duration = int
type strategy_data = bytes

#include "fighter.schema.mligo"

type fight_data = {
    id: fight_id;
    a: fighter_id;
    b: fighter_id;
    rounds: round_data list;
    round_cnt: round_amount;
    stake: fight_stake;
    state: fight_state;
    result: int;
    start_date: timestamp;
    round_duration: round_duration;
    metadata: fight_metadata
}

type fight_storage = {
    next_id: fight_id;
    fight_fee: tez;
    fighter_addr: address;
    tournament_addr: address;
    attribute_addr: address;
    admin: address;
    fights: (fight_id, fight_data) big_map;
    fights_by_fighter: (fighter_id, fight_id set) big_map;
    queues: (fight_queue, fighter_id set) big_map
}

type fight_parameter =
| SetFightFee of tez
| SetFighterAddr of address
| SetTournamentAddr of address
| SetAttributeAddr of address
| CreateFight of fighter_id * fighter_id * round_amount * fight_stake * round_duration
| ResolveRound of fight_id * nat * int * round_data
| SetStrategy of fight_id * fighter_id * strategy_data
| AddToQueue of fighter_id * fight_queue
| CancelQueue of fighter_id
| SinkFees of address


#endif