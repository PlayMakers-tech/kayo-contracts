#if !FIGHT_SCHEMA
#define FIGHT_SCHEMA

type fight_league = string
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

#include "shop.schema.mligo"

type fight_stake =
| NoStake
| TezStake of tez
| ItemStake of shop_item * nat

type fight_reward =
| NoReward
| XPReward of nat
| FighterReward
| TezReward of tez
| ItemReward of shop_item * nat

type fight_queue = fight_league * fight_stake * fight_reward

#include "fighter.schema.mligo"
#include "tournament.schema.mligo"

type fight_data = {
    id: fight_id;
    a: fighter_id;
    b: fighter_id;
    rounds: round_data list;
    round_cnt: round_amount;
    queue: fight_queue;
    state: fight_state;
    result: int;
    start_date: timestamp;
    round_duration: round_duration;
    tournament: tournament_id;
    metadata: fight_metadata
}

type fight_storage = {
    admins: address set;
    managers: address set;
    matchers: address set;
    resolvers: address set;
    next_id: fight_id;
    fight_fee: tez;
    fighter_addr: address;
    attribute_addr: address;
    shop_addr: address;
    tournament_addr: address;
    fights: (fight_id, fight_data) big_map;
    fights_by_fighter: (fighter_id, fight_id set) big_map;
    queues: (fight_queue, fighter_id set) big_map
}

type fight_parameter =
| SetAdmins of address set
| SetManagers of address set
| SetMatchers of address set
| SetResolvers of address set
| SetFightFee of tez
| SetFighterAddr of address
| SetAttributeAddr of address
| SetShopAddr of address
| SetTournamentAddr of address
| CreateFight of fighter_id * fighter_id * round_amount * fight_queue * round_duration
| ResolveRound of fight_id * nat * int * round_data
| SetStrategy of fight_id * fighter_id * strategy_data
| AddToQueue of fighter_id * fight_queue
| CancelQueue of fighter_id
| SinkFees of address


#endif