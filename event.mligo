#include "schema.mligo"

// Ability event
// None
// Attributes event
type event_level_up = fighter_id * nat // levelUp
// Fight event
type event_new_round = fight_id * fighter_id * fighter_id * nat * timestamp // newRound
type event_round_resolved = fight_id * nat * int * round_data // roundResolved
type event_strategy = fight_id * fighter_id * strategy_data // strategy
type event_added_to_queue = fighter_id * fight_queue // addedToQueue
// Fighter event
type event_minting = fighter_id * (shop_item option) // minting
type event_minted = fighter_id // minted
type event_transfer = fighter_id * address * address // transfer
// Marketfighter event
type event_sold = fighter_id * tez // sold
type event_selling = fighter_id * tez // selling
type event_buying = fighter_id // buying
type event_cancel_selling = fighter_id // cancelSelling
type event_cancel_buying = fighter_id // cancelBuying
// Shop event
type event_bought_item = shop_item * nat * address // boughtItem
type event_bought_bundle = shop_bundle * nat * address // boughtBundle
// Tournament event
type event_new_tournament = tournament_id * tournament_stake * timestamp  // newTournament
type event_generate_tree = tournament_id // generateTree

