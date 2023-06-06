#include "fight.schema.mligo"

(** NewRound event, whenever a new fight round starts
    @param fight_id:   the id of the relevant fight
    @param fighter_id: the fighter involved (A)
    @param fighter_id: the fighter involved (B)
    @param nat:        the round (starts at 1)
    @param timestamp:  the expected deadline for the resolution
*)
type event_new_round = fight_id * fighter_id * fighter_id * nat * timestamp

(** ResolvedRound event, whenever a round is resolved
    @param fight_id:   the id of the relevant fight
    @param nat:        the round (starts at 1)
    @param int:        the result (1 if A won, -1 if B won, 0 if draw)
    @param round_data: the encoded data allowing us to replay the round
*)
type event_round_resolved = fight_id * nat * int * round_data


(** AddToQueue event, whenever a player queues a fighter
    @param fighter_id:  the id of the queued fighter
    @param fight_queue: the relevant queue
*)
type event_added_to_queue = fighter_id * fight_queue
