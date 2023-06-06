#include "tournament.schema.mligo"

(** New Tournament event, whenever a new tournament is created
    @param tournament_id:  the id of the new tournament
    @param tournament_def: the definition of the tournament
    @param timestamp:      the planned start date
*)
type event_new_tournament = tournament_id * tournament_def * timestamp

(** Joined Tournament event, whenever a fighter joins an open tournament
    @param tournament_id:  the id of the tournament
    @param fighter_id:     the id of the fighter
*)
type event_joined_tournament = tournament_id * fighter_id

(** Cancelled Tournament event, whenever a tournament is cancelled
    @param tournament_id:  the id of the tournament
*)
type event_cancelled_tournament = tournament_id

(** Generate Tree event, whenever a tournament is closed
    @param tournament_id:       the id of the new tournament
    @param tournament_metadata: metadata to be used to construct the brackets
*)
type event_generate_tree = tournament_id * tournament_metadata

(** Next Phase event, whenever a tournament goes to its next phase
    @param tournament_id: the id of the tournament
    @param nat:           the phase of the tournament goes in (starts at 1)
*)
type event_next_phase = tournament_id * nat

(** End Tournament event, whenever a new tournament is finishing
    @param tournament_id:  the id of the ending tournament
*)
type event_ended_tournament = tournament_id
