#include "attribute.schema.mligo"

(** LevelUp event, after a call of EarnXP 
    @param fighter_id: the fighter who levelled up
    @param nat:        the new level reached by this fighter
*)
type event_level_up = fighter_id * nat
