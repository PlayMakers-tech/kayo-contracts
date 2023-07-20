(**
    ERROR module
    Table to error messages
    @author Maxime Niankouri - PlayMakers - 2023
    @version 1.0.0
*)
#if !ERROR_MODULE
#define ERROR_MODULE

module ERROR =
  struct
  type t = string

  let fee                 = "000"
  let stake               = "001"
  let price               = "002"
  let no_refund           = "003"

  let listed              = "100"
  let inactive            = "101"
  let minting             = "102"
  let queued              = "103"
  let fighting            = "104"
  let tournamenting       = "105"
  let occupied            = "106"
  let not_in_queue        = "107"
  let minted              = "108"
  let fighter_id          = "109"
  let invalid_fighter     = "110"
  let name_too_long       = "111"
  let unavailable_fighter = "112"

  let fight_id                = "200"
  let tournament_id           = "201"
  let invalid_round           = "202"
  let invalid_queue           = "203"
  let different_queue         = "204"
  let cancel_not_open         = "205"
  let join_not_open           = "206"
  let close_not_open          = "207"
  let pending_fights          = "208"
  let cant_start_next_phase   = "209"
  let end_not_ongoing         = "210"
  let not_enough_participants = "211"

  let ability_id      = "300"
  let rarity          = "301"
  let rarity_overload = "302"
  let ability_known   = "303"
  let ability_unknown = "304"
  let no_ability_left = "305"

  let shop_item       = "400"
  let shop_bundle     = "401"
  let min_price       = "402"
  let already_owned   = "403"
  let not_listed      = "404"
  let no_offer        = "405"
  let item_no_stock   = "406"
  let item_not_enough = "407"
  let shop_closed     = "408"
  let name_taken      = "409"

  let rights_admin     = "500"
  let rights_manager   = "501"
  let rights_minter    = "502"
  let rights_matcher   = "503"
  let rights_resolver  = "504"
  let rights_scheduler = "505"
  let rights_other     = "506"
  let rights_owner     = "507"

  let wrong_proba_skin = "600"

end

#endif
