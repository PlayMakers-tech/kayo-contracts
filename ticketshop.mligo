#include "ticketshop.schema.mligo"

let buy (t, n, d: ticket_value * nat * ticketshop_storage) =
	match Map.get t d.price with
	| None -> failwith "Invalid ticket value"
	| Some p -> if Tezos.get_amount () <> p then failwith "Invalid payment value"
	else let addr = Tezos.get_sender () in
	let account = Big_map.get addr bank in
	[], { d with
		bank = Big_map.update addr (Some {account with }) d.bank
	}


let sink_fees (addr, d: address * ticketshop_storage) =
    if Tezos.get_sender () <> d.admin
    then failwith "Admin rights required"
    else [Tezos.transaction unit (Tezos.get_balance ()) (Tezos.get_contract addr)], d


// Need to destructure ticket ?
let main (action, d: ticketshop_parameter * ticketshop_storage) = 
    ( match action with
    | Buy (t,n) -> buy(t,n,d)
    | Withdraw (t,n) -> withdraw(t,n,d)
    | SetPrice (t,v) -> set_price(t,v,d)
    | SetOnSale (t,n) -> set_on_sale(t,n,d)
    | Grant (t,n,addr) -> grant(t,n,addr,d)
    | SinkFees addr -> sink_fees(addr,d)
    : (operation list * ticketshop_storage))