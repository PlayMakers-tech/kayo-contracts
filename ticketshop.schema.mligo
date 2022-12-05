#if !TICKETSHOP_SCHEMA
#define TICKETSHOP_SCHEMA

type ticket_value = string

type ticketshop_storage = {
    bank: (address, ticket_value ticket set) big_map;
    price: (ticket_value, tez) map;
    sale: (ticket_value, nat) map;
    admin: address
}

type ticketshop_parameter =
| Buy of ticket_value * nat
| Withdraw of ticket_value * nat
| SetPrice of ticket_value * tez
| SetOnSale of ticket_value * nat
| Grant of ticket_value * nat * address
| SinkFees of address

#endif