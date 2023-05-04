#if !SHOP_SCHEMA
#define SHOP_SCHEMA

type shop_item = string
type shop_bundle = string

type shop_item_data = {
    item: shop_item;
    quantity: nat;
    consumers: address set;
    price: tez
}

type shop_bundle_data = {
    bundle: shop_bundle;
    quantity: nat;
    items: (shop_item, nat) map;
    (* ops: operation list;  maybe fun here to get Tezos.sender () *)
    price: tez
}

type shop_storage = {
    admins: address set;
    managers: address set;
    is_open: bool;
    items: (shop_item, shop_item_data) map;
    bundles: (shop_bundle, shop_bundle_data) map;
    owned_items: (address, (shop_item, nat) map) big_map
}

type shop_parameter =
| SetAdmins of address set
| SetManagers of address set
| SetShopOpen of bool
| NewItem of shop_item_data
| SetItemPrice of shop_item * tez
| SetItemConsumers of shop_item * (address set)
| NewBundle of shop_bundle_data
| SetBundlePrice of shop_bundle * tez
| DeleteBundle of shop_bundle
| GrantItem of shop_item * nat * address
| BuyItem of shop_item * nat
| BuyBundle of shop_bundle * nat
| ConsumeItem of shop_item * nat * address
| SinkFees of address


#endif