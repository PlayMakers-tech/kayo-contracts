# Smart Contracts repository for KAYO

This repository holds the smart contracts deployed to run the online game KAYO.
More information on the game can be found over https://kayo.game

This repository is used as a submodule in the wider context of KAYO, where can be found the
full test infrastructure, the ReactJS frontend, Taqueria's setup and additional off-chain utilities.
More information will be published at a later date.

## Smart contracts

The smart contracts themselves are written in CameLIGO. 
More information can be browsed over https://ligolang.org/?lang=cameligo

Each contract is structured as follow:
- `name.mligo`: where the entrypoints can be found
- `name.event.mligo`: where all the emitted events can be found
- `name.schema.mligo`: where the type definitions can be found
- `name.storageList.mligo`: where initial storage values can be found
- `name.test.mligo`: where unit testing is written

### Overview

Most contracts are interdependent, calling each other, checking the other's identity,
sometimes bidirectionnaly, most often unidirectionnaly.

The starting contract is the Fighter, where anyone can mint a new NFT representing a
unique Fighter.

This fighter can potentially be placed on sale or receive offers through the Marketfighter.

The fighter can also take part into Fights which are duels composed of series of rounds.

A fighter can also join a scheduled Tournament, and the relevant Fights that will be 
generated out of it through several phases.

To get a Fighter, or join Fights and Tournaments, it might be required to possess items
from the Shop. Items can also be rewarded in cases of victory.

A fighter possess abilities, listed in the Ability contract. Those abilities have levels
of rarities, as well as some off-chain information characterizing them.

A fighter is born with attributes and traits, which are saved in the Attribute contract
in an encrypted fashion. It is one of the mechanic of the game to attempt to assess each
fighter's attributes and get the most of them.

### Details

Please browse the documentation in the code to learn more about the details of the
smart contracts, the entrypoints, the data model, and the behavior.

When/If time allows, we will try to extract some nice illustrated documentation.


## Build

Once available, it will be recommended to use to top level repository to work on the smart
contracts and test them in the bigger picture.

For information, it was transpiled to Michelson using Ligo 0.63.2 within Taqueria 0.28.6.


## Tests

A CameLIGO test file stands beside each contract. It holds basic unit testing.
Additionnaly, since a lot of interaction are present, some top-level ligo-level tests were
written.

Since those contracts are meant to be mainly used within the frontend of the KAYO game,
a full blown series of tests in Jest (using taquito) are available within the parent
repository. It covers some of the holes that could be present on the ligo tests only. 

An HTML report can be generated (through TAP, using perl) at any time.

