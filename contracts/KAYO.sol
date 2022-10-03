// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.9;

library KAYO {

	uint constant QUEUE_LENGTH = 8;
	uint constant TOURNAMENT_MAX_SIZE = 16;

	// FIGHTER
	struct Fighter {
		uint64 id;
		address payable owner;
		// Relevant for Marketplace
		uint256 price;
		bool listed;
		// Relevant for fights
		uint64 fight; // 0 or fightId
		uint64 booked; // 0 or tournamentId
		uint8 queue; // 0 or stakeType
		// Relevant for gameplay
		uint16 str;
		uint16 agi;
		uint16 con;
		uint16 spd;
	}

   function createFighterInit(uint64 id, address owner) internal view returns (Fighter memory) {
   	uint r = random(id);
   	return Fighter({
   		id:     id,
   		owner:  payable(owner),
   		price:  0,
   		listed: false,
   		fight:  0,
   		booked: 0,
   		queue:  0,
   		str:    uint16(1+r%10),
   		agi:    uint16(1+(r/10)%10),
   		con:    uint16(1+(r/100)%10),
   		spd:    uint16(1+(r/1000)%10)
   	});
   }
   function createFighterFusion(uint64 id, address owner, Fighter storage f, Fighter storage m) internal view returns (Fighter memory) {
   	uint r = random(id);
   	return  Fighter({
   		id:     id,
   		owner:  payable(owner),
   		price:  0,
   		listed: false,
   		fight:  0,
   		booked: 0,
   		queue:  0,
   		str:    (f.str+ m.str+1)/2,
   		agi:    (f.agi+ m.agi+1)/2,
   		con:    (f.con+ m.con+1)/2,
   		spd:    (f.spd+ m.spd+1)/2
   	});
   }

  function random(uint64 add) private view returns (uint) {
    return uint(keccak256(abi.encodePacked(block.difficulty, block.timestamp, msg.sender, add)));
  }


  // FIGHT
	struct Fight {
		// Fight id
		uint64 id;
		// Fighters
		uint64 a;
		uint64 b;
		// Rounds
		uint8 roundCnt;
		uint8 round;
		// Type of stake
		uint8 stake;
		uint256 price;
		// Outcome and statistics
		uint8 state;
		int8 result;
		uint256 data;
	}

	function createFight(uint64 id, Fighter memory a, Fighter memory b, uint8 roundCnt, uint8 stake, uint256 price) internal returns (Fight memory) {
		// Require msg.sender to be one of the owners or the _owner of the contract
		// Require distinct owners ?
		require(!a.listed && !b.listed);
   	return Fight({
   		id:       id,
   		a:        a.id,
   		b:        b.id,
   		roundCnt: roundCnt,
   		round:    0,
   		stake:    stake,
   		price:    price,
   		state:    1, // Initialized (0 will be 'cancelled')
   		result:   0,
   		data:     0
   	});
  }

  /*
	// FIGHTER
	struct Tournament {
		uint64 id;
		uint64[] fighter;
		uint64[] fight;
		// Rounds
		uint8 roundCnt;
		// Stake
		uint8 stake;
		uint256 price;
		// Outcome and statistics
		uint8 state;
		uint8 result;
		uint256 data;
	}*/

}