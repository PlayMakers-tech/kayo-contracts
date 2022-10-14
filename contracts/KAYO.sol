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
		// Genealogy
		uint64 father;
		uint64 mother;
	}

  function createFighterInit(uint64 id, address owner) internal pure returns (Fighter memory) {   	
   	return Fighter({
   		id:     id,
   		owner:  payable(owner),
   		price:  0,
   		listed: false,
   		fight:  0,
   		booked: 0,
   		queue:  0,
   		father: 0,
   		mother: 0
   	});
  }
  function createFighterFusion(uint64 id, address owner, Fighter storage f, Fighter storage m) internal view returns (Fighter memory) {
   	return  Fighter({
   		id:     id,
   		owner:  payable(owner),
   		price:  0,
   		listed: false,
   		fight:  0,
   		booked: 0,
   		queue:  0,
   		father: f.id,
   		mother: m.id
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

	function createFight(uint64 id, Fighter memory a, Fighter memory b, uint8 roundCnt, uint8 stake, uint256 price) internal pure returns (Fight memory) {
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

  // ABILITY
	function createAbilityInit(uint16[] storage arr) internal {
   	arr.push(1);
   	arr.push(2);
   	arr.push(3);
   	arr.push(4);
   	arr.push(5);
   	arr.push(6);
  }
  function createAbilityFusion(uint16[] storage arr, uint16[] storage f, uint16[] storage m) internal {
   	for(uint i=0; i < f.length; i++) {
   		arr.push(f[i]);
   	}
   	for(uint i=0; i < m.length; i++) {
   		bool nomatch = true;
   		for(uint j=0; j < f.length; j++) {
   			if(m[i]==f[j]) {
   				nomatch = false;
   				break;
   			}
   		}
   		if(nomatch) arr.push(f[i]);
   	}
  }

  // STATISTICS
	struct Statistics {
		uint64 id;
		uint64 xp;
		uint16 str;
		uint16 agi;
		uint16 con;
		uint16 spd;
	}

	function createStatisticsInit(uint64 id) internal view returns (Statistics memory) {
   	uint r = random(id);
   	return Statistics({
   		id:     id,
   		xp:     0,
   		str:    uint16(1+r%10),
   		agi:    uint16(1+(r/10)%10),
   		con:    uint16(1+(r/100)%10),
   		spd:    uint16(1+(r/1000)%10)
   	});
  }
  function createStatisticsFusion(uint64 id, Statistics storage f, Statistics storage m) internal view returns (Statistics memory) {
   	//uint r = random(id);
   	return Statistics({
   		id:     id,
   		xp:     0,
   		str:    (f.str+m.str)/2,
   		agi:    (f.agi+m.agi)/2,
   		con:    (f.con+m.con)/2,
   		spd:    (f.spd+m.spd)/2
   	});
  }
  function levelUpStatistics(Statistics storage old) internal view returns (Statistics memory) {
   	return Statistics({
   		id:     old.id,
   		xp:     old.xp,
   		str:    old.str+1,
   		agi:    old.agi+1,
   		con:    old.con+1,
   		spd:    old.spd+1
   	});
  }

  function getLevelFromXP(uint64 xp) internal pure returns (uint16) {
  	return uint16(sqrt(xp/10));
  }
  function sqrt(uint64 x) internal pure returns (uint64 y) {
    uint64 z = (x+1)/2;
    y = x;
    while (z < y) {
        y = z;
        z = (x/z + z)/2;
    }
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