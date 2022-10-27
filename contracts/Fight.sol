// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.9;

import "@openzeppelin/contracts/utils/Address.sol";
import "hardhat/console.sol";
import "./KAYO.sol";
import "./Fighter.sol";
import "./Statistics.sol";


contract Fight {
    uint64 private _fightIds;
    uint64 private _roundIds;
    address payable private _owner;
    address private _tournamentContractAddress;
    Fighter private _fighterContract;
    Statistics private _statisticsContract;
    uint256 private fightPrice = 0.001 ether;

    mapping(uint64 => KAYO.Fight) private _fightData;
    mapping(uint64 => string)     private _roundData;

    event FightNew(uint64 id, uint64 ida, uint64 idb);
    event FightEnd(uint64 indexed id, uint64 ida, uint64 idb, int8 result);
    event LookFight(uint64 tokenId, uint8 stake);
    event Unlook(uint64 indexed tokenId, uint64 fightId);
    event NextRound(uint64 id, uint64 ida, uint64 idb, uint8 round);
    event RoundResult(uint64 indexed id, uint8 indexed round, int8 result, uint64 roundId);
    event Strategy(uint64 indexed fightId, uint64 indexed tokenId, uint256 data);

    constructor() {
        _owner = payable(msg.sender);
    }

    function getFightAmount() public view returns(uint64) {
        return _fightIds;
    }

    function setTournamentContractAddress(address addr) public {
        require(msg.sender == _owner);
        _tournamentContractAddress = addr;
    }
    function setFighterContract(address addr) public {
        require(_owner == msg.sender);
        _fighterContract = Fighter(addr);
    }
    function setStatisticsContract(address addr) public {
        require(_owner == msg.sender);
        _statisticsContract = Statistics(addr);
    }

    function getFightData(uint64 id) public view returns (KAYO.Fight memory) {
        return _fightData[id];
    }
    function getRoundData(uint64 id) public view returns (string memory) {
        return _roundData[id];
    }

    function getFightPrice() public view returns(uint256) {
        return fightPrice;
    }
    function setFightPrice(uint256 value) public {
        require(_owner == msg.sender);
        fightPrice = value;
    }

    function createFight(uint64 a, uint64 b, uint8 roundCnt, uint8 stake, uint256 price) public returns (uint64) {
        require(_owner == msg.sender || msg.sender == _tournamentContractAddress);
        KAYO.Fighter memory fa = _fighterContract.getFighterData(a);
        KAYO.Fighter memory fb = _fighterContract.getFighterData(b);
        require(!fa.listed&&fa.fight==0);
        require(!fb.listed&&fb.fight==0);
        _fightData[_fightIds] = KAYO.createFight(++_fightIds, fa, fb, roundCnt, stake, price);
        _fighterContract.setFightState(fa.id, _fightIds, fa.booked, 0);
        _fighterContract.setFightState(fb.id, _fightIds, fb.booked, 0);
        emit FightNew(_fightIds, a, b);
        emit NextRound(_fightIds, a, b, 1);
        if(fa.queue!=0) emit Unlook(a, _fightIds);
        if(fb.queue!=0) emit Unlook(b, _fightIds);
        return _fightIds;
    }

    function resolveRound(uint64 id, uint8 round, int8 result, string calldata data) public {
        require(_owner == msg.sender);
        KAYO.Fight storage f = _fightData[id];
        require(round == f.round+1);
        f.round++;
        f.result += result;
        _roundData[++_roundIds] = data;
        emit RoundResult(id, round, result, _roundIds);
        if(round < f.roundCnt) {
            emit NextRound(id, f.a, f.b, round+1);
        }
        else {
            _resolveFight(id);
        }
    }

    // if fightId == 0: this is the default strategy
    function setStrategy(uint64 fightId, uint64 tokenId, uint256 data) public {
        KAYO.Fighter memory fa = _fighterContract.getFighterData(tokenId);
        require(fa.owner == msg.sender);
        emit Strategy(fightId, tokenId, data);
    }


  function _resolveFight(uint64 id) private {
        require(msg.sender == _owner); // Could be _server instead
        KAYO.Fight storage f = _fightData[id];
        require(f.state == 1);
        KAYO.Fighter memory fa = _fighterContract.getFighterData(f.a);
        KAYO.Fighter memory fb = _fighterContract.getFighterData(f.b);
        f.state = 2;
        if(f.stake == 1) {
            // Money case
            if(f.result > 0) {
                Address.sendValue(fa.owner, 2*f.price);
            }
            else if(f.result < 0) {
                Address.sendValue(fb.owner, 2*f.price);
            }
            else {
                Address.sendValue(fa.owner, f.price);
                Address.sendValue(fb.owner, f.price);
            }
        }
        
        _fighterContract.setFightState(f.a, 0, fa.booked, 0);
        _fighterContract.setFightState(f.b, 0, fb.booked, 0);

        // Reward the fighters if corresponding stake
        if(f.stake == 2) {
            if(f.result > 0) {
                _fighterContract.transferTo(fa.owner, fb.id);
            }
            else if(f.result < 0) {
                _fighterContract.transferTo(fb.owner, fa.id);
            }
        }

        // Earn XP
        _statisticsContract.earnXP(fa.id, f.result<0?1:uint8(f.result+2));
        _statisticsContract.earnXP(fb.id, f.result>0?1:uint8(-f.result+2));

        emit FightEnd(id, f.a, f.b, f.result);
    }


    function lookForFight(uint64 tokenId, uint8 stake) public payable {
        require(stake > 0 && stake < 7, "Invalid stake code");
        KAYO.Fighter memory fa = _fighterContract.getFighterData(tokenId);
        require(fa.owner == msg.sender, "You do not own this fighter");
        require(fa.listed == false , "The fighter is listed on the marketplace");
        require(fa.queue == 0, "The fighter is already in a queue");
        require(fa.booked == 0, "The fighter is already booked");
        require(fa.fight == 0, "The fighter is already in a fight");
        // No stake || Stake Fighter || Custom stake
        if(stake == 1 || stake == 2 || stake == 3) {   
            require(msg.value == fightPrice, "Required fight price not met");
        }
        // Stake Ether values
        else if(stake == 4) {
            require(msg.value == fightPrice + 0.01 ether, "Required fight price + stake not met");
        }
        else if(stake == 5) {
            require(msg.value == fightPrice + 0.1 ether, "Required fight price + stake not met");
        }
        else if(stake == 6) {
            require(msg.value == fightPrice + 1.0 ether, "Required fight price + stake not met");
        }

        _fighterContract.setFightState(tokenId, 0, 0, stake);
        emit LookFight(tokenId, stake);
    }


    function cancelLookForFight(uint64 tokenId) public {
        KAYO.Fighter memory fa = _fighterContract.getFighterData(tokenId);
        require(fa.owner == msg.sender && fa.queue > 0);
        // Unstake Ether values
        if(fa.queue == 4) {    
            Address.sendValue(fa.owner, 0.01 ether);
        }
        else if(fa.queue == 5) {
            Address.sendValue(fa.owner, 0.1 ether);
        }
        else if(fa.queue == 6) {
            Address.sendValue(fa.owner, 1.0 ether);
        }

        _fighterContract.setFightState(tokenId, 0, 0, 0);
        emit Unlook(tokenId,0);
    }
}