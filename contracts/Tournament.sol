// SPDX-License-Identifier: UNLICENSED
/*pragma solidity ^0.8.9;

import "@openzeppelin/contracts/utils/Address.sol";
import "hardhat/console.sol";
import "./KAYO.sol";
import "./Fighter.sol";
import "./Fight.sol";


contract Tournament {
    uint64 private _tournamentIds;
    address payable private _owner;
    Fighter private _fighterContract;
    Fight   private _fighterContract;
    uint256 private tournamentPrice = 0.001 ether;

    mapping(uint64 => KAYO.Tournament)   private _tournamentData;

    event FightNew(uint64 id, uint64 ida, uint64 idb);
    event FightStart(uint64 id, uint64 ida, uint64 idb);
    event FightEnd(uint64 id, uint64 ida, uint64 idb, uint8 result);

    constructor(address fighterContractAddress) public {
        _owner = payable(msg.sender);
        setFighterContract(fighterContractAddress);
    }

    function getTournamentAmount() public view returns(uint64) {
        return _tournamentIds;
    }

    function getTournamentPrice() public view returns(uint256) {
        return tournamentPrice;
    }
    function setTournamentPrice(uint256 value) public {
        require(_owner == msg.sender);
        tournamentPrice = value;
    }
    function setFighterContract(address addr) public {
        require(_owner == msg.sender);
        _fighterContract = Fighter(addr);
    }

    function getTournamentData(uint64 id) public view returns (KAYO.Tournament memory) {
        return _tournamentData[id];
    }

    function createTournament(uint8 roundCnt, uint8 stake, uint256 price) public returns (uint64) {
        require(msg.sender == _owner);
        _tournamentData[_tournamentIds] = KAYO.createTournament(++_tournamentIds, roundCnt, stake, price);
        return _tournamentIds;
    }

    function joinTournament(uint64 id, uint64 fighterId) public payable {
        KAYO.Tournament storage t = _tournamentData[id];
        KAYO.Fighter memory fighter = _fighterContract.getFighterData(fighterId);
        require(t.state == 1);
        require(msg.value == tournamentPrice + t.price);
        require(msg.sender == fighter.owner);
        Address.sendValue(_owner, fightingPrice);
        _fighterContract.transferForFight(false, msg.sender, fa.id);
        f.ownera = true;
        _fighterContract.setFighting(f.a, true);
  }

    function cancelFight(uint64 id) public payable {
        KAYO.Fight storage f = _fightData[id];
        KAYO.Fighter memory fa = _fighterContract.getFighterData(f.a);
        KAYO.Fighter memory fb = _fighterContract.getFighterData(f.b);
        require(f.state == 1);
        require(msg.sender == fa.owner || msg.sender == fb.owner);
        f.state = 0;
        if(f.ownera) {
            _fighterContract.transferForFight(true, fa.owner, fa.id);
            Address.sendValue(fa.owner, fightingPrice + f.price);
            _fighterContract.setFighting(f.a, false);
        }
        if(f.ownerb) {
            _fighterContract.transferForFight(true, fb.owner, fb.id);
            Address.sendValue(fb.owner, fightingPrice + f.price);
            _fighterContract.setFighting(f.b, false);
        }

        _fighterContract.removeFromQueue(f.a, f.id);
        _fighterContract.removeFromQueue(f.b, f.id);
  }

  function resolveFight(uint64 id, uint8 result, uint256 data) public payable {
        KAYO.Fight storage f = _fightData[id];
        KAYO.Fighter memory fa = _fighterContract.getFighterData(f.a);
        KAYO.Fighter memory fb = _fighterContract.getFighterData(f.b);
        require(f.state == 2);
        require(msg.sender == _owner); // Could be _server instead
        _fighterContract.setFighting(f.a, false);
        _fighterContract.setFighting(f.b, false);
        _fighterContract.removeFromQueue(f.a, f.id);
        _fighterContract.removeFromQueue(f.b, f.id);
        f.result = result;
        f.data = data;
        f.state = 3;
        if(f.stake == 1) {
            // Money case
            if(result == 1) {
                Address.sendValue(fa.owner, 2*f.price);
            }
            else if(result == 2) {
                Address.sendValue(fa.owner, f.price);
                Address.sendValue(fb.owner, f.price);
            }
            else if(result == 3) {
                Address.sendValue(fb.owner, 2*f.price);
            }
        }
        // Return the fighters
        if(f.stake == 2) {
            if(result == 1) {
                _fighterContract.transferForFight(true, fa.owner, fa.id);
                _fighterContract.transferForFight(true, fa.owner, fb.id);
            }
            if(result == 2) {
                _fighterContract.transferForFight(true, fa.owner, fa.id);
                _fighterContract.transferForFight(true, fb.owner, fb.id);
            }
            else if(result == 3) {
                _fighterContract.transferForFight(true, fb.owner, fa.id);
                _fighterContract.transferForFight(true, fb.owner, fb.id);
            }
        }
        else {
            _fighterContract.transferForFight(true, fa.owner, fa.id);
            _fighterContract.transferForFight(true, fb.owner, fb.id);
        }

        emit FightEnd(id, f.a, f.b, result);
    }
}*/