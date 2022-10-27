// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.9;

import "@openzeppelin/contracts/utils/Address.sol";
import "hardhat/console.sol";
import "./KAYO.sol";
import "./Fighter.sol";
import "./Fight.sol";


contract Tournament {
    uint64 private _tournamentIds;
    address payable private _owner;
    Fighter private _fighterContract;
    Fight   private _fightContract;
    uint256 private tournamentPrice = 0.001 ether;

    mapping(uint64 => KAYO.Tournament) private _tournamentData;

    // Dynamic arrays linked to tournament data
    mapping(uint64 => uint64[]) private _fighters;
    mapping(uint64 => uint64[]) private _fights;
    mapping(uint64 => uint8[]) private _scores;

    event TournamentNew(uint64 id, uint64 start_time);
    event TournamentReady(uint64 id);
    event TournamentJoin(uint64 id, uint64 fighterId);
    event TournamentNextPhase(uint64 id, uint8 phase);
    event TournamentWin(uint64 id, uint64 fighterId);

    constructor() {
        _owner = payable(msg.sender);
    }

    function setTournamentPrice(uint256 value) public {
        require(_owner == msg.sender);
        tournamentPrice = value;
    }
    function setFighterContract(address addr) public {
        require(_owner == msg.sender);
        _fighterContract = Fighter(addr);
    }
    function setFightContract(address addr) public {
        require(_owner == msg.sender);
        _fightContract = Fight(addr);
    }

    function getTournamentAmount() public view returns(uint64) {
        return _tournamentIds;
    }
    function getTournamentPrice() public view returns(uint256) {
        return tournamentPrice;
    }
    function getTournamentData(uint64 id) public view returns (KAYO.Tournament memory) {
        return _tournamentData[id];
    }
    function getFighters(uint64 id) public view returns (uint64[] memory) {
        return _fighters[id];
    }
    function getFights(uint64 id) public view returns (uint64[] memory) {
        return _fights[id];
    }
    function getScores(uint64 id) public view returns (uint8[] memory) {
        return _scores[id];
    }

    function createTournament(uint8 stake, uint256 price, uint64 start_time) public returns (uint64) {
        require(msg.sender == _owner);
        _tournamentData[_tournamentIds] = KAYO.createTournament(++_tournamentIds, stake, price);
        emit TournamentNew(_tournamentIds, start_time);
        return _tournamentIds;
    }

    function joinTournament(uint64 id, uint64 fighterId) public payable {
        KAYO.Tournament storage t = _tournamentData[id];
        KAYO.Fighter memory f = _fighterContract.getFighterData(fighterId);
        require(t.state == 1);
        require(msg.value == tournamentPrice + t.price);
        require(f.owner == msg.sender, "You do not own this fighter");
        require(f.listed == false , "The fighter is listed on the marketplace");
        require(f.queue  == 0, "The fighter is already in a queue");
        require(f.booked == 0, "The fighter is already booked");
        require(f.fight  == 0, "The fighter is already in a fight");
        Address.sendValue(_owner, tournamentPrice);
        _fighterContract.setFightState(fighterId, 0, id, 0);
        _fighters[id].push(fighterId);
        emit TournamentJoin(id, fighterId);
  }

  function generateTree(uint64 id) public {
    require(_owner == msg.sender);
    KAYO.Tournament storage t = _tournamentData[id];
    require(t.state == 1);
    // We assume we must have a power of 2 amount of fighters here
    for(uint64 i=0; i < _fighters[id].length;i++) {
        _scores[id].push(0);
        uint64 tmp = _fighters[id][i];
        uint64 j = uint64(KAYO.random(id+i)) % uint64(_fighters[id].length);
        _fighters[id][i] = _fighters[id][j];
        _fighters[id][j] = tmp;
    }
    t.state = 2;
    emit TournamentReady(id);
  }

  function nextPhase(uint64 id) public {
    require(_owner == msg.sender);
    KAYO.Tournament storage t = _tournamentData[id];
    require(t.state == 2);
    // Gather results of the previous phase
    while(t.fight_ptr < _fights[id].length) {
        KAYO.Fight memory f = _fightContract.getFightData(_fights[id][t.fight_ptr]);        
        require(f.state == 2, "A fight of the previous phase is not complete yet");
        t.fight_ptr++;
        for(uint64 i=0; i < _fighters[id].length;i++) {
            if(f.result>0 ? (f.a == _fighters[id][i]) : (f.b == _fighters[id][i])) {
                _scores[id][i]++;
            }
        }
    }
    // Schedule the fights of the next phase
    uint64 fa = 0;
    bool no_more_fight = true;
    for(uint64 i=0; i < _fighters[id].length;i++) {
        if(_scores[id][i] == t.phase) {
            if(fa==0) {
                fa = _fighters[id][i];
            }
            else {
                _fights[id].push(_fightContract.createFight(fa,_fighters[id][i],3,3,0));
                fa = 0;
                no_more_fight = false;
            }
        }
    }
    t.phase++;
    // If no fights were scheduled, compute the victory and release fighters
    if(no_more_fight) {
        // Winner is fa
        KAYO.Fighter memory fwin = _fighterContract.getFighterData(fa);
        Address.sendValue(fwin.owner, _fighters[id].length * t.price);
        t.state = 3;
        emit TournamentWin(id, fa);
        for(uint64 i=0; i < _fighters[id].length;i++) {
            _fighterContract.setFightState(_fighters[id][i], 0, 0, 0); // Disable the booked flag
        }
    }
    else {
        emit TournamentNextPhase(id, t.phase);
    }
  }
}