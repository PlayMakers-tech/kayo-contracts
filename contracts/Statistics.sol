// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.9;

import "@openzeppelin/contracts/utils/Address.sol";
import "hardhat/console.sol";
import "./KAYO.sol";


contract Statistics {
    address payable private _owner;
    address private _fighterContractAddress;
    address private _fightContractAddress;

    mapping(uint64 => KAYO.Statistics) private _statsData;

    event LevelUp(uint64 id, uint16 lvl);

    constructor() {
        _owner = payable(msg.sender);
    }

    function setFighterContractAddress(address addr) public {
        require(_owner == msg.sender);
        _fighterContractAddress = addr;
    }

    function setFightContract(address addr) public {
        require(_owner == msg.sender);
        _fightContractAddress = addr;
    }

    function getStatistics(uint64 id) public view returns (KAYO.Statistics memory) {
        return _statsData[id];
    }

    function earnXP(uint64 id, uint64 xp) public returns (uint16) {
        require( _owner == msg.sender
            || _fighterContractAddress == msg.sender
            || _fightContractAddress == msg.sender);
        uint16 lvl = KAYO.getLevelFromXP(_statsData[id].xp);
        uint16 lvlup = KAYO.getLevelFromXP(_statsData[id].xp + xp) - lvl;
        for(uint16 i=1; i <= lvlup; i++) {
            emit LevelUp(id, lvl+i);
            _statsData[id] = KAYO.levelUpStatistics(_statsData[id]);            
        }
        _statsData[id].xp += xp;
        return lvlup;
    }

    function mintNFT(uint64 id) public {
        require(_fighterContractAddress == msg.sender);
        _statsData[id] = KAYO.createStatisticsInit(id);
    }
    function fusion(uint64 id, uint64 father, uint64 mother) public {
        require(_fighterContractAddress == msg.sender);
        _statsData[id] = KAYO.createStatisticsFusion(id, _statsData[father], _statsData[mother]);
    }

}