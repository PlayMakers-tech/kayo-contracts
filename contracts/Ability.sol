// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.9;

import "@openzeppelin/contracts/utils/Address.sol";
import "hardhat/console.sol";
import "./KAYO.sol";


contract Ability {
    address payable private _owner;
    address private _fighterContractAddress;

    // Abilities of a fighter given its id
    mapping(uint64 => KAYO.Ability) private _abilityData;

    // Data about the ability itself
    mapping(uint16 => uint64) private _abilityCount;
    mapping(uint16 => uint8)  private _abilityRarity;
    uint16 _maxAbility = 0;

    constructor() {
        _owner = payable(msg.sender);
    }

    function setFighterContractAddress(address addr) public {
        require(_owner == msg.sender);
        _fighterContractAddress = addr;
    }

    function getAbilities(uint64 id) public view returns (KAYO.Ability memory) {
        return _abilityData[id];
    }

    function mintNFT(uint64 id) public {
        require(_fighterContractAddress == msg.sender);
        _abilityData[id] = KAYO.createAbilityInit(id);
    }
    function fusion(uint64 id, uint64 father, uint64 mother) public {
        require(_fighterContractAddress == msg.sender);
        _abilityData[id] = KAYO.createAbilityFusion(id, _abilityData[father], _abilityData[mother]);
    }


    // Interact with the abilities themselves
    function addAbility(uint8 rarity) public returns (uint16) {
        require(_owner == msg.sender);
        _maxAbility++;
        _abilityCount[_maxAbility] = 0;
        _abilityRarity[_maxAbility] = rarity;
        return _maxAbility;
    }
    function updateAbility(uint16 id, uint8 rarity) public {
        require(_owner == msg.sender);
        _abilityRarity[id] = rarity;
    }
    function getAbilityCount(uint16 id) public view returns (uint64) {
        return _abilityCount[id];
    }
    function getAbilityRarity(uint16 id) public view returns (uint8) {
        return _abilityRarity[id];
    }
}