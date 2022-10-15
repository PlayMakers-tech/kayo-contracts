// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.9;

import "@openzeppelin/contracts/utils/Address.sol";
import "hardhat/console.sol";
import "./KAYO.sol";


contract Ability {
    address payable private _owner;
    address private _fighterContractAddress;

    // Abilities of a fighter given its id
    mapping(uint64 => uint16[]) private _abilityData;

    // Data about the ability itself
    mapping(uint16 => uint64) private _abilityCount;
    mapping(uint16 => uint8)  private _abilityRarity;
    uint16 _maxAbility = 0;

    // Keep track of what's available
    mapping(uint8 => uint16[]) private _availablePerRarity;
    mapping(uint8 => uint64)   private _probaRarity;

    constructor() {
        _owner = payable(msg.sender);
        // Initial Ability rates and fill up
        _probaRarity[2] = 20;
        _probaRarity[3] = 4000;
        for(int i=0; i<10; i++) addAbility(1);
        for(int i=0; i< 4; i++) addAbility(2);
        for(int i=0; i< 2; i++) addAbility(3);
    }

    function setFighterContractAddress(address addr) public {
        require(_owner == msg.sender);
        _fighterContractAddress = addr;
    }
    function setProbaRarity(uint8 rarity, uint64 proba) public {
        require(_owner == msg.sender);
        require(rarity>1, "Only rarity>=2 can have a proba");
        require(proba>1, "Proba needs to be a integer >=2");
        _probaRarity[rarity] = proba;
    }
    function getProbaRarity(uint8 rarity) public view returns (uint64) {
        return _probaRarity[rarity];
    }

    function getAbilities(uint64 id) public view returns (uint16[] memory) {
        return _abilityData[id];
    }

    function mintNFT(uint64 id) public {
        require(_fighterContractAddress == msg.sender);
        for(uint i=0; i<6; i++) {
            learnAbility(id,randAbility(id,i*1337));
        }
    }
    function fusion(uint64 id, uint64 father, uint64 mother) public {
        require(_fighterContractAddress == msg.sender);
        // TODO Will have to consider the internal functions below
        KAYO.createAbilityFusion(_abilityData[id], _abilityData[father], _abilityData[mother]);
    }


    // Interact with the abilities themselves
    function addAbility(uint8 rarity) public returns (uint16) {
        require(_owner == msg.sender);
        _maxAbility++;
        _abilityCount[_maxAbility] = 0;
        _abilityRarity[_maxAbility] = rarity;
        _availablePerRarity[rarity].push(_maxAbility);
        return _maxAbility;
    }
    function getAbilityCount(uint16 aid) public view returns (uint64) {
        return _abilityCount[aid];
    }
    function getAbilityRarity(uint16 aid) public view returns (uint8) {
        return _abilityRarity[aid];
    }

    // Internal functions    
    function learnAbility(uint64 id, uint16 aid) internal {
        uint8 rarity = _abilityRarity[aid];
        require(rarity==1 || (rarity==2 && _abilityCount[aid]<5) || (rarity==3 && _abilityCount[aid]<1), "Rarity overload");
        for(uint i=0; i < _abilityData[id].length;i++) {
            require(_abilityData[id][i] != aid, "Can't grant a known ability");
        }
        _abilityData[id].push(aid);
        _abilityCount[aid]++;
        // Remove from the availability list if we reach the quota for the rarity
        if(rarity==2 && _abilityCount[aid]==5 || rarity==3) {
            uint l = _availablePerRarity[rarity].length;
            for(uint i=0; i < l-1;i++) {
                if(_availablePerRarity[rarity][i] == aid) {
                    _availablePerRarity[rarity][i] = _availablePerRarity[rarity][l-1];
                    break;
                }
            }
            _availablePerRarity[rarity].pop();
        }
    }

    function forgetAbility(uint64 id, uint16 aid) internal {
        uint l = _abilityData[id].length;
        bool found = false;
        for(uint i=0; i < l;i++) {
            if(_abilityData[id][i] == aid) {
                _abilityData[id][i] = _abilityData[id][l-1];
                found = true;
                break;
            }
        }
        require(found, "Trying to forget an unknown ability");
        _abilityData[id].pop();
        uint8 rarity = _abilityRarity[aid];
        if(rarity==2 && _abilityCount[aid]==5 || rarity==3) {
            _availablePerRarity[rarity].push(aid);
        }
    }

    function randAbility(uint64 id, uint r) internal view returns (uint16 aid) {
        r = KAYO.random(id+uint64(r));
        uint l;
        for(uint8 rarity=3; rarity>1; rarity--) {
            l = _availablePerRarity[rarity].length;
            if(l>0 && r%_probaRarity[rarity]==0) {
                r = r/_probaRarity[rarity];
                aid = _availablePerRarity[rarity][r%l];
                r = r/l;
                for(uint16 j=0; j<_abilityData[id].length;j++) {
                    if(_abilityData[id][j]==aid) {
                        aid=0;
                        break;
                    }
                }
                break;          
            }
            r = r/_probaRarity[rarity];
        }
        l = _availablePerRarity[1].length;
        while(aid==0) {
            require(_abilityData[id].length<l, "This fighter knows all abilities of rarity 1");
            aid = _availablePerRarity[1][r%l];
            r = r/l;
            for(uint16 j=0; j<_abilityData[id].length;j++) {
                if(_abilityData[id][j]==aid) {
                    aid=0;
                    break;
                }
            }
        }
    }
}