// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.9;

import "@openzeppelin/contracts/token/ERC721/ERC721.sol";
import "@openzeppelin/contracts/utils/Address.sol";
import "hardhat/console.sol";
import "./KAYO.sol";
import "./Statistics.sol";
import "./Ability.sol";


contract Fighter is ERC721 {
    uint64 private _tokenIds;
    address payable private _owner;
    address private _fightContractAddress;
    Statistics private _statisticsContract;
    Ability private _abilityContract;
    uint256 private mintPrice    = 0.02  ether;
    uint256 private fusionPrice  = 0.05  ether;
    uint256 private listPrice = 0.001 ether;

    mapping(uint64 => KAYO.Fighter) private _fighterData;

    event List(uint64 id, uint256 price);
    event Unlist(uint64 id);

    constructor() ERC721("Fighter", "KAYO") {
        _owner = payable(msg.sender);
    }

    function mintNFT() public payable returns (uint64) {
        require(msg.value == mintPrice);
        _safeMint(msg.sender, ++_tokenIds);
        _fighterData[_tokenIds] = KAYO.createFighterInit(_tokenIds, msg.sender);
        _statisticsContract.mintNFT(_tokenIds);
        _abilityContract.mintNFT(_tokenIds);
        Address.sendValue(_owner, mintPrice);
        return _tokenIds;
    }

    function getTokenAmount() public view returns(uint64) {
        return _tokenIds;
    }

    function getMintPrice() public view returns(uint256) {
        return mintPrice;
    }
    function setMintPrice(uint256 value) public {
        require(_owner == msg.sender);
        mintPrice = value;
    }

    function getFusionPrice() public view returns(uint256) {
        return fusionPrice;
    }
    function setFusionPrice(uint256 value) public {
        require(_owner == msg.sender);
        fusionPrice = value;
    }

    function getListPrice() public view returns(uint256) {
        return listPrice;
    }
    function setListPrice(uint256 value) public {
        require(_owner == msg.sender);
        listPrice = value;
    }

    function setFightContractAddress(address addr) public {
        require(_owner == msg.sender);
        _fightContractAddress = addr;
    }
    function setStatisticsContract(address addr) public {
        require(_owner == msg.sender);
        _statisticsContract = Statistics(addr);
    }
    function setAbilityContract(address addr) public {
        require(_owner == msg.sender);
        _abilityContract = Ability(addr);
    }

    function fusion(uint64 father, uint64 mother) public payable returns (uint64) {
        require(father != mother);
        address owner = ownerOf(father);
        require(owner == msg.sender && owner == ownerOf(mother));
        require(msg.value == fusionPrice);
        _safeMint(owner, ++_tokenIds);
        _fighterData[_tokenIds] = KAYO.createFighterFusion(_tokenIds, owner, _fighterData[father], _fighterData[mother]);    
        _statisticsContract.fusion(_tokenIds, father, mother);
        _abilityContract.fusion(_tokenIds, father, mother);
        _burn(father);
        _burn(mother);
        _fighterData[father].owner = payable(address(this));
        _fighterData[mother].owner = payable(address(this));
        Address.sendValue(_owner, fusionPrice);
        return _tokenIds;
    }

    function getFighterData(uint64 id) public view returns (KAYO.Fighter memory) {
        return _fighterData[id];
    }

    function listToken(uint64 tokenId, uint256 price) public payable {
        require(_fighterData[tokenId].owner == msg.sender 
            && _fighterData[tokenId].listed == false 
            && _fighterData[tokenId].queue == 0 
            && _fighterData[tokenId].booked == 0 
            && _fighterData[tokenId].fight == 0 
            && msg.value == listPrice);
        _fighterData[tokenId].listed = true;
        _fighterData[tokenId].price = price;
        _transfer(msg.sender, address(this), tokenId);
        emit List(tokenId, price);
    }
    function cancelListToken(uint64 tokenId) public payable {
        require(_fighterData[tokenId].owner == msg.sender 
            && _fighterData[tokenId].listed == true);
        _fighterData[tokenId].listed = false;
        _transfer(address(this), msg.sender, tokenId);
        emit Unlist(tokenId);
    }
    
    function buyToken(uint64 tokenId) public payable {
        uint256 price = _fighterData[tokenId].price;
        require(_fighterData[tokenId].listed == true && msg.value == price);    
        _fighterData[tokenId].listed = false;
        _transfer(address(this), msg.sender, tokenId);    
        Address.sendValue(_fighterData[tokenId].owner, price);
        _fighterData[tokenId].owner = payable(msg.sender);
        emit Unlist(tokenId);
    }

    function transferForFight(bool goBack, address owner, uint64 tokenId) public {
        require(msg.sender == _fightContractAddress);
        if(goBack) {
            _transfer(address(this), owner, tokenId);
            _fighterData[tokenId].owner = payable(owner);
        }
        else {
            _transfer(owner, address(this), tokenId);
        }
    }
    function setFightState(uint64 tokenId, uint64 fight, uint64 booked, uint8 queue) public {
        require(msg.sender == _fightContractAddress);
        _fighterData[tokenId].fight  = fight;
        _fighterData[tokenId].booked = booked;
        _fighterData[tokenId].queue  = queue;
    }

}