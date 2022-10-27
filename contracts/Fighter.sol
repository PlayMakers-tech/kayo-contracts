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
    address private _tournamentContractAddress;
    Statistics private _statisticsContract;
    Ability private _abilityContract;
    uint256 private mintPrice    = 0.02  ether;
    uint256 private fusionPrice  = 0.05  ether;
    uint256 private listPrice = 0.001 ether;

    mapping(uint64 => KAYO.Fighter) private _fighterData;

    event List(uint64 id, uint256 price);
    event Unlist(uint64 id);

    modifier ownerOnly() {
        require(_owner == msg.sender, "You do not have permissions to do this");
        _;
    }

    constructor() ERC721("Fighter", "KAYO") {
        _owner = payable(msg.sender);
    }

    // Override function from OpenZeppelin to prevent transfer if Fighter is being used
    function _beforeTokenTransfer(address, address, uint256 tokenId) internal view override {
        KAYO.Fighter memory f = _fighterData[uint64(tokenId)];
        require(f.listed == false , "The fighter is listed on the marketplace");
        require(f.queue  == 0, "The fighter is already in a queue");
        require(f.booked == 0, "The fighter is already booked");
        require(f.fight  == 0, "The fighter is already in a fight");
    }

    function mintNFT() public payable returns (uint64) {
        require(msg.value == mintPrice, "Required minting price not met");
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
    function setMintPrice(uint256 value) ownerOnly public {
        mintPrice = value;
    }

    function getFusionPrice() public view returns(uint256) {
        return fusionPrice;
    }
    function setFusionPrice(uint256 value) ownerOnly public {
        fusionPrice = value;
    }

    function getListPrice() public view returns(uint256) {
        return listPrice;
    }
    function setListPrice(uint256 value) ownerOnly public {
        listPrice = value;
    }

    function setFightContractAddress(address addr) ownerOnly public {
        _fightContractAddress = addr;
    }
    function setTournamentContractAddress(address addr) ownerOnly public {
        _tournamentContractAddress = addr;
    }
    function setStatisticsContract(address addr) ownerOnly public {
        _statisticsContract = Statistics(addr);
    }
    function setAbilityContract(address addr) ownerOnly public {
        _abilityContract = Ability(addr);
    }

    function fusion(uint64 father, uint64 mother) public payable returns (uint64) {
        require(father != mother, "Two distrinct fighters need to be selected");
        address owner = ownerOf(father);
        require(owner == msg.sender && owner == ownerOf(mother), "You do not have permissions to do this");
        require(msg.value == fusionPrice, "Required fusion price not met");
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
        emit List(tokenId, price);
    }
    function cancelListToken(uint64 tokenId) public payable {
        require(_fighterData[tokenId].owner == msg.sender 
            && _fighterData[tokenId].listed == true);
        _fighterData[tokenId].listed = false;
        emit Unlist(tokenId);
    }
    
    function buyToken(uint64 tokenId) public payable {
        uint256 price = _fighterData[tokenId].price;
        require(_fighterData[tokenId].listed == true && msg.value == price);    
        _fighterData[tokenId].listed = false;
        _transfer(_fighterData[tokenId].owner, msg.sender, tokenId);    
        Address.sendValue(_fighterData[tokenId].owner, price);
        _fighterData[tokenId].owner = payable(msg.sender);
        emit Unlist(tokenId);
    }

    function transferTo(address owner, uint64 tokenId) public {
        require(msg.sender == _fightContractAddress || msg.sender == _tournamentContractAddress);
        _transfer(_fighterData[tokenId].owner, owner, tokenId);
        _fighterData[tokenId].owner = payable(owner);
    }

    function setFightState(uint64 tokenId, uint64 fight, uint64 booked, uint8 queue) public {
        require(msg.sender == _fightContractAddress || msg.sender == _tournamentContractAddress);
        _fighterData[tokenId].fight  = fight;
        _fighterData[tokenId].booked = booked;
        _fighterData[tokenId].queue  = queue;
    }

}