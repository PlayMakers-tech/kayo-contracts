const { expect } = require("chai");
const { loadFixture } = require("@nomicfoundation/hardhat-network-helpers");

describe("Overall test", function () {

  // Deployment of the Contracts
  // More or less a copy/paste of scripts/deploy.js
  async function deployFixture() {
    const [owner, addr1, addr2] = await ethers.getSigners();
    const Fighter    = await ethers.getContractFactory("Fighter");
    const Fight      = await ethers.getContractFactory("Fight");
    const Statistics = await ethers.getContractFactory("Statistics");
    const Ability    = await ethers.getContractFactory("Ability");
    const contractFighter    = await Fighter.deploy();
    const contractFight      = await Fight.deploy();
    const contractStatistics = await Statistics.deploy();
    const contractAbility    = await Ability.deploy();
    await contractFighter.setFightContractAddress(contractFight.address);
    await contractStatistics.setFighterContractAddress(contractFighter.address);
    await contractStatistics.setFightContractAddress(contractFight.address);
    await contractAbility.setFighterContractAddress(contractFighter.address);  
    await contractFighter.setStatisticsContract(contractStatistics.address);
    await contractFighter.setAbilityContract(contractAbility.address);
    await contractFight.setFighterContract(contractFighter.address);
    await contractFight.setStatisticsContract(contractStatistics.address);

    return {contractFighter, contractFight, contractStatistics, contractAbility, owner, addr1, addr2};
  }


  it("Should deploy contracts", async function () {
    await loadFixture(deployFixture);
  });

  it("Should mint", async function () {
    const { contractFighter, owner, addr1, addr2 } = await loadFixture(deployFixture);

    const mintPrice = await contractFighter.getMintPrice();

    await expect(contractFighter.connect(addr1).mintNFT({value: mintPrice}))
      .to.emit(contractFighter, "Transfer")
      .withArgs("0x0000000000000000000000000000000000000000", addr1.address, 1);

    await expect(contractFighter.connect(addr2).mintNFT({value: mintPrice}))
      .to.emit(contractFighter, "Transfer")
      .withArgs("0x0000000000000000000000000000000000000000", addr2.address, 2);

    await expect(contractFighter.connect(addr2).mintNFT({value: mintPrice}))
      .to.emit(contractFighter, "Transfer")
      .withArgs("0x0000000000000000000000000000000000000000", addr2.address, 3);
  });


  it("Should get fighter data", async function () {
    const { contractFighter, contractStatistics, contractAbility, owner, addr1, addr2 } = await loadFixture(deployFixture);

    const mintPrice = await contractFighter.getMintPrice();
    const token1 = 1; await contractFighter.connect(addr1).mintNFT({value: mintPrice});

    await contractFighter.connect(addr2).getFighterData(token1);
    await contractStatistics.connect(addr2).getStatistics(token1);
    await contractAbility.connect(addr2).getAbilities(token1);

  });

  it("Should fight", async function () {
    const { contractFighter, contractFight, contractStatistics, owner, addr1, addr2 } = await loadFixture(deployFixture);

    // Mint the tokens
    const mintPrice = await contractFighter.getMintPrice();
    const token1 = 1; await contractFighter.connect(addr1).mintNFT({value: mintPrice});
    const token2 = 2; await contractFighter.connect(addr2).mintNFT({value: mintPrice});

    // Have both of them open for matching
    const fightPrice = await contractFight.getFightPrice();
    await contractFight.connect(addr1).lookForFight(token1, 1, {value:fightPrice});
    await contractFight.connect(addr2).lookForFight(token2, 1, {value:fightPrice});

    // Match them
    const fightId = 1; await contractFight.createFight(token1, token2, 3, 0, 0.0);

    // Resolve the fight
    await contractFight.resolveRound(fightId, 1, 1, "ThisIsEncodedMovesFromServer");
    await contractFight.resolveRound(fightId, 2, 1, "ThisIsEncodedMovesFromServer");
    await contractFight.resolveRound(fightId, 3, 1, "ThisIsEncodedMovesFromServer");
    
    // Could check that the fight went through successfully
    //console.log(await contractFight.getFightData(fightId));
    //console.log(await contractFight.getRoundData(1));

    // Could check that the new XP is non zero
    //console.log(await contractStatistics.getStatistics(token1));

  });

  it("Should trade", async function () {
    const { contractFighter, owner, addr1, addr2 } = await loadFixture(deployFixture);

    // Mint the tokens
    const mintPrice = await contractFighter.getMintPrice();
    const token1 = 1; await contractFighter.connect(addr1).mintNFT({value: mintPrice});

    // List the token on the marketplace
    const myPrice = 1000000;
    const listPrice = await contractFighter.getListPrice();
    await expect(contractFighter.connect(addr1).listToken(token1, myPrice, {value: listPrice}))
      .to.emit(contractFighter, "Transfer")
      .withArgs(addr1.address, contractFighter.address, token1);

    // Buy it and check that the transfer takes place
    await expect(contractFighter.connect(addr2).buyToken(token1, {value: myPrice}))
      .to.emit(contractFighter, "Transfer")
      .withArgs(contractFighter.address, addr2.address, token1);

    // We should check that the owner is set properly
    // ...
  });

  it("Should fuse", async function () {
    const { contractFighter, owner, addr1 } = await loadFixture(deployFixture);

    // Mint the tokens
    const mintPrice = await contractFighter.getMintPrice();
    const token1 = 1; await contractFighter.connect(addr1).mintNFT({value: mintPrice});
    const token2 = 2; await contractFighter.connect(addr1).mintNFT({value: mintPrice});

    // Fuse the tokens
    const fusionPrice = await contractFighter.getFusionPrice();    
    await expect(contractFighter.connect(addr1).fusion(token1, token2, {value: fusionPrice}))
      .to.emit(contractFighter, "Transfer")
      .withArgs("0x0000000000000000000000000000000000000000", addr1.address, token2+1);

    // Should check that parents are burnt, and so on.

  });

  it("Should have proper abilities", async function () {
    const { contractFighter, contractAbility, owner, addr1 } = await loadFixture(deployFixture);

    const mintPrice = await contractFighter.getMintPrice();
    for(let token=1; token <=1; token++) {
      await contractFighter.connect(addr1).mintNFT({value: mintPrice});
      //console.log(await contractAbility.getAbilities(token));
    }

  });

});
