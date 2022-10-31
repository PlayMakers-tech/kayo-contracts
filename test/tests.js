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
    const Tournament = await ethers.getContractFactory("Tournament");
    const contractFighter    = await Fighter.deploy();
    const contractFight      = await Fight.deploy();
    const contractStatistics = await Statistics.deploy();
    const contractAbility    = await Ability.deploy();
    const contractTournament = await Tournament.deploy();
    await contractFighter.setFightContractAddress(contractFight.address);
    await contractFighter.setTournamentContractAddress(contractTournament.address);
    await contractFight.setTournamentContractAddress(contractTournament.address);
    await contractStatistics.setFighterContractAddress(contractFighter.address);
    await contractStatistics.setFightContractAddress(contractFight.address);
    await contractAbility.setFighterContractAddress(contractFighter.address);  
    await contractFighter.setStatisticsContract(contractStatistics.address);
    await contractFighter.setAbilityContract(contractAbility.address);
    await contractFight.setFighterContract(contractFighter.address);
    await contractFight.setStatisticsContract(contractStatistics.address);
    await contractTournament.setFighterContract(contractFighter.address);
    await contractTournament.setFightContract(contractFight.address);

    return {contractFighter, contractFight, contractStatistics, contractAbility, owner, addr1, addr2, contractTournament};
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
    await expect(contractFight.resolveRound(fightId, 3, 1, "ThisIsEncodedMovesFromServer"))
      .to.emit(contractFight, "FightEnd")
      .withArgs(fightId,token1,token2,3);
    
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
    await contractFighter.connect(addr1).listToken(token1, myPrice, {value: listPrice});

    // Buy it and check that the transfer takes place
    await expect(contractFighter.connect(addr2).buyToken(token1, {value: myPrice}))
      .to.emit(contractFighter, "Transfer")
      .withArgs(addr1.address, addr2.address, token1);

    // We should check that the owner is set properly
    // ...
  });

  it("Should prevent transfers", async function () {
    const { contractFighter, contractFight, owner, addr1, addr2 } = await loadFixture(deployFixture);

    // Mint the tokens
    const mintPrice = await contractFighter.getMintPrice();
    const token1 = 1; await contractFighter.connect(addr1).mintNFT({value: mintPrice});

    const fightPrice = await contractFight.getFightPrice();
    await contractFight.connect(addr1).lookForFight(token1, 1, {value:fightPrice});
    // Attempt to transfer
    await expect(contractFighter.connect(addr1).transferFrom(addr1.address, addr2.address, token1))
      .to.be.revertedWith("The fighter is already in a queue");

    await contractFight.connect(addr1).cancelLookForFight(token1);
    // However, authorize transfers if available
    await expect(contractFighter.connect(addr1).transferFrom(addr1.address, addr2.address, token1))
      .to.emit(contractFighter, "Transfer")
      .withArgs(addr1.address, addr2.address, token1);

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

  it("Should handle Level Ups", async function () {
    const { contractFighter, contractStatistics, owner, addr1 } = await loadFixture(deployFixture);
    
    const mintPrice = await contractFighter.getMintPrice();
    const token1 = 1; await contractFighter.connect(addr1).mintNFT({value: mintPrice});

    await expect(contractStatistics.earnXP(token1, 15))
      .to.emit(contractStatistics, "LevelUp")
      .withArgs(token1, 1);
    await expect(contractStatistics.earnXP(token1, 25))
      .to.emit(contractStatistics, "LevelUp")
      .withArgs(token1, 2);

  });

  it("Should handle tournament", async function () {
    const { contractFighter, contractFight, contractTournament, owner, addr1 } = await loadFixture(deployFixture);
    
    const mintPrice = await contractFighter.getMintPrice();
    const tournamentPrice = await contractTournament.getTournamentPrice();
    for(let token=1; token <=4; token++) {
      await contractFighter.connect(addr1).mintNFT({value: mintPrice});
    }

    // Create tournament
    const timestamp = 20221225;
    const tournamentId = 1;
    await expect(contractTournament.createTournament(1,0,timestamp))
      .to.emit(contractTournament, "TournamentNew")
      .withArgs(tournamentId, timestamp);

    // Have the 4 tokens join the tournament
    for(let token=1; token <=4; token++) {
      await expect(contractTournament.connect(addr1).joinTournament(tournamentId, token, {value: tournamentPrice}))
      .to.emit(contractTournament, "TournamentJoin")
      .withArgs(tournamentId, token);
    }

    // Generate the tournament tree
    await contractTournament.generateTree(tournamentId);

    // Compute first phase (2 fights)
    await expect(contractTournament.nextPhase(tournamentId))
      .to.emit(contractFight, "FightNew");
    for(let fight = 1; fight <= 2; fight++) {
      for(let round = 1; round <= 3; round++) {
        await contractFight.resolveRound(fight, round, 1, "tournament");
      }
    }

    // Compute second phase (1 fight)
    await expect(contractTournament.nextPhase(tournamentId))
      .to.emit(contractTournament, "TournamentNextPhase")
      .withArgs(tournamentId,2);
    for(let fight = 3; fight <= 3; fight++) {
      for(let round = 1; round <= 3; round++) {
        await contractFight.resolveRound(fight, round, 1, "tournament");
      }
    }

    // Announce winner
    await expect(contractTournament.nextPhase(tournamentId))
      .to.emit(contractTournament, "TournamentWin");

  });

});
