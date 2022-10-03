const { expect } = require("chai");
const hre = require("hardhat");

describe("Fight", function () {
  it("Should deploy contract and do stuff", async function () {
    const [owner, ADDR1, ADDR2] = await ethers.getSigners();

    const Fighter         = await hre.ethers.getContractFactory("Fighter");
    const contractFighter = await Fighter.deploy();
    const Fight           = await hre.ethers.getContractFactory("Fight");
    const contractFight   = await Fight.deploy(contractFighter.address);
    await contractFighter.setFightContractAddress(contractFight.address);

    const SOURCE = "0x0000000000000000000000000000000000000000";
    const WALL1  = ADDR1.address;
    const WALL2  = ADDR2.address;

    const mintPrice    = await contractFighter.getMintPrice();
    const fusionPrice  = await contractFighter.getFusionPrice();
    const listingPrice = await contractFighter.getListPrice();
    const fightingPrice = await contractFight.getFightPrice();

    await expect(contractFighter.connect(ADDR1).mintNFT({value: mintPrice}))
      .to.emit(contractFighter, "Transfer")
      .withArgs(SOURCE,WALL1,1);

    await expect(contractFighter.connect(ADDR2).mintNFT({value: mintPrice}))
      .to.emit(contractFighter, "Transfer")
      .withArgs(SOURCE,WALL2,2);

    await expect(contractFighter.connect(ADDR2).mintNFT({value: mintPrice}))
      .to.emit(contractFighter, "Transfer")
      .withArgs(SOURCE,WALL2,3);

    const myPrice = 10000;
    /*await contractFighter.connect(ADDR2).listToken(3, myPrice, {value: listingPrice});
    await contractFighter.connect(ADDR1).buyToken(3, {value: myPrice});
    await contractFighter.connect(ADDR1).fusion(1, 3, {value: fusionPrice});*/

    await contractFight.connect(ADDR1).lookForFight(1, 1, {value:parseInt(fightingPrice)});
    await contractFight.connect(ADDR2).lookForFight(2, 1, {value:parseInt(fightingPrice)});
    await contractFight.createFight(1, 2, 1, 0, 0.0);
    await contractFight.resolveRound(1, 1, 1, "ThisIsEncodedMovesFromServer");

    //console.log(await contractFighter.getFighterData(1));
    console.log(await contractFight.getFightData(1));
    console.log(await contractFight.getRoundData(1));
    //console.log(await contractFighter.getFighterData(2));
    //console.log(await contractFighter.getData(2));
    //console.log(await contractFighter.getData(3));

    // assert that the value is correct
    //expect(await contractFighter.owner()).to.equal(1);
    //expect(await contractFighter.getCounter()).to.equal(1);
    //expect(await contractFighter.connect(ADDR1).get17({value: mintPrice})).to.equal(17);
  });
});
