const fs = require('fs/promises');

async function main() {

  const contracts = {};
  const contractNames = ["Fighter", "Fight", "Statistics", "Ability", "Tournament"];

  // Deploy the contracts
  for(const contract of contractNames) {
    const factory = await ethers.getContractFactory(contract);
    console.log("Deploying ", contract, " Contract...");
    contracts[contract] = await factory.deploy();
    console.log("Deployed  ", contract, " Contract...");
  }
  // Delay to avoid timing issue
  await new Promise(r => setTimeout(r, 15000));

  // Add links between the contracts
  console.log("Configure links...");
  // Set simple addresses
  await contracts["Fighter"].setFightContractAddress(contracts["Fight"].address);
  await contracts["Fighter"].setTournamentContractAddress(contracts["Tournament"].address);
  await contracts["Fight"].setTournamentContractAddress(contracts["Tournament"].address);
  await contracts["Statistics"].setFighterContractAddress(contracts["Fighter"].address);
  await contracts["Statistics"].setFightContractAddress(contracts["Fight"].address);
  await contracts["Ability"].setFighterContractAddress(contracts["Fighter"].address);  
  // Set contract objects
  await contracts["Fighter"].setStatisticsContract(contracts["Statistics"].address);
  await contracts["Fighter"].setAbilityContract(contracts["Ability"].address);
  await contracts["Fight"].setFighterContract(contracts["Fighter"].address);
  await contracts["Fight"].setStatisticsContract(contracts["Statistics"].address);
  await contracts["Tournament"].setFighterContract(contracts["Fighter"].address);
  await contracts["Tournament"].setFightContract(contracts["Fight"].address);
  console.log("Configured links");

  // Reporting address on screen and in file (to be redistributed to other repositories)
  let str = "";
  for(const contract of contractNames) {
    console.log(contract.padEnd(10), " Contract deployed to address:", contracts[contract].address);
    str += contract + "," + contracts[contract].address + "\n";
  }
  await fs.writeFile("./deployedAddresses.csv", str);
}

main()
    .then(() => process.exit())
    .catch(error => {
        console.error(error);
        process.exit(1);
});