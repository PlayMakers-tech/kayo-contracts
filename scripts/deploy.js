async function main() {
  // Grab the contract factory 
  const Fighter    = await ethers.getContractFactory("Fighter");
  const Fight      = await ethers.getContractFactory("Fight");
  const Statistics = await ethers.getContractFactory("Statistics");
  const Ability    = await ethers.getContractFactory("Ability");
  const Tournament = await ethers.getContractFactory("Tournament");

  // Start deployment, returning a promise that resolves to a contract object
  console.log("Deploying Fighter Contract...");
  const contractFighter = await Fighter.deploy(); // Instance of the contract
  console.log("Deployed Fighter Contract");
  console.log("Deploying Fight Contract...");
  const contractFight   = await Fight.deploy();
  console.log("Deployed Fight Contract");
  console.log("Deploying Statistics Contract...");
  const contractStatistics = await Statistics.deploy();
  console.log("Deployed Statistics Contract");
  console.log("Deploying Ability Contract...");
  const contractAbility = await Ability.deploy();
  console.log("Deployed Ability Contract");
  console.log("Deploying Tournament Contract...");
  const contractTournament = await Tournament.deploy();
  console.log("Deployed Tournament Contract");
  // Delay to avoid timing issue
  await new Promise(r => setTimeout(r, 15000));
  console.log("Configure links...");
  // Set simple addresses
  await contractFighter.setFightContractAddress(contractFight.address);
  await contractFighter.setTournamentContractAddress(contractTournament.address);
  await contractFight.setTournamentContractAddress(contractTournament.address);
  await contractStatistics.setFighterContractAddress(contractFighter.address);
  await contractStatistics.setFightContractAddress(contractFight.address);
  await contractAbility.setFighterContractAddress(contractFighter.address);  
  // Set contract objects
  await contractFighter.setStatisticsContract(contractStatistics.address);
  await contractFighter.setAbilityContract(contractAbility.address);
  await contractFight.setFighterContract(contractFighter.address);
  await contractFight.setStatisticsContract(contractStatistics.address);
  await contractTournament.setFighterContract(contractFighter.address);
  await contractTournament.setFightContract(contractFight.address);
  console.log("Configured links");

  console.log("Fighter    Contract deployed to address:", contractFighter.address);
  console.log("Fight      Contract deployed to address:", contractFight.address);
  console.log("Statistics Contract deployed to address:", contractStatistics.address);
  console.log("Ability    Contract deployed to address:", contractAbility.address);
  console.log("Tournament Contract deployed to address:", contractTournament.address);
}

main()
    .then(() => process.exit())
    .catch(error => {
        console.error(error);
        process.exit(1);
})