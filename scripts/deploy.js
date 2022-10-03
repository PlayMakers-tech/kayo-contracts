async function main() {
  // Grab the contract factory 
  const Fighter = await ethers.getContractFactory("Fighter");
  const Fight   = await ethers.getContractFactory("Fight");

  // Start deployment, returning a promise that resolves to a contract object
  console.log("Deploying Fighter Contract...")
  const contractFighter = await Fighter.deploy(); // Instance of the contract
  console.log("Deployed Fighter Contract")
  await new Promise(r => setTimeout(r, 10000));
  console.log("Deploying Fight Contract...")
  const contractFight   = await Fight.deploy(contractFighter.address);
  console.log("Deployed Fight Contract")
  await new Promise(r => setTimeout(r, 10000));
  console.log("Configure links...")
  await contractFighter.setFightContractAddress(contractFight.address);
  console.log("Configured links")

  console.log("Fighter Contract deployed to address:", contractFighter.address);
  console.log("Fight   Contract deployed to address:", contractFight.address);
}

main()
    .then(() => process.exit())
    .catch(error => {
        console.error(error);
        process.exit(1);
})