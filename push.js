const fs = require('fs');
const { spawn, exec, execSync  } = require('child_process');
const path = require('path');
const process = require('process');
const readline = require('readline').createInterface({
  input: process.stdin,
  output: process.stdout
});

function parseVersion(str) {
	const parts = str.split('.').map((a) => parseInt(a));
	
	if(parts.length > 1) {
		return parts;
	}
	else {
		return null;
	}
}

function getCurrentVersion() {
	return parseVersion(execSync("git describe --abbrev=0 --tags", { encoding: 'utf8' }).toString().trim());
}

function nextMinorVersion(v) {
	const res = v.slice(0, v.length - 1);
	res.push(v[v.length-1] + 1);
	return res;
}
function nextMajorVersion(v) {
	const res = v.slice(0, v.length - 2);
	res.push(v[v.length-2] + 1);
	res.push(0);
	return res;
}

function versionString(v) {
	return v.join(".");
}

const outputDirectory = path.join(__dirname, "dist");

function getPackCmd (v) {
	const args =
		[
			"/p:Version=" + versionString(v),
			"/p:AssemblyVersion=" + versionString(v),
			"/p:Authors=Aardworx",
			"/p:Copyright=Aardworx",
			"/p:RepositoryUrl=https://github.com/aardworx/aardvark.web",
			"/p:PackageLicenseExpression=AGPL-3.0-only",
			"-o \"" + outputDirectory + "\"",
			"-c Release",
		];
	return "dotnet pack " + args.join(" ");
}

const currentVersion = getCurrentVersion();

let newVersion = currentVersion;
if (process.argv.length > 2) {
	if (process.argv[2].toLowerCase() == "minor") newVersion = nextMinorVersion(currentVersion);
	else if (process.argv[2].toLowerCase() == "major") newVersion = nextMajorVersion(currentVersion);
}	

function getUserHome() {
  return process.env[(process.platform == 'win32') ? 'USERPROFILE' : 'HOME'];
}

var deleteFolderRecursive = function(path) {
  if (fs.existsSync(path)) {
    fs.readdirSync(path).forEach(function(file, index){
      var curPath = path + "/" + file;
      if (fs.lstatSync(curPath).isDirectory()) { // recurse
        deleteFolderRecursive(curPath);
      } else { // delete file
        fs.unlinkSync(curPath);
      }
    });
    fs.rmdirSync(path);
  }
};

readline.question("\x1b[33mcreate packages with version " + versionString(newVersion) + "? [Y|n]\x1b[0m", (r) => {
	
	if (r.toLowerCase() == "n") {
		process.exit(0);
	}
	
	deleteFolderRecursive(path.join(__dirname, "dist"));
	const packCmd = getPackCmd(newVersion);
	console.log("\x1b[33mcreating packages\x1b[0m");
	execSync(packCmd, { stdio: 'inherit' });
	
	
	fs.readdir(outputDirectory, function (err, files) {
		if(err) {
			console.log("\x1b[31munable to scan directory: " + err + "\x1b[0m");
			process.exit(1);
		}
		
		const key = fs.readFileSync(path.join(getUserHome(), ".ssh", "aardworx.key"));
		console.log("using key: \x1b[33m" + key + "\x1b[0m");
		const paket = path.join(__dirname, ".paket", "paket.exe");
		files
			.filter((f) => path.extname(f).toLowerCase() == ".nupkg")
			.map((f) => path.join(__dirname, "dist", f))
			.forEach((f) => {
				console.log("\x1b[31m" + f + "\x1b[0m");
				
				
				execSync(paket + " push --api-key " + key + " \"" + f + "\""); 
			});
				
	
		process.exit(0);
		
	});
		
});

//
//console.log("\x1b[33mcreate packages with version " + versionString(newVersion) + "? [Y|n]\x1b[0m");
//
//
//const packCmd = getPackCmd(newVersion);
//console.log("\x1b[33mcreating packages\x1b[0m");
//execSync(packCmd, { stdio: 'inherit' });






