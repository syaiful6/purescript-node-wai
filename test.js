var tes = require('./output/Test.Main')

console.log(process.memoryUsage());
gc();
console.log(process.memoryUsage());
tes.main()
console.log(process.memoryUsage());
