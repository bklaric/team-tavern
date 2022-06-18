var graph = require("./graph.json")

var offendingModules = []

for (const module in graph) {
    if (module.startsWith("TeamTavern.Client") && graph[module].depends.some(dep => dep.startsWith("TeamTavern.Server"))) {
        offendingModules.push(module)
    }
}

console.log(offendingModules)
