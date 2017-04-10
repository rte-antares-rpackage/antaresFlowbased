# Le package R 'antaresFlowbased'


le package R `antaresFlowbased` permet de lancer une étude flowBased depuis R à partir d'une étude existante, pour finalement l'ouvir et l'analyser à partir du logiciel **antares**

## Installation

Le package peut s'installer directement depuis **github**. Il dépend également du package **antaresRead**. Instruction d'installation ici : 

https://github.com/rte-antares-rpackage/antaresRead

```r
# Install dependencies
install.packages(c("devtools", "data.table", "plyr", "pipeR", "digest"))
install_github("rte-antares-rpackage/antaresFlowbased")
```

## Utilisation

Le plus simple est d'aller voir la vignette après installation : 

```r
require(antaresFlowbased)
vignette("Use_antaresFlowbased")
```
