# The 'antaresFlowbased' R package

The `antaresFlowbased` package provides functions that for launch a flowBased study from an existed antares study, which can be finally open and analyse with the **antares** software.

## Installation

You can install the package directly from **Github**. It depends on **antaresRead** R package :

https://github.com/rte-antares-rpackage/antaresRead

```r
# Install dependencies
install.packages(c("devtools", "data.table", "plyr", "pipeR", "digest"))
install_github("rte-antares-rpackage/antaresFlowbased")
```

## Use

The simplest way is to look the vignette : 

```r
require(antaresFlowbased)
vignette("English")
vignette("FranCais")
```
