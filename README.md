[![Travis-CI Build Status](https://travis-ci.org/rte-antares-rpackage/antaresFlowbased.svg?branch=master)](https://travis-ci.org/rte-antares-rpackage/antaresFlowbased)[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/rte-antares-rpackage/antaresFlowbased?branch=master&svg=true)](https://ci.appveyor.com/project/rte-antares-rpackage/antaresFlowbased)[![Coverage Status](https://img.shields.io/codecov/c/github/rte-antares-rpackage/antaresFlowbased/master.svg)](https://codecov.io/github/rte-antares-rpackage/antaresFlowbased?branch=master)

# The 'antaresFlowbased' R package

The `antaresFlowbased` package provides functions that for launch a flowBased study from an existed antares study, which can be finally open and analyse with the **antares** software.

## Installation

You can install the package directly from **Github**. It depends on **antaresRead**, **antaresEditObject** & **antaresProcessing** R package :

https://github.com/rte-antares-rpackage/antaresRead
https://github.com/rte-antares-rpackage/antaresEditObject
https://github.com/rte-antares-rpackage/antaresProcessing


```r
# Install dependencies
install.packages(c("antaresRead","pipeR","data.table","testthat",
 "plyr","ROI","ROI.plugin.clp","rmarkdown","flexdashboard","rAmCharts",
  "manipulateWidget","DT","shiny","slam","antaresProcessing"))
  
install_github("rte-antares-rpackage/antaresEditObject")  
install_github("rte-antares-rpackage/antaresFlowbased")
```

## Use

The simplest way is to look the vignette : 

```r
require(antaresFlowbased)

# available vignette
vignette(package = "antaresFlowbased")

vignette("BeforeAntares")
vignette("AfterAntares")
```
