CSIndicators
===============

#### Sectoral Indicators for Climate Services Based on Sub-Seasonal to Decadal Climate Predictions

## Description
Set of generalised tools for the flexible computation of climate related 
indicators defined by the user. Each method represents a specific mathematical 
approach which is combined with the possibility to select an arbitrary time 
period to define the indicator. This enables a wide range of possibilities to 
tailor the most suitable indicator for each particular climate service 
application (agriculture, food security, energy, water management…). This package 
is intended for sub-seasonal, seasonal and decadal climate predictions, but its 
methods are also applicable to other time-scales, provided the dimensional 
structure of the input is maintained. Additionally, the outputs of the functions 
in this package are compatible with CSTools.

## Functions and documentation

To learn how to use the package see:

- [**Agricultural Indicators**](https://CRAN.R-project.org/package=CSIndicators/vignettes/AgriculturalIndicators.html)
- [**Wind Energy Indicators**](https://CRAN.R-project.org/package=CSIndicators/vignettes/EnergyIndicators.html)

Functions documentation can be found [here](https://CRAN.R-project.org/package=CSIndicators/CSIndicators.pdf)

| Function                       | CST version                        | Indicators                      |
|--------------------------------|------------------------------------|---------------------------------|
|PeriodMean                      |CST_PeriodMean                      |GST, SprTX, DTR                  |
|PeriodAccumulation              |CST_PeriodAccumulation              |SprR, HarR, PRCPTOT              | 
|AccumulationExceedingThreshold  |CST_AccumulationExceedingThreshold  |GDD, R95pTOT, R99pTOT            |
|TotalTimeExceedingThreshold     |CST_TotalTimeExceedingThreshold     |SU35, SU, FD, ID, TR, R10mm, Rnmm|
|TotalSpellTimeExceedingThreshold|CST_TotalSpellTimeExceedingThreshold|WSDI, CSDI                       |
|WindCapacityFactor              |CST_WindCapacityFactor              |Wind Capacity Factor             |
|WindPowerDensity                |CST_WindPowerDensity                |Wind Power Density               |
 
  	
| Auxiliar function | CST version          |
|-------------------|----------------------|
|AbsToProbs         |CST_AbsToProbs        |
|QThreshold         |CST_QThreshold        |
|Threshold          |CST_Threshold         |
|MergeRefToExp      |CST_MergeRefToExp     |
|SelectPeriodOnData |CST_SelectPeriodOnData|
|SelectPeriodOnDates|                      |

Find the current status of each function in this link: https://docs.google.com/spreadsheets/d/1arqgw-etNPs-XRyMTJ4ekF5YjQxAZBzssxxr2GMXp3c/edit#gid=0.

*Note: the CST version uses 's2dv_cube' objects as inputs and outputs while the former version uses multidimensional arrays with named dimensions as inputs and outputs*

*Note: All functions computing indicators allows to subset a time period if required, although this temporal subsetting can also be done with functions `SelectPeriodOnData` in a separated step.* 


### How to contribute

1. Open an issue to ask for help or describe a function to be integrated
2. Agree with maintainers (@ngonzal2, @rmarcos, @nperez and @erifarov) on the requirements
3. Create a new branch from master with a meaningful name
4. Once the development is finished, open a merge request to merge the branch on master

*Note: Remember to work with multidimensionals arrays with named dimensions when possible and use multiApply (https://earth.bsc.es/gitlab/ces/multiApply)*

### Add a function

To add a new function in this R package, follow this considerations:

1. Each function exposed to the users should be in separate files in the R folder
2. The name of the function should match the name of the file (e.g.: `Function()` included in file **Function.R**
3. The documentation should be in roxygen2 format as a header of the function
4. Once, the function and the documentation is finished, run the command `devtools::document()` in your R terminal to automatically generate the **Function.Rd** file
5. Remember to use R 3.6.1 when doing the development 
6. Code format: include spaces between operators (e.g. +, -, &), before { and after ','. The maximum length of lines is of 100 characters (hard limit 80 characters). Number of indentation spaces is 2.
7. Functions computing Climate indicators should include a temporal subsetting option. Use the already existing functions to adapt your code. 
