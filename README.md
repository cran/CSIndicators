CSIndicators
===============

#### Sectoral Indicators for Climate Services Based on Sub-Seasonal to Decadal Climate Predictions

Set of generalised tools for the flexible computation of climate related indicators defined by the user. Each method represents a specific mathematical approach which is combined with the possibility to select an arbitrary time period to define the indicator. This enables a wide range of possibilities to tailor the most suitable indicator for each particular climate service application (agriculture, food security, energy, water management…). This package is intended for sub-seasonal, seasonal and decadal climate predictions, but its methods are also applicable to other time-scales, provided the dimensional structure of the input is maintained. Additionally, the outputs of the functions in this package are compatible with [CSTools](https://earth.bsc.es/gitlab/external/cstools).  

How to cite
-----------

> Pérez-Zanón, N., Ho, A. Chou, C., Lledó, L., Marcos-Matamoros, R., Rifà, E. and González-Reviriego, N. (2023). CSIndicators: Get tailored climate indicators for applications in your sector. Climate Services. https://doi.org/10.1016/j.cliser.2023.100393  

For details in the methodologies see:  

> Pérez-Zanón, N., Caron, L.-P., Terzago, S., Van Schaeybroeck, B., Lledó, L., Manubens, N., Roulin, E., Alvarez-Castro, M. C., Batté, L., Bretonnière, P.-A., Corti, S., Delgado-Torres, C., Domínguez, M., Fabiano, F., Giuntoli, I., von Hardenberg, J., Sánchez-García, E., Torralba, V., and Verfaillie, D.: Climate Services Toolbox (CSTools) v4.0: from climate forecasts to climate forecast information, Geosci. Model Dev., 15, 6115–6142, https://doi.org/10.5194/gmd-15-6115-2022, 2022.  
Chou, C., R. Marcos-Matamoros, L. Palma Garcia, N. Pérez-Zanón, M. Teixeira, S. Silva, N. Fontes, A. Graça, A. Dell'Aquila, S. Calmanti and N. González-Reviriego (2023). Advanced seasonal predictions for vine management based on bioclimatic indicators tailored to the wine sector. Climate Services, 30, 100343, https://doi.org/10.1016/j.cliser.2023.100343.  
Lledó, Ll., V. Torralba, A. Soret, J. Ramon and F.J. Doblas-Reyes (2019). Seasonal forecasts of wind power generation. Renewable Energy, 143, 91-100, https://doi.org/10.1016/j.renene.2019.04.135.

Installation
------------

You can then install the public released version of CSIndicators from CRAN:
```r
install.packages("CSIndicators")
```
Or the development version from the GitLab repository:
```r
# install.packages("devtools")
devtools::install_git("https://earth.bsc.es/gitlab/es/csindicators.git")
```

Overview
--------

To learn how to use the package see:

- [**Agricultural Indicators**](https://CRAN.R-project.org/package=CSIndicators/vignettes/AgriculturalIndicators.html)
- [**Wind Energy Indicators**](https://CRAN.R-project.org/package=CSIndicators/vignettes/EnergyIndicators.html)

Functions documentation can be found [here](https://CRAN.R-project.org/package=CSIndicators/CSIndicators.pdf).

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

Find the current status of each function in [this link](https://docs.google.com/spreadsheets/d/1arqgw-etNPs-XRyMTJ4ekF5YjQxAZBzssxxr2GMXp3c/edit#gid=0).

*Note: the CST version uses 's2dv_cube' objects as inputs and outputs while the former version uses multidimensional arrays with named dimensions as inputs and outputs*

*Note: All functions computing indicators allows to subset a time period if required, although this temporal subsetting can also be done with functions `SelectPeriodOnData` in a separated step.* 

#### Object class 's2dv_cube'

This package is designed to be compatible with other R packages such as [CSTools](https://CRAN.R-project.org/package=CSTools) through a common object: the `s2dv_cube` object class, used in functions with the prefix **CST_**. This object can be created from Start ([startR](https://CRAN.R-project.org/package=startR) package) and from Load ([s2dv](https://CRAN.R-project.org/package=s2dv) package) directly.  

The class `s2dv_cube` is mainly a list of named elements to keep data and metadata in a single object. Basic structure of the object:

```r
$ data: [data array]
$ dims: [dimensions vector]
$ coords: [List of coordinates vectors]
  $ sdate
  $ time
  $ lon
  [...]
$ attrs: [List of the attributes]
  $ Variable:
    $ varName
    $ metadata 
  $ Datasets
  $ Dates
  $ source_files
  $ when
  $ load_parameters
```

More information about the `s2dv_cube` object class can be found here: [description of the s2dv_cube object structure document](https://docs.google.com/document/d/1ko37JFl_h6mOjDKM5QSQGikfLBKZq1naL11RkJIwtMM/edit?usp=sharing).

The current `s2dv_cube` object (CSIndicators 1.0.0 and CSTools 5.0.0) differs from the original object used in the previous versions of the packages. If you have **questions** on this change you can follow some of the points below:

- [New s2dv_cube object discussion in CSTools](https://earth.bsc.es/gitlab/external/cstools/-/issues/94)
- [How to deal with the compatibility break in CSIndicators](https://earth.bsc.es/gitlab/es/csindicators/-/issues/25)

Contribute
----------

1. Open an issue to ask for help or describe a function to be integrated
2. Agree with maintainers (@ngonzal2, @rmarcos, @nperez and @erifarov) on the requirements
3. Create a new branch from master with a meaningful name
4. Once the development is finished, open a merge request to merge the branch on master

*Note: Remember to work with multidimensionals arrays with named dimensions when possible and use [multiApply](https://earth.bsc.es/gitlab/ces/multiApply).*

#### Add a function

To add a new function in this R package, follow this considerations:

1. Each function exposed to the users should be in separate files in the R folder
2. The name of the function should match the name of the file (e.g.: `Function()` included in file **Function.R**)
3. The documentation should be in roxygen2 format as a header of the function
4. Once, the function and the documentation is finished, run the command `devtools::document()` in your R terminal to automatically generate the **Function.Rd** file
5. Remember to use R 4.1.2 when doing the development 
6. Code format: include spaces between operators (e.g. +, -, &), before and after ','. The maximum length of lines is of 100 characters (hard limit 80 characters). Number of indentation spaces is 2.
7. Functions computing Climate indicators should include a temporal subsetting option. Use the already existing functions to adapt your code. 
