# CJanalyses JAMOVI module

This repository contains a JAMOVI module for doing Comparative Judgement
analyses.
This module is meant for the AEA workshop and other D-PAC workshops
For more information on Comparative Judgement see the [D-PAC website](http://www.d-pac.be)

This module is built and tested under Jamovi 0.8.0.8

## Installation

To install the module in jamovi with the .jmo file:
1.In Jamovi click on modules >> jamovi library
2. Select Sideload and clicc the upload icon
3. Select the correct .jmo file
..* File ending on _win.jmo is the windows build
..* File ending on _mac is the mac build

To install the module from the R scripts:
* Open the R project (.Rproj) file [only for R studio]
* Open the file "jamovi module setup.R" and folow the steps there

## Repository structure
```  
.  
|  
|––_CJanalyses    folder with files for build  
|  |––_jamovi     folder with yaml files for jamovi analysis, results and UI build  
|  |––_R          folder with R scripts for analysis  
|––_Data          folder with example data  
|––_dist          folder with last stable copy of the module files  

```
