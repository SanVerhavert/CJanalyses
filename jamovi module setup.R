################################################################################
################################################################################
#######################                                  #######################
#######################    Set up a new JAMOVI module    #######################
#######################                                  #######################
################################################################################
################################################################################

#### Preparation ####
### install package jmvtools
install.packages( "jmvtools", repos=c('https://repo.jamovi.org',
                                      'https://cran.r-project.org') )

### Check if jmvtools can find the jamovi install
jmvtools::check()

# if this does not work, locate the jamovi ##### folder [!! not the bin and .exe]
#   the folder can be located by calling properties on the quick launch in the start menu
# add the path to the check function as follows
jmvtools::check( home = "C:/Program Files/jamovi 0.8.0.8" )

# if the install is found in this way do not forget to globally set the
# home folder for this session in the following way
options(jamovi_home = "C:/Program Files/jamovi 0.8.0.8" )

#### install JAMOVI module ####
### Set the working directory to where you downloaded the folder
setwd( "C:/Users/SVerhavert/Desktop/Set up jamovi module" )

### install the module
jmvtools::install( "DPACanalyses" )

