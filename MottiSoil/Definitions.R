## List of defined variables is at the end of this file.


## 1. MELA FILES AND IF ALL INPUT (MELAINPUT, YEARS, AREAS) ARE READ FROM
##    ONE FILE - THIS THE NEW WAY
##    OR IF THE MELAINPUT (stock, nat. mortality, harvest waste) ARE READ FROM
##    SEPARATE FILES AND YEARS AND AREAS ARE GIVEN AS A SEPARATE INPUT (IN AN INPUT
##    FILE) - THIS THE OLD WAY
##    

InputDirectory <- '~/Hiililaskuri/'
fil = "exampleinput.txt"
InputFile = paste(InputDirectory, fil,sep="")  #Melainput, Melayears and Areas from here

#Sometimes it is necessary to print out (on console) some information
#for tracking an error in reading the file (probably the file had not standard structure)

InputFileDebug = TRUE


Region = 1     #1 = Southern Finland = Forestry Centres 1-10
               #2 = Northern Finland = Forestry Centres 11-13

YassoSpinUp = TRUE


## 3. FILES FOR LITTER INPUT BEFORE MELA PERIOD AND INITIAL SOIL C STOCK
##    THEY ARE (NORMALLY) FROM CALCULATIONS OF GREEHOUSE GAS INVENTORY

SomeFilesDirectory <- 'SomeFiles/'


## 5. DEFINITIONS RELATED TO YASSO

YPath <- '/hsan2/khk/ghg/2010/soil/functions/yasso07/'
YassoFile <- paste(YPath,"yasso07.R",sep="")
YassoSharedLibrary <- paste(YPath,"yasso07.so",sep="")

# If weather conditions are constant or change in time - this affects also
# the litter input and initial state file from the GHGI

VariableWeather = TRUE

if(VariableWeather) {
    YassoInitialFi <- "GHGI-soil-carbon-mineral-variable-weather.txt"
} else {
    YassoInitialFi <- "GHGI-soil-carbon-mineral-constant-weather.txt"
}
YassoInitialStateFile <- paste(SomeFilesDirectory, YassoInitialFi,sep="")
#GHGILitterMineralFile <- paste(StockLitterInputDirectory,GHGILitterMineralIF,sep="")

# Parameters
#I got this from Aleksi May 2014
### Scandinavian new, added 7.5.2012
YassoParameters <- c(-0.5172509,-3.551512,-0.3458914,-0.2660175,0.044852223,
           0.0029265443,0.9779027,0.6373951,0.3124745,0.018712098,0.022490378,
           0.011738963,0.00099046889,0.3361765,0.041966144,0.089885026,0.089501545,-0.0022709155,0.17,
           -0.0015,0.17,-0.0015,0.17,-0.0015,0,-2.935411,0,101.8253,
           260,-0.080983594,-0.315179,-0.5173524,0, 0,-0.00024180325,
           0.0015341907, 101.8253,260,-0.5391662,1.18574,-0.2632936,0,0,0)

# mean temperature (C), precipitation (mm), temperature amplitude (C)
# WEATHER CAN BE CONSTANT (E.G. MEAN OF A PERIOD) OR CHANGING FROM YEAR TO YEAR
# IN BOTH CASES THE WEATHER IS DEFINED HERE AS A FUNCTION = F(YEAR,REGION,WHICH QUANTITY)
if(VariableWeather) {
    weather <- read.csv(paste(SomeFilesDirectory,"GHG_climate_1960_2017.csv",sep=""),header=TRUE)
    #Annual values 1960-2017 from Jaakko Heikkinen 18.10.2018
    #columns of weather are  "Year" "meanT_north" "meanT_south" "Ampli_north" "Ampli_south"
    #"Rainfall_north" "Rainfall_south"
    #This function returns mean of this year and 29 previous years
     weatherFunction <- function(year,region,variable) {
         #region 1 = south, 2 = north
         #variable 1 = mean temp, 2 = temp amplitude, 3 = rainfall
         col <- variable * 2
         if(region == 1) {
            col <- col + 1
         }

    if(year <= 1960) {
        returnValue <- weather[1,col]   #First value
    }
    year1 <- year          #returns same value for all years >= 2017
    if(year1 > 2017) {
        year1 = 2017
    }
    if(year1 < 1960) {
        year1 = 1960
    }

    if(year1 < 1989) {
        span = year1 - 1960 + 1
    } else {
        span = 30
    }

    lastRow <- which(weather$Year == year1)
    firstRow <- lastRow - span + 1

    returnValue <- mean(weather[firstRow:lastRow,col])
    
    returnValue
     }
} else {  #constant weather
  # row 1 southern Finland, row 2 northern Finland
  #YassoEnvironment <- rbind(c(1,3.832523,566.382,13.3571),c(2,0.530517,510.4763,14.57347)) #   mean of 1971-2016
  # from Jaakko Heikkinen 9.10.2018
constWeather <- rbind(c(1,3.808594, 564.741837, 13.404949),c(2,0.5153716, 508.3148014, 14.6240573))
     weatherFunction <- function(year,region,variable) {
                  #region 1 = south, 2 = north
                  #variable 1 = mean temp, 2 = temp amplitude, 3 = rainfall
         if(variable == 1) {
             colm = 2
         } else {
             if(variable == 2) {
                 colm = 4
             } else {
                 colm = 3
             }
         }
         returnValue <- constWeather[region,colm]
         returnValue
     }
}
#YassoInitialpath <- '~/projections/2011/soil/mela/r-litter/'
#YassoInitialState <- paste(YassoInitialpath, "yasso07_initial_state_finland_2007-GHGI.dat",sep="")

Yassox0Factor <- 1 #Multiplies initial state of Yasso
YassoSs <- 1  #Multiplies first value of litter
YassoCC <- FALSE       #Whether climate change effects are present --
                       #affects also organic soil calculation

# Output of Yasso results to files
YassoOutputPath <- 'Result/'
YassoOutputLitter <- paste(YassoOutputPath,"litt.dat",sep="")
YassoOutputStockPerHa <- paste(YassoOutputPath,"CStockPerHa.dat",sep="")
YassoOutputCO2 <- paste(YassoOutputPath,"CO2Result.dat",sep="")



## 5. DEFINITIONS FOR PEATLAND CALCULATIONS (DRAINED SOILS)

sh <- read.table(paste(SomeFilesDirectory,"Peatland-type-shares24.1.2019.txt",sep=""), header=TRUE)

PeatlandTypeShares <- function(year) {
    if(year < 1990) {
        ret <- c(sh[sh$year == 1990,2:6][[1]],sh[sh$year == 1990,2:6][[2]],
                 sh[sh$year == 1990,2:6][[3]],sh[sh$year == 1990,2:6][[4]],
                 sh[sh$year == 1990,2:6][[5]])
    }
    else { if(year > 2016) {
        ret <- c(sh[sh$year == 2016,2:6][[1]],sh[sh$year == 2016,2:6][[2]],
               sh[sh$year == 2016,2:6][[3]],sh[sh$year == 2016,2:6][[4]],
               sh[sh$year == 2016,2:6][[5]])
       
    } else {
      ret <- c(sh[sh$year == year,2:6][[1]],sh[sh$year == year,2:6][[2]],
             sh[sh$year == year,2:6][[3]],sh[sh$year == year,2:6][[4]],
             sh[sh$year == year,2:6][[5]])
        
    } }
   ret
}


#Understorey litter from GHGI
UnderstoreyPath <- '/hsan2/khk/ghg/2009/soil/understorey/litter/'
UnderstoreyMineralSouth <-  paste(UnderstoreyPath,"und.nwl.SF.min.csv", sep="")
UnderstoreyMineralNorth <-  paste(UnderstoreyPath,"und.nwl.NF.min.csv", sep="")
UnderstoreyOrganicBelow <-  paste(UnderstoreyPath,"und.org.bel.csv", sep="")  #The same for South and North


## 7. BIOMASS FUNCTIONS AND BEF'S

FunctionPath <- '/hsan2/khk/ghg/2009/soil/functions/'
#These function files contain many function definitions made by Aleksi Lehtonen
source(paste(FunctionPath,"biomass.r", sep=""))              
source(paste(FunctionPath,"litter.r", sep=""))

#BEF
path.bef <- '/hsan2/khk/ghg/2008/trees/stock/remaining/'  # using BEFs from Juha
BEF.sp <- read.table(paste(path.bef,"corrected/pineBEFs10.dat", sep=""), header=TRUE)
BEF.ns <- read.table(paste(path.bef,"corrected/spruceBEFs10.dat", sep=""), header=TRUE)
BEF.dc <- read.table(paste(path.bef,"corrected/decidBEFs10.dat", sep=""), header=TRUE)

#The BEF tables (BEF.sp, BEF.ns, BEF.dc) are packaged to a function (see below) that returns
#BEF as BEF(region, soil, species, part). Part of the code uses this BEF function.


# foliage ratio of fine roots in southern and northern finland
BEF7.sf <-  read.csv('/hsan2/khk/ghg/2009/soil/nfidata/nfi7/BEF/Bef.SF.csv', header=TRUE)
BEF7.nf <-  read.csv('/hsan2/khk/ghg/2009/soil/nfidata/nfi7/BEF/Bef.NF.csv', header=TRUE)
FoliageRatioSouth <-  BEF7.sf[1:4, 9]/ BEF7.sf[1:4,8]  #These are vectors of length 4
FoliageRatioNorth <-  BEF7.nf[1:4, 9]/ BEF7.nf[1:4,8]


# Packaging of BEF.sp, BEF.ns, AND BEF.dc to a function

#This function reads BEF from BEF tables BEF.sp, BEF.ns  and BEF.dc when input is:
# region, soil, species, part
# region = 1,2 (southern, northern Finland)
# soil = 1,2,3 mineral, ditched, non-ditched organic soil
# species = 1,2,3 (pine, spruce, birch & other decidious)
# part = 1,2,3,4,5,6,7,8
# foliage           = 1
# branch (living)   = 2
#stem               = 3
#stump              = 4
# roots (coarse)    = 5
#bark               = 6
#deadBranch         = 7
#total              = 8

#ispecies deternines the BEF table
#ipart determines the column. The BEF values are in columns 3 - 10 in the tables
#in the order
#"stemwood" "bark" "liveBranch" "foliage" "deadBranch" "stump" "roots" "total"
#thus ipart translates to columns as follows:
#  ipart    1    2   3   4   5   6   7   8
#  col      6    5   3   8   9   4   7   10
#
# mineral soil is on row 1 (south) or row 4 (north)
# ditched organic soil (isoil=2) is on lines 3  and 6 (south, north)
# non-ditched organic soil (isoil = 3) on lines 2 and 5 (south, north) 
column = c(6,5,3,8,9,4,7,10)
soil_addrow = c(0,2,1)
region_row = c(1,4)

functionBEF <- function(iregion,isoil,ispecies,ipart) {
icol <- column[ipart]
irow <- region_row[iregion] + soil_addrow[isoil]
if(ispecies == 1)
  bef <- BEF.sp[irow,icol]
else {
  if(ispecies == 2)
    bef <- BEF.ns[irow,icol]
  else
    bef <- BEF.dc[irow,icol]
  }

    bef
}


#======================================================================================
# THE FOLLOWING VARIABLES / FUNCTIONS ARE DEFINED IN THIS FILE ABOVE

# MelaInputDirectory MelaStock MelaNatMortality MelaWaste
# MelaYears Areas 
# YPath YassoFile YassoSharedLibrary YassoParameters
# YassoEnvironment YassoInitialpath YassoInitialState Yassox0Factor YassoSs YassoCC
# YassoOutputPath YassoOutputLitter YassoOutputStockPerHaSouth YassoOutputStockPerHaNorth
# IntermediateLitterDir MineralSoilSouth MineralSoilNorth OrganicSoilAboveSouth
# OrganicSoilAboveNorth OrganicSoilBelowSouth OrganicSoilBelowNorth 
# UnderstoreyPath UnderstoreyMineralSouth UnderstoreyMineralNorth UnderstoreyOrganicBelow
# FunctionPath path.bef BEF.sp BEF.ns BEF.dc BEF7.sf BEF7.nf FoliageRatioSouth FoliageRatioNorth
# column soil_addrow functionBEF DeltaTSouth DeltaTNorth PrecChgSouth PrecChgNortg






