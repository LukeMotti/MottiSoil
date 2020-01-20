c## This  "main program" runs transformation of MELA output to carbon balance of forests.
## MELA output consists of three parts (input files): data on stock (amount of stemwood),
## harvest residues (biomass) and natural mortality (amount of stemwood).
##
## Calculation takes place by pieces of R-code through source(" "). As a result, the variables
## in any part of the code are visible in other parts. BE CAREFUL NOT TO REDEFINE VARIABLES
## IF YOU ADD CODE TO THIS SYSTEM!
##
## All necessary input & definition of variables for calculations are given in the definition
## file MELAToC_definitions.R. The variables are kind of global variables for calculations.
## List of defined variables are at the end of file. The naming convention is capitalize
## each word & concatenate, e.g. MelaInputDirectory.
##
## MELAToC_volToCInput.R calculates litter input from standing trees. It writes intermediate
## files that Yasso and organic soil calculation will read. Stores results in data frame litt.
##
## MELAToC_stock.R calculates C balance of trees, results are in data frame cstockchange.
##
## MELATOc_yasso.R: Yasso for mineral soils, results are in data frame yassoresult.
##
## MELAToC_drainedsoils.R: calculations for derained organic soils, results are in
## data frame organic.
##
## Final results are in data frame CO2result (Tg CO2, IPCC sign convention)

source("Definitions.R")

source("ProcessFile.R")

source("VolToCInput.R")    #litt

source("Yasso.R")          #yassoresult

source("DrainedSoils.R") #organic


## OUTPUT

CO2result <- as.data.frame(cbind(yassoresult[,1],-44*yassoresult[,3]*1.0e-6/12,-44*organic[,3]*1.0e-6/12,
           44*yassoresult[,2]*1.0e-6/12,44*organic[,2]*1.0e-6/12) )

colnames(CO2result) <- c("year","CO2_min", "CO2_org","Litt_min","Litt_org")

write.table(round(CO2result,4), file=YassoOutputCO2,row.names=FALSE )