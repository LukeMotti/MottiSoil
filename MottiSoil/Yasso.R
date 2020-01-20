source(YassoFile)
dyn.load(YassoSharedLibrary)
    
################################################################################
# MELA data
# Waste contains biomass of all material left in harvesting (whether normal or
# energy) in the forest.
# Each line contains: soil species part region dataYear1 ... data YearN
# soil = 1,2,3 (upland, ditched and non-ditched peatland)
# species = 1,2,3 (pine, spruce, birch & other tree species)
# part = 1,2,3,4,5 (foliage, branches, stem, stump, coarse roots (diam > 2mm))


#NatMort is as Waste but only stem volume for region soil species

# sf.min is litter from standing trees southern finland produced by Clean_volToCInput.R
# columns are "year" "nwl_A" "nwl_W" "nwl_E" "nwl_N" "fwl_A" "fwl_W" "fwl_E" "fwl_N"
#( _A is Acid soluble etc)
# rows are years 2007 ... 2057
# unit is t C / ha

#nf.min is as sf_min but for northern Finland

# Initial states for upland soils (southern Finland, northern Finland) are
# read from a file.
# Columns are: nwl_south" "fwl_south" "cwl_south" "nwl_north" "fwl_north" "cwl_north"
# Rows are A W E N H
###############################################################################

melaWaste <- MelaWaste
melaNatMort <- MelaNatMortality


# Litter from standing trees is per ha (to be consistent with Aleksi's calculations)
# Totals by multiplication with area
min <- TreeLitterYasso

#MELA areas
Akankaat = Areas$area[Areas$soil==1]
    
#There are 8 data colums in sf.min and nf.min
for(i in 2:9) {
min[,i] <- Akankaat * min[,i]
}


#Sum all litter inputs (melaWaste, melaNatMort and min (sf na nf)

melaTimes <- MelaYears

melaN = length(melaTimes)
melaNlitter = melaN - 1

## First melaWaste, that is cutting residues

#wastelitter for output
wastelitt<-matrix(data=0,nrow=melaNlitter,ncol=6)


#fol.rat is for correcting estimates of fine roots made on the
#basis of biomass calculated with the aid of Repola et al. functions and
#using BEFs based on Marklund's eqns
if(Region == 1) {     #Southern Finland
   fol.rat <-  FoliageRatioSouth
} else {
   fol.rat <-  FoliageRatioNorth
}

sumlitter.nwl <-matrix(data=0,nrow=melaNlitter,ncol=4)
sumlitter.fwl <-matrix(data=0,nrow=melaNlitter,ncol=4)
sumlitter.cwl <-matrix(data=0,nrow=melaNlitter,ncol=4)
#row = years MelaYears
#column = A W E N

for(ispecies in 1:3) {
  for(ipart in 1:5) {
    for(itimes in 1:melaNlitter) {
   #OBS wastelit only here and in fine roots
        wastelitt[itimes,ipart] = wastelitt[itimes,ipart] +
            carbon(melaWaste[melaWaste$soil==1
                             &melaWaste$species==ispecies&melaWaste$part==ipart,3+itimes])
            
       if(ipart == 1) { #fine roots also here with foliage
           sumlitter.nwl[itimes,] = sumlitter.nwl[itimes,] +
               foliage.AWEN(carbon(melaWaste[melaWaste$soil==1
             &melaWaste$species==ispecies&melaWaste$part==ipart,3+itimes]),ispecies)
           if(ispecies == 1) {
               flr <- fol.rat[1]
           } else if(ispecies == 2) {
               flr <- fol.rat[2]
           } else{
               flr = 1
           }
          
           sumlitter.nwl[itimes,] = sumlitter.nwl[itimes,] +
                  fineroot.AWEN(carbon(fineroots(flr*melaWaste[
                  melaWaste$soil==1&melaWaste$species==ispecies&melaWaste$part==ipart,3+itimes],
                                                 ispecies)),ispecies)
           wastelitt[itimes,6] = wastelitt[itimes,6] +
               carbon(fineroots(flr*melaWaste[
             melaWaste$soil==1&melaWaste$species==ispecies&melaWaste$part==ipart,3+itimes],
             ispecies))
       }  # ipart == 1
        if(ipart == 2) {
            sumlitter.fwl[itimes,] = sumlitter.fwl[itimes,] +
                 branches.AWEN(carbon(melaWaste[melaWaste$soil==1
                 &melaWaste$species==ispecies&melaWaste$part==ipart,3+itimes]))/
                   sum(branches.AWEN(1))
          }
        if(ipart == 5) { #roots are also fwl
            sumlitter.fwl[itimes,] = sumlitter.fwl[itimes,] +
                 stem.AWEN(carbon(melaWaste[melaWaste$soil==1
                 &melaWaste$species==ispecies&melaWaste$part==ipart,3+itimes]),ispecies)/
                   sum(stem.AWEN(1,ispecies))
          }
        if(ipart == 3 | ipart == 4) { #stem and stump are cwl
            sumlitter.cwl[itimes,] = sumlitter.cwl[itimes,] +
                  stem.AWEN(carbon(melaWaste[melaWaste$soil==1
                  &melaWaste$species==ispecies&melaWaste$part==ipart,3+itimes]),ispecies)/
                    sum(stem.AWEN(1,ispecies))
        }
    }
  }
}


## Then litter from natural mortality - all of it is litter. Natural mortality
## is specified in terms of stand volume in melaNatMort

#mortlitter for output
mortlitt<-matrix(data=0,nrow=melaNlitter,ncol=6)

for(ispecies in 1:3) {
    for(itimes in 1:melaNlitter) {
        stemVol <- melaNatMort[melaNatMort$soil==1
                     &melaNatMort$species==ispecies,2+itimes]
        for(ipart in 1:5) {
            if(ipart == 1)  {#with foliage also fine roots
             sumlitter.nwl[itimes,] = sumlitter.nwl[itimes,] +
              foliage.AWEN(carbon(functionBEF(Region,1,ispecies,ipart)*stemVol),ispecies)
             mortlitt[itimes,ipart] = mortlitt[itimes,ipart] +
               carbon(functionBEF(Region,1,ispecies,ipart)*stemVol)
             
             if(ispecies == 1) {
                 flr <- fol.rat[1]} else {
               if(ispecies == 2) {
                  flr = fol.rat[1]} else{flr = 1}
              }
             sumlitter.nwl[itimes,] = sumlitter.nwl[itimes,] +
             fineroot.AWEN(carbon(fineroots(flr*functionBEF(Region,1,ispecies,ipart)*stemVol,
                                            ispecies)),ispecies)
             mortlitt[itimes,6] = mortlitt[itimes,6] +
             carbon(fineroots(flr*functionBEF(Region,1,ispecies,ipart)*stemVol,
                                                       ispecies))
            } #if(ipart ==1
            if(ipart == 2) {
             sumlitter.fwl[itimes,] = sumlitter.fwl[itimes,] +
             branches.AWEN(carbon((functionBEF(Region,1,ispecies,ipart)+
             functionBEF(Region,1,ispecies,7))*stemVol))
             mortlitt[itimes,ipart] = mortlitt[itimes,ipart] +
             carbon((functionBEF(Region,1,ispecies,ipart)+
              functionBEF(Region,1,ispecies,7))*stemVol)
           }
           if(ipart == 5) {  #Roots are also fwl
             sumlitter.fwl[itimes,] = sumlitter.fwl[itimes,] +
             stem.AWEN(carbon(functionBEF(Region,1,ispecies,ipart)*stemVol),ispecies)
             mortlitt[itimes,ipart] = mortlitt[itimes,ipart] +
               carbon(functionBEF(Region,1,ispecies,ipart)*stemVol)
           }
           if( ipart == 3 | ipart == 4) {       #stem and stump are cwl
             sumlitter.cwl[itimes,] = sumlitter.cwl[itimes,] + #OnlyDOMSOM = not Deadwood
             stem.AWEN(carbon(functionBEF(Region,1,ispecies,ipart)*stemVol),ispecies)
             mortlitt[itimes,ipart] = mortlitt[itimes,ipart] +
               carbon(functionBEF(Region,1,ispecies,ipart)*stemVol)
             if(ipart ==3) {sumlitter.cwl[itimes,] = sumlitter.cwl[itimes,] +
             stem.AWEN(carbon(functionBEF(Region,1,ispecies,6)*stemVol),ispecies)
             mortlitt[itimes,ipart] = mortlitt[itimes,ipart] +
               carbon(functionBEF(Region,1,ispecies,6)*stemVol)       #Bark
             }
           }
        }#for(ipart     
    }
}


simulationPeriod <- melaTimes[1]:max(melaTimes)

#Approximation of wastelitter and mortlitter that are a step function = constant during 10 year
#periods: the value is at the midle of the period except the first one.

#approxTimes = value time of litter input. Best (must) be between times when MELA stock values
#have been given. The litter from harvest (waste) and natural mortality is constant for periods
# between MELA stock values - most natural
#is thus in the middle of the period. rule = 2 gives constant value outside approxTimes, thus
#litter input from harvest and natural mortality = constant in the first half of the first MELA period.
#Litter from standing trees (sf.min and nf.min) change from the start because it is calculated with
#the aid of stock values that change between melaTimes.

approxTimes = as.integer(melaTimes[1:(melaN-1)] + diff(melaTimes)/2)

inputLitter <- data.frame(cbind(simulationPeriod,
    min[,2] + 1000*approx(approxTimes,sumlitter.nwl[,1],simulationPeriod,rule=2)$y,
    min[,3] + 1000*approx(approxTimes,sumlitter.nwl[,2],simulationPeriod,rule=2)$y,
    min[,4] + 1000*approx(approxTimes,sumlitter.nwl[,3],simulationPeriod,rule=2)$y,
    min[,5] + 1000*approx(approxTimes,sumlitter.nwl[,4],simulationPeriod,rule=2)$y,
    min[,6] + 1000*approx(approxTimes,sumlitter.fwl[,1],simulationPeriod,rule=2)$y,
    min[,7] + 1000*approx(approxTimes,sumlitter.fwl[,2],simulationPeriod,rule=2)$y,
    min[,8] + 1000*approx(approxTimes,sumlitter.fwl[,3],simulationPeriod,rule=2)$y,
    min[,9] + 1000*approx(approxTimes,sumlitter.fwl[,4],simulationPeriod,rule=2)$y,
    1000*approx(approxTimes,sumlitter.cwl[,1],simulationPeriod,rule=2)$y,
    1000*approx(approxTimes,sumlitter.cwl[,2],simulationPeriod,rule=2)$y,    #no cwl litter from
    1000*approx(approxTimes,sumlitter.cwl[,3],simulationPeriod,rule=2)$y,    #standing trees
    1000*approx(approxTimes,sumlitter.cwl[,4],simulationPeriod,rule=2)$y))

    colnames(inputLitter) <- c("year","nwl_A","nwl_W","nwl_E","nwl_N","fwl_A","fwl_W",
                                  "fwl_E","fwl_N","cwl_A","cwl_W","cwl_E","cwl_N")

#litters for output
wasteLitter <- data.frame(cbind(simulationPeriod,
        1000.0*approx(approxTimes,wastelitt[,1],simulationPeriod,rule=2)$y,
        1000.0*approx(approxTimes,wastelitt[,2],simulationPeriod,rule=2)$y,
        1000.0*approx(approxTimes,wastelitt[,3],simulationPeriod,rule=2)$y,
        1000.0*approx(approxTimes,wastelitt[,4],simulationPeriod,rule=2)$y,
        1000.0*approx(approxTimes,wastelitt[,5],simulationPeriod,rule=2)$y,
        1000.0*approx(approxTimes,wastelitt[,6],simulationPeriod,rule=2)$y))

mortLitter <- data.frame(cbind(simulationPeriod,
        1000.0*approx(approxTimes,mortlitt[,1],simulationPeriod,rule=2)$y,
        1000.0*approx(approxTimes,mortlitt[,2],simulationPeriod,rule=2)$y,
        1000.0*approx(approxTimes,mortlitt[,3],simulationPeriod,rule=2)$y,
        1000.0*approx(approxTimes,mortlitt[,4],simulationPeriod,rule=2)$y,
        1000.0*approx(approxTimes,mortlitt[,5],simulationPeriod,rule=2)$y,
        1000.0*approx(approxTimes,mortlitt[,6],simulationPeriod,rule=2)$y))
colnames(wasteLitter) <- c("year","mwafol","mwabra","mwastem","mwastump","mwaroots","mwafr")
colnames(mortLitter) <- c("year","mmofol","mmobra","mmostem","mmostump","mmoroots","mmofr")


## UNDERSTOREY LITTER

if(Region == 1) {            #Southern Finland
   undLitt <-  read.csv(UnderstoreyMineralSouth, header=TRUE)
   } else {
   undLitt <-  read.csv(UnderstoreyMineralNorth, header=TRUE)
   }
# All are nwl

for(i in 1:length(simulationPeriod)) {
      inputLitter[i,2:5] <- inputLitter[i,2:5] + 0.0*Akankaat*t(undLitt)[1:4]
}


## inputLitter is litter input to Yasso calculations,
## inputLitterFinland is for output

inputLitterFinland <- cbind(simulationPeriod,inputLitter[,2:13])
colnames(inputLitterFinland) <- c("year","nwl_A","nwl_W","nwl_E","nwl_N","fwl_A","fwl_W",
                                  "fwl_E","fwl_N","cwl_A","cwl_W","nwl_E","cwl_N")


## YASSO CALCULATIONS

## INITIAL STATE

if(!YassoSpinUp) {
#ist <- read.table("~/projections/2011/soil/mela/r-litter/GHGI-soil-carbon-mineral-variable-weather.txt"
#  ,header=TRUE)   #ist contains Yasso soil C stocks
           #for years of greenhouse gas inventory
ist <- read.table(YassoInitialStateFile, header=TRUE)

initialState <- data.frame(cbind(t(ist[ist$year==simulationPeriod[1] & ist$region == 1,3:7]),
  t(ist[ist$year==simulationPeriod[1] & ist$region == 1,8:12]),
  t(ist[ist$year==simulationPeriod[1] & ist$region == 1,13:17]),
  t(ist[ist$year==simulationPeriod[1] & ist$region == 2,3:7]),
  t(ist[ist$year==simulationPeriod[1] & ist$region == 2,8:12]),
  t(ist[ist$year==simulationPeriod[1] & ist$region == 2,13:17])
  ))
 colnames(initialState) = c("nwl_south", "fwl_south", "cwl_south","nwl_north","fwl_north", "cwl_north")
 rownames(initialState) = c("a","w","e","n","h")

 # Intial state is per ha , thus totals by multiplication with area
for(i in 1:3) {
   initialState[,i] <- Akankaat * initialState[,i]
   initialState[,i+3] <- Akankaat * initialState[,i+3]
 }
}

# The weather contitions
# mean temperature (C), precipitation (mm), temperature amplitude (C)
# were earlier constant and given in a two-row table:
# row 1 southern Finland, row 2 northern Finland, named in this code
# as e0, and assigned to the value that was defined in MELAToC_defintions.R
#e0 <- YassoEnvironment
# NOW the weather conditions are given through the function (defined in MELAToC_defintions.R)
# weatherFunction(year, region, variable)
   #region 1 = south, 2 = north
   #variable 1 = mean temp, 2 = temp amplitude, 3 = rainfall
# (weather conditions can be either changing or constant)

# In the code, e0 has been replaced by call
# weatherFunction(i, region, variable).
# NOTE that in e0 the variables were in order according to columns:
# temperature, precipitation, temp. amplitude, in weatherFunction the
# "order" of variables is mean temp, temp amplitude, precipitation.


#lout contains litter fed into Yasso each simulation year with break down
# non woody, fine woody and coarse woody litter.
lout<-data.frame(matrix(0,nrow=length(simulationPeriod),ncol=4))
colnames(lout)=c("year","litter_nwl","litter_fwls","litter_cwls")
lout[,1] = simulationPeriod

step = 1
zDummy <- c(0,0,0,0,0)
init<-c(0,0,0,0,0)

zeroNyrs <- rep(0,times=length(simulationPeriod))

#nwl
diameter <- 0

Cnwl <- data.frame(simulationPeriod,zeroNyrs,zeroNyrs,zeroNyrs,zeroNyrs,zeroNyrs)
colnames(Cnwl)=c("year","A","W","E","N","H")

if(YassoSpinUp) {
     inity = simulationPeriod[1]
     e = c(weatherFunction(inity,1,1),weatherFunction(inity,1,3),weatherFunction(inity,1,2))
     spininput =c(mean(inputLitter[,2]),mean(inputLitter[,3]),
                             mean(inputLitter[,4]),mean(inputLitter[,5]),0)
     initnwl <- Yassox0Factor*yasso07(YassoParameters, 10000, e, init, spininput, diameter, zDummy)
     } else {
         if(Region == 1) {
             initnwl = Yassox0Factor * initialState[,1]
         } else {
             initnwl = Yassox0Factor * initialState[,4]         
         }
     }

for(i in simulationPeriod) {
  if (i==simulationPeriod[1]) {
      init <- initnwl
      } else {
      init <- Cnwl[Cnwl$year==i-1,2:6]
      }
      e = c(weatherFunction(i,1,1),weatherFunction(i,1,3),weatherFunction(i,1,2))
      lout[lout$year==i,2]=sum(inputLitter[inputLitter$year==i,2:5])
      Cnwl[Cnwl$year==i,2:6] <-
      yasso07(YassoParameters, step, e, init,
              c(inputLitter[inputLitter$year==i,2:5],0), diameter, zDummy)
}

#fwl
diameter = 2

Cfwl <- data.frame(simulationPeriod,zeroNyrs,zeroNyrs,zeroNyrs,zeroNyrs,zeroNyrs)
colnames(Cfwl)=c("year","A","W","E","N","H")
init <- c(0,0,0,0,0)
if(YassoSpinUp) {
     inity = simulationPeriod[1]
     e = c(weatherFunction(inity,1,1),weatherFunction(inity,1,3),weatherFunction(inity,1,2))
     spininput =c(mean(inputLitter[,6]),mean(inputLitter[,7]),
                             mean(inputLitter[,8]),mean(inputLitter[,9]),0)
     initfwl <- Yassox0Factor*yasso07(YassoParameters, 10000, e, init, spininput, diameter, zDummy)
     } else {
         if(Region == 1) {
             initfwl = Yassox0Factor * initialState[,2]
         } else {
             initfwl = Yassox0Factor * initialState[,5]         
         }
     }

   for(i in simulationPeriod) {
        if (i==simulationPeriod[1]) {init <- initfwl} else
            {init <- Cfwl[Cfwl$year==i-1,2:6]}
        e = c(weatherFunction(i,1,1), weatherFunction(i,1,3),
                        weatherFunction(i,1,2))
      lout[lout$year==i,3]=sum(inputLitter[inputLitter$year==i,6:9])
         Cfwl[Cfwl$year==i,2:6] <-
         yasso07(YassoParameters, step, e, init,
         c(inputLitter[inputLitter$year==i,6:9],0), diameter, zDummy)
      }

#cwl
diameter = 15

Ccwl <- data.frame(simulationPeriod,zeroNyrs,zeroNyrs,zeroNyrs,zeroNyrs,zeroNyrs)
colnames(Ccwl)=c("year","A","W","E","N","H")
init <- c(0,0,0,0,0)
if(YassoSpinUp) {
     inity = simulationPeriod[1]
     e = c(weatherFunction(inity,1,1),weatherFunction(inity,1,3),weatherFunction(inity,1,2))
     spininput =c(mean(inputLitter[,10]),mean(inputLitter[,11]),
                             mean(inputLitter[,12]),mean(inputLitter[,13]),0)
     initcwl <- Yassox0Factor*yasso07(YassoParameters, 10000, e, init, spininput, diameter, zDummy)
     } else {
         if(Region == 1) {
             initcwl = Yassox0Factor * initialState[,3]
         } else {
             initncwl = Yassox0Factor * initialState[,6]         
         }
     }

   for(i in simulationPeriod) {
      if (i==simulationPeriod[1]) {init <- initcwl} else
          {init <- Ccwl[Ccwl$year==i-1,2:6]}
      e = c(weatherFunction(i,1,1), weatherFunction(i,1,3), weatherFunction(i,1,2))
      lout[lout$year==i,4]=sum(inputLitter[inputLitter$year==i,10:13])
      Ccwl[Ccwl$year==i,2:6] <-
       yasso07(YassoParameters, step, e, init,
       c(inputLitter[inputLitter$year==i,10:13],0), diameter, zDummy)
      }



CFinlandMineralSoil = cbind(Cnwl[,"year"],Cnwl[,2:6] + Cfwl[,2:6] + Ccwl[,2:6])
colnames(CFinlandMineralSoil)=c("year","A","W","E","N","H")

write.table(round(lout,4), file=YassoOutputLitter,row.names=FALSE )


CStockPerHa = cbind(Cnwl[,"year"],Cnwl[,2:6]/Akankaat +
                   Cfwl[,2:6]/Akankaat
                   + Ccwl[,2:6]/Akankaat)
colnames(CStockPerHa)=c("year","A","W","E","N","H")

write.table(round(CStockPerHa,4), file=YassoOutputStockPerHa,row.names=FALSE )

change <- diff(rowSums(CFinlandMineralSoil[,2:length(CFinlandMineralSoil[1,])]),lag=1)


yassoresult <- cbind(simulationPeriod,rowSums(inputLitterFinland[,2:length(inputLitterFinland[1,])]),
                          c(change,change[length(change)]))

yassoresult <- cbind(yassoresult,wasteLitter[,2:7],mortLitter[,2:7])
                         
colnames(yassoresult) <- c("year","mineralLitter","C_mineral",colnames(wasteLitter)[2:7],
                   colnames(mortLitter)[2:7])

