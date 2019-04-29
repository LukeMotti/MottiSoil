# Calculates the the annual carbon balance of drained organic soils as is (approximately)
# done in the Greenhouse Gas Inventory, that is,
# balance = emission - below-ground litter input
#
# Emission is calculated with the aid of site specific emission factors and areas of site types 
# Litter input consists litter from living trees, harvest residues, natural mortality and
# understorey vegetation
#


## LITTER INPUT

## UNDERSTOREY LITTER BELOW GROUND

undBel <-  (read.csv(UnderstoreyOrganicBelow, header=TRUE))

## LITTER FROM BELOW-GROUND PARTS OF TREES
treelitter <- OrganicSoilBelowLitter

## LITTER FROM LOGGING RESIDUES
melaWaste <- MelaWaste


melaTimes <- MelaYears
melaN <- length(melaTimes)
melaNlitter <- melaN - 1 

if(Region == 1) {
    fol.rat <-  FoliageRatioSouth
} else {
    fol.rat <- FoliageRatioNorth
}

#wastelitter for output
wastelitt<-matrix(data=0,nrow=melaNlitter,ncol=2)

sumlitterWasteBel <- matrix(data=0,nrow=melaNlitter,ncol=1)

sumlitter <-matrix(data=0,nrow=melaNlitter,ncol=1)
for(ispecies in 1:3) {
    for(ipart in 1:5) {
        for(itimes in 1:melaNlitter) {
            if(ipart == 1) { #only fine roots here
                if(ispecies == 1) {
                    flr <- fol.rat[1]
                } else if(ispecies == 2) {
                    flr <- fol.rat[2]
                } else {
                    flr = 1
                }
             sumlitter[itimes] = sumlitter[itimes] +
             carbon(fineroots(flr*1000.0*melaWaste[
             melaWaste$soil==2&melaWaste$species==ispecies&melaWaste$part==ipart,3+itimes],
             ispecies))
             wastelitt[itimes,1] = wastelitt[itimes,1] +
             carbon(fineroots(flr*1000.0*melaWaste[
             melaWaste$soil==2&melaWaste$species==ispecies&melaWaste$part==ipart,3+itimes],
             ispecies))
            }
            if(ipart == 5) {
              sumlitter[itimes] = sumlitter[itimes] +
              carbon(1000.0*melaWaste[melaWaste$soil==2
              &melaWaste$species==ispecies&melaWaste$part==ipart,3+itimes])
              wastelitt[itimes,1] = wastelitt[itimes,1] +
              carbon(1000.0*melaWaste[melaWaste$soil==2
              &melaWaste$species==ispecies&melaWaste$part==ipart,3+itimes])                        
            }
            if(ipart == 2 | ipart == 3 | ipart == 4 ) {
            wastelitt[itimes,2] = wastelitt[itimes,2] +
            carbon(1000.0*melaWaste[melaWaste$soil==2
            &melaWaste$species==ispecies&melaWaste$part==2,3+itimes]) +
            carbon(1000.0*melaWaste[melaWaste$soil==2
            &melaWaste$species==ispecies&melaWaste$part==3,3+itimes]) +
            carbon(1000.0*melaWaste[melaWaste$soil==2
            &melaWaste$species==ispecies&melaWaste$part==4,3+itimes])
            }
           
        }
      } #for(ipart in
    } #for(ispecies in
         
sumlitterWasteBel <- sumlitter


## LITTER FROM NATURAL MORTALITY

#melaNatMort = read.table(MelaNatMortality, header=TRUE)
melaNatMort = MelaNatMortality
## Natural mortality is specified in terms of stand volume in melaNatMort

sumlitterNMortBel <-matrix(data=0,nrow=melaNlitter,ncol=1)

#mortlitter for output
mortlitt<-matrix(data=0,nrow=melaNlitter,ncol=2)


sumlitter <-matrix(data=0,nrow=melaNlitter,ncol=1)


iregion <- 1                 ##HUOM!
    
for(ispecies in 1:3) {
    for(itimes in 1:melaNlitter) {
        stemVol <- 1000.0*melaNatMort[melaNatMort$soil==2
                   &melaNatMort$species==ispecies,2+itimes]
        for(ipart in 1:5) {
            if(ipart == 1)  {#only fine roots from foliage
                if(ispecies == 1) {
                    flr <- fol.rat[1]
                } else if(ispecies == 2) {
                    flr <- fol.rat[2]
                } else {
                    flr = 1
                }
                sumlitter[itimes] = sumlitter[itimes] +
                    carbon(fineroots(flr*functionBEF(iregion,2,ispecies,ipart)*stemVol,
                                     ispecies))
                mortlitt[itimes,1] = mortlitt[itimes,1] +
                    carbon(fineroots(flr*functionBEF(iregion,2,ispecies,ipart)*stemVol,
                                     ispecies))
            } #if(ipart ==1
            if(ipart == 5) {  #Roots are bel
                sumlitter[itimes] = sumlitter[itimes] +
                    carbon(functionBEF(iregion,2,ispecies,ipart)*stemVol)
                mortlitt[itimes,1] = mortlitt[itimes,1] +
                    carbon(functionBEF(iregion,2,ispecies,ipart)*stemVol)
            }
            if(ipart == 2 | ipart == 3 | ipart == 4) { #all rest is above-ground
                 mortlitt[itimes,2] = mortlitt[itimes,2] +
                     carbon(functionBEF(iregion,2,ispecies,2)*stemVol) +
                     carbon(functionBEF(iregion,2,ispecies,3)*stemVol) +
                     carbon(functionBEF(iregion,2,ispecies,4)*stemVol)
            }
        }#for(ipart
    }
}
sumlitterNMortBel <- sumlitter


## CALCULATION OF THE BALANCE

melaYears = seq(from=melaTimes[1], to=melaTimes[length(melaTimes)], by = 1)
approxTimes <- as.integer(melaTimes[1:(melaN-1)] + diff(melaTimes)/2)

#Areas of drained organic soils in Southern and Northern Finland
MelaADraindedOrg = Areas$area[Areas$soil==2]


# Litter
litterT <- MelaADraindedOrg*rowSums(treelitter[,2:9])[1:length(melaYears)]
litterW <- approx(approxTimes,sumlitterWasteBel,melaYears,rule=2)$y
litterM <- approx(approxTimes,sumlitterNMortBel,melaYears,rule=2)$y

#Litters for output
wasteLitter <- data.frame(cbind(melaYears,
               approx(approxTimes,wastelitt[,1],melaYears,rule=2)$y,
               approx(approxTimes,wastelitt[,2],melaYears,rule=2)$y))
mortLitter <- data.frame(cbind(melaYears,
               approx(approxTimes,mortlitt[,1],melaYears,rule=2)$y,
               approx(approxTimes,mortlitt[,2],melaYears,rule=2)$y))
    

#### Annual emissions from drained peat sites  g C m-2 a-1 to ton C /ha (note CARBON)
Rhtkg <- 425.7*0.01
Mtkg <- 312.1*0.01
Ptkg <- 242.3*0.01
Vatkg <- 218.9*0.01
Jatkg <- 185.2*0.01

#Areas of types of peatland  year 2008 (NIR 2010) 1000 ha
# Rhtkg        Mtkg       Ptkg      Vatkg       Jatkg
# 688         1150       1628        860         41
#relMireTypeAreas = c(688, 1150, 1628, 860, 41) / sum(688, 1150, 1628, 860, 41)

#Both emission and understorey litter input per unit area are constant over
#the whole county. If no litter input is coming from trees either south or north,
# emission and understorey litter input are equal to zero in the respective area.
emission <- 1:length(melaYears)
for(i in 1:length(melaYears)) {   
    rMTA <- PeatlandTypeShares(melaYears[i])
    emission[i] <- MelaADraindedOrg * (rMTA[1]*Rhtkg +
                   rMTA[2]*Mtkg + rMTA[3]*Ptkg +
                   rMTA[4]*Vatkg + rMTA[5]*Jatkg)
}

litterU <- MelaADraindedOrg * sum(undBel)

litterTotal <- litterU + litterT + litterW + litterM


cBalance <- litterTotal - emission

# Effect of (above-ground) dead wood pool - constant sink after 1997 or 2001
# This is from GHGI (Paula Ollila) 11.10.2018
#dom.sf <- c(0,0,0,0,0,0,0,0,rep(0.00754106277747484,(yr-1997)))
#dom.nf <- c(0,0,0,0,0,0,0,0,0,0,0,0, rep(0.00720002212248889,(yr-2001)))
#unit is t C / ha (/year)

timeSpan <- melaYears
condDeadwood <- timeSpan > 1997       #TAMA ETELA-SUOMEN TILANNE
#cond.nf <- timeSpan > 2001
deadWoodChange <- MelaADraindedOrg * condDeadwood* 0.00754106277747484
#deadWoodChange.nf <- MelaADraindedOrg.nf * cond.nf * 0.00720002212248889

cBalance <- cBalance + deadWoodChange


organic <- cbind(melaYears,litterTotal,cBalance,wasteLitter[,2:3],mortLitter[,2:3],litterT,litterU)
colnames(organic)=c("year","orgLitter","C_organic","owaste_bel","owaste_abv","omort_bel","omort_abv",
                    "otree_bel","ound")

