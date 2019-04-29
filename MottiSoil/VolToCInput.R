### to estimate litter of trees from MELA projections

### Projection data 

#in 1000 m3 stemwood, breakdown by region,soil,species
#soil     1 = mineral   2 = organic
#species  1 = pine      2 = spruce    3 = birch & other decidious

#####stock <-  read.csv(paste(path.data,stock.input, sep=""), header=TRUE)
#stock <-  read.csv(MelaStock, header=TRUE)

stock = MelaStock

##! Dividing by area --> m3/ha

mela.area <- Areas

    
v.min.sp <- stock[stock$soil==1&stock$species==1,3:
                     length(stock[stock$soil==1&stock$species==1,])]*1000/
                     mela.area$area[mela.area$soil==1] 
v.min.ns <- stock[stock$soil==1&stock$species==2,3:
                     length(stock[stock$soil==1&stock$species==2,])]*1000/
                 mela.area$area[mela.area$soil==1]
v.min.dc <- stock[stock$soil==1&stock$species==3,3:
                     length(stock[stock$soil==1&stock$species==3,])]*1000/
  mela.area$area[mela.area$soil==1]



v.org.sp <- stock[stock$soil==2&stock$species==1,3:
                     length(stock[stock$soil==2&stock$species==1,])]*1000/
                     mela.area$area[mela.area$soil==2]
v.org.ns <- stock[stock$soil==2&stock$species==2,3:
                     length(stock[stock$soil==2&stock$species==2,])]*1000/
                     mela.area$area[mela.area$soil==2]
v.org.dc <- stock[stock$soil==2&stock$species==3,3:
                     length(stock[stock$soil==2&stock$species==3,])]*1000/
                     mela.area$area[mela.area$soil==2]




addReg <- (Region - 1) * 3

addReg <- 0





#############
### 1. biomass then litter
#############

##! Lf = litter foliage, Lb = litter branches, Ls = litter stem bark,
##! Lst = litter stump, Lr = litter roots, Lfr = litter fine roots

## Unit t C / ha

## Obs: litter is divided in AWEN -compartment
## Litter (e.g. Lf.sp) is a matrix, columns are A W E N and
## rows are the years on MELA input- note transpose t()

#foliage - mineral soil
Mf.min.sp <- (v.min.sp)* BEF.sp[1+addReg,6]
Mf.min.ns <- (v.min.ns)* BEF.ns[1+addReg,6]
Mf.min.dc <- (v.min.dc)* BEF.dc[1+addReg,6]

Lf.sp <- foliage.AWEN(t(foliage.litter(Mf.min.sp,1,1,1)),1)
Lf.ns <- foliage.AWEN(t(foliage.litter(Mf.min.ns,2,1,1)),2)
Lf.dc <- foliage.AWEN(t(foliage.litter(Mf.min.dc,3,1,1)),3)

Lf.min <- carbon(Lf.sp+Lf.ns+Lf.dc) # here carbon

# foliage - drained org

Mf.org.sp <- (v.org.sp)* BEF.sp[3+addReg,6]
Mf.org.ns <- (v.org.ns)* BEF.ns[3+addReg,6]
Mf.org.dc <- (v.org.dc)* BEF.dc[3+addReg,6]

Lf.sp <- foliage.AWEN(t(foliage.litter(Mf.org.sp,1,1,0)),1)
Lf.ns <- foliage.AWEN(t(foliage.litter(Mf.org.ns,2,1,0)),2)
Lf.dc <- foliage.AWEN(t(foliage.litter(Mf.org.dc,3,1,0)),3)

Lf.org <- carbon(Lf.sp+Lf.ns+Lf.dc) # here carbon 

# branches - mineral soil

Mb.min.sp <- (v.min.sp)* BEF.sp[1+addReg,5]
Mb.min.ns <- (v.min.ns)* BEF.ns[1+addReg,5]
Mb.min.dc <- (v.min.dc)* BEF.dc[1+addReg,5]

Lb.sp <- branches.AWEN(t(branch.litter(Mb.min.sp,1)))/sum(branches.AWEN(1))
Lb.ns <- branches.AWEN(t(branch.litter(Mb.min.ns,2)))/sum(branches.AWEN(1))
Lb.dc <- branches.AWEN(t(branch.litter(Mb.min.dc,3)))/sum(branches.AWEN(1))

Lb.min <- carbon(Lb.sp+Lb.ns+Lb.dc) # here carbon

# branches - drained org

Mb.org.sp <- (v.org.sp)* BEF.sp[3+addReg,5]
Mb.org.ns <- (v.org.ns)* BEF.ns[3+addReg,5]
Mb.org.dc <- (v.org.dc)* BEF.dc[3+addReg,5]

Lb.sp <- branches.AWEN(t(branch.litter(Mb.org.sp,1)))/sum(branches.AWEN(1))
Lb.ns <- branches.AWEN(t(branch.litter(Mb.org.ns,2)))/sum(branches.AWEN(1))
Lb.dc <- branches.AWEN(t(branch.litter(Mb.org.dc,3)))/sum(branches.AWEN(1))

Lb.org <- carbon(Lb.sp+Lb.ns+Lb.dc) # here carbon

#stem bark - mineral soil

Ms.min.sp <- (v.min.sp)* BEF.sp[1+addReg,4]
Ms.min.ns <- (v.min.ns)* BEF.ns[1+addReg,4]
Ms.min.dc <- (v.min.dc)* BEF.dc[1+addReg,4]

#HUOM! stem.AWEN(1,1) does not sum up to 1: sum(stem.AWEN(1,1)) = 0.985
# neither do other (= 0.985, 0.99)

Ls.sp <- stem.AWEN(t(bark.litter(Ms.min.sp,1)),1)/sum(stem.AWEN(1,1))
Ls.ns <- stem.AWEN(t(bark.litter(Ms.min.ns,2)),2)/sum(stem.AWEN(1,2))
Ls.dc <- stem.AWEN(t(bark.litter(Ms.min.dc,3)),3)/sum(stem.AWEN(1,3))

Ls.min <- carbon(Ls.sp+Ls.ns+Ls.dc) # here carbon

# stem bark - drained org

Ms.org.sp <- (v.org.sp)* BEF.sp[3+addReg,4]
Ms.org.ns <- (v.org.ns)* BEF.ns[3+addReg,4]
Ms.org.dc <- (v.org.dc)* BEF.dc[3+addReg,4]

Ls.sp <- stem.AWEN(t(bark.litter(Ms.org.sp,1)),1)/sum(stem.AWEN(1,1))
Ls.ns <- stem.AWEN(t(bark.litter(Ms.org.ns,2)),2)/sum(stem.AWEN(1,2))
Ls.dc <- stem.AWEN(t(bark.litter(Ms.org.dc,3)),3)/sum(stem.AWEN(1,3))

Ls.org <- carbon(Ls.sp+Ls.ns+Ls.dc) # here carbon

# Stump - mineral soil

Mst.min.sp <- (v.min.sp)* BEF.sp[1+addReg,8]
Mst.min.ns <- (v.min.ns)* BEF.ns[1+addReg,8]
Mst.min.dc <- (v.min.dc)* BEF.dc[1+addReg,8]

Lst.sp <- stem.AWEN(t(bark.litter(Mst.min.sp,1)),1)/sum(stem.AWEN(1,1))
Lst.ns <- stem.AWEN(t(bark.litter(Mst.min.ns,2)),2)/sum(stem.AWEN(1,2))
Lst.dc <- stem.AWEN(t(bark.litter(Mst.min.dc,3)),3)/sum(stem.AWEN(1,3))

Lst.min <- carbon(Lst.sp+Lst.ns+Lst.dc) # here carbon

# Stump - drained org

Mst.org.sp <- (v.org.sp)* BEF.sp[3+addReg,8]
Mst.org.ns <- (v.org.ns)* BEF.ns[3+addReg,8]
Mst.org.dc <- (v.org.dc)* BEF.dc[3+addReg,8]

Lst.sp <- stem.AWEN(t(bark.litter(Mst.org.sp,1)),1)/sum(stem.AWEN(1,1))
Lst.ns <- stem.AWEN(t(bark.litter(Mst.org.ns,2)),2)/sum(stem.AWEN(1,2))
Lst.dc <- stem.AWEN(t(bark.litter(Mst.org.dc,3)),3)/sum(stem.AWEN(1,3))

Lst.org <- carbon(Lst.sp+Lst.ns+Lst.dc) # here carbon

## Roots - mineral soil

Mr.min.sp <- (v.min.sp)* BEF.sp[1+addReg,9]
Mr.min.ns <- (v.min.ns)* BEF.ns[1+addReg,9]
Mr.min.dc <- (v.min.dc)* BEF.dc[1+addReg,9]

Lr.sp <- stem.AWEN(t(root.litter(Mr.min.sp,1)),1)/sum(stem.AWEN(1,1))
Lr.ns <- stem.AWEN(t(root.litter(Mr.min.ns,2)),2)/sum(stem.AWEN(1,2))
Lr.dc <- stem.AWEN(t(root.litter(Mr.min.dc,3)),3)/sum(stem.AWEN(1,3))

Lr.min <- carbon(Lr.sp+Lr.ns+Lr.dc) # here carbon

# Roots - drained org

Mr.org.sp <- (v.org.sp)* BEF.sp[3+addReg,9]
Mr.org.ns <- (v.org.ns)* BEF.ns[3+addReg,9]
Mr.org.dc <- (v.org.dc)* BEF.dc[3+addReg,9]

Lr.sp <- stem.AWEN(t(root.litter(Mr.org.sp,1)),1)/sum(stem.AWEN(1,1))
Lr.ns <- stem.AWEN(t(root.litter(Mr.org.ns,2)),2)/sum(stem.AWEN(1,2))
Lr.dc <- stem.AWEN(t(root.litter(Mr.org.dc,3)),3)/sum(stem.AWEN(1,3))

Lr.org <- carbon(Lr.sp+Lr.ns+Lr.dc) # here carbon

## Fineroots - mineral soil

if(Region == 1) {          #Southern Finland
    fol.rat <- FoliageRatioSouth
} else {
    fol.rat <- FoliageRatioSouth
}

################NOTE Fol Rat file here!!!

Mfr.min.sp <- fineroots(((v.min.sp)* BEF.sp[1+addReg,6]*fol.rat[1]),1)
Mfr.min.ns <- fineroots(((v.min.ns)* BEF.ns[1+addReg,6]*fol.rat[3]),2)
Mfr.min.dc <- fineroots(((v.min.dc)* BEF.dc[1+addReg,6]),3)

Lfr.sp <- fineroot.AWEN(t(fineroot.litter(Mfr.min.sp)),1)
Lfr.ns <- fineroot.AWEN(t(fineroot.litter(Mfr.min.ns)),2)
Lfr.dc <- fineroot.AWEN(t(fineroot.litter(Mfr.min.dc)),3)

Lfr.min <- carbon(Lfr.sp+Lfr.ns+Lfr.dc) # here carbons

# Fineroots - drained org

Mfr.org.sp <- fineroots(((v.org.sp)* BEF.sp[3+addReg,6]*fol.rat[2]),1)
Mfr.org.ns <- fineroots(((v.org.ns)* BEF.ns[3+addReg,6]*fol.rat[4]),2)
Mfr.org.dc <- fineroots(((v.org.dc)* BEF.dc[3+addReg,6]),3)

Lfr.sp <- fineroot.AWEN(t(fineroot.litter(Mfr.org.sp)),1)
Lfr.ns <- fineroot.AWEN(t(fineroot.litter(Mfr.org.ns)),2)
Lfr.dc <- fineroot.AWEN(t(fineroot.litter(Mfr.org.dc)),3)

Lfr.org <- carbon(Lfr.sp+Lfr.ns+Lfr.dc) # here carbon


##################
### 3. Summing up litter input 
#################

#Mineral soils
nwl.min <- Lf.min + Lfr.min
fwl.min <- Lb.min + Ls.min + Lst.min + Lr.min

# Drained organic soils
nwl.org.abv <- Lf.org 
nwl.org.bel <- Lfr.org

fwl.org.abv <- Lb.org + Ls.org + Lst.org
fwl.org.bel <- Lr.org

########################
###  4. Interpolating time series for Mineral soils 
########################

index <- MelaYears
    
xout<-seq(from=min(index),to=max(index),by=1)

nwl.min.final <- cbind(approx(index,nwl.min[,1],xout,rule=2)$y,approx(index,nwl.min[,2],xout,rule=2)$y,
                         approx(index,nwl.min[,3],xout,rule=2)$y, approx(index,nwl.min[,4],xout,rule=2)$y)

fwl.min.final <- cbind(approx(index,fwl.min[,1],xout,rule=2)$y,approx(index,fwl.min[,2],xout,rule=2)$y,
                         approx(index,fwl.min[,3],xout,rule=2)$y, approx(index,fwl.min[,4],xout,rule=2)$y)

min <- cbind(xout,nwl.min.final,fwl.min.final)
colnames(min)=c("year","nwl_A","nwl_W","nwl_E","nwl_N","fwl_A","fwl_W","fwl_E","fwl_N")

##!  t C/ha
#This table goes to Yasso (Yasso.R)
TreeLitterYasso <- min
    

########################
###  5. Interpolating time series for drained organic soils 
########################

nwl.org.abv.final <- cbind(approx(index,nwl.org.abv[,1],xout)$y,approx(index,nwl.org.abv[,2],xout)$y,
                              approx(index,nwl.org.abv[,3],xout)$y, approx(index,nwl.org.abv[,4],xout)$y)
fwl.org.abv.final <- cbind(approx(index,fwl.org.abv[,1],xout)$y,approx(index,fwl.org.abv[,2],xout)$y,
                              approx(index,fwl.org.abv[,3],xout)$y, approx(index,fwl.org.abv[,4],xout)$y)

nwl.org.bel.final <- cbind(approx(index,nwl.org.bel[,1],xout)$y,approx(index,nwl.org.bel[,2],xout)$y,
                              approx(index,nwl.org.bel[,3],xout)$y, approx(index,nwl.org.bel[,4],xout)$y)
fwl.org.bel.final <- cbind(approx(index,fwl.org.bel[,1],xout)$y,approx(index,fwl.org.bel[,2],xout)$y,
                              approx(index,fwl.org.bel[,3],xout)$y, approx(index,fwl.org.bel[,4],xout)$y)

org.abv <- cbind(xout,nwl.org.abv.final,fwl.org.abv.final)
org.bel <- cbind(xout,nwl.org.bel.final,fwl.org.bel.final)

colnames(org.abv)=c("year","nwl_A","nwl_W","nwl_E","nwl_N","fwl_A","fwl_W","fwl_E","fwl_N")
colnames(org.bel)=c("year","nwl_A","nwl_W","nwl_E","nwl_N","fwl_A","fwl_W","fwl_E","fwl_N")

#These intermdiate files are used in organic soils calculations

OrganicSoilBelowLitter <- org.bel 

########################
###  5. Litter for output
########################

years <- seq(from = min(index), to = max(index), by = 1)

Amin = mela.area$area[mela.area$soil==1]
Aorg = mela.area$area[mela.area$soil==2]

litter.mineral<-cbind(years,approx(index,rowSums(Amin*Lf.min),years)$y,
                            approx(index,rowSums(Amin*Lb.min),years)$y,
                            approx(index,rowSums(Amin*Ls.min),years)$y,
                            approx(index,rowSums(Amin*Lst.min),years)$y,
                            approx(index,rowSums(Amin*Lr.min),years)$y,
                            approx(index,rowSums(Amin*Lfr.min),years)$y)

litter.organic<-cbind(approx(index,rowSums(Aorg*Lf.org),years)$y,
                            approx(index,rowSums(Aorg*Lb.org),years)$y,
                            approx(index,rowSums(Aorg*Ls.org),years)$y,
                            approx(index,rowSums(Aorg*Lst.org),years)$y,
                            approx(index,rowSums(Aorg*Lr.org),years)$y,
                            approx(index,rowSums(Aorg*Lfr.org),years)$y)

litt <- data.frame(cbind(litter.mineral,litter.organic))
colnames(litt) <- c("year","mlitfol","mlitbra","mlitstem","mlitstump","mlitroots","mlitfr",
                          "olitfol","olitbra","olitstem","olitstump","olitroots","olitfr")


