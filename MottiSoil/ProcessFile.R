# This reads input for calculation of stock changes of soil carbon of forest areas.
# The data read are:
# Years when tree stock values are available, defining also the simulation period (from first to last)
# Areas for which calculations are done (upland soil, drained peatland, and also undrained peatland),
# 1000 ha
# Tree stock 1000 m3 stemwood 
# Amount of natural mortality 1000 m3 / year of stemwood (between stock years)
# Amount of harvest waste (beteen stock years) 1000 t dry matter / year

readAllLines = function(filepath, lines) {
  con = file(filepath, "r")
  i = 0
  while ( TRUE ) {
    line = readLines(con, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    i = i + 1
    lines[i] = line
  }

  close(con)
  lines
}


filepath = InputFile

n_lines = 72
if(InputFileDebug) {
    print(paste("The file has ", as.character(n_lines+1), " lines.", sep = ""))
}

lines <- 1:n_lines
lines <- readAllLines(filepath, lines)

#Fixed structure of the input file is assumed. There are 72 line in the file.
# years are at line 2
# Area information starts at line 4
# Stock values start at line 8
# Natural mortality values start at line 18
# Harvest waste values start at line 28


## Years
tcon <- textConnection(lines[2])
ddd <-read.table(tcon)
close(tcon)
MelaYears <- as.numeric(ddd[1,])
nYears <- length(MelaYears)


if(InputFileDebug) {
    print("MelaYears")
    print(MelaYears)
}

## AREAS

tcon <- textConnection(lines[4])
ddd <-read.table(tcon)
close(tcon)
upland <- as.numeric(ddd[1,2:length(ddd[1,])])
tcon <- textConnection(lines[5])
ddd <-read.table(tcon)
close(tcon)
undrained <- as.numeric(ddd[1,2:length(ddd[1,])])
tcon <- textConnection(lines[6])
ddd <-read.table(tcon)
close(tcon)
drained <- as.numeric(ddd[1,2:length(ddd[1,])])


Areas <- data.frame(rbind(c(1,upland[1]*1000),c(2,drained[1]*1000),c(3,1000*undrained[1])))          
colnames(Areas)<-c("soil","area")

if(InputFileDebug) {
        print("AREAS")
        print(Areas)
}


#nYears <- length(MelaYears)


## STOCK
# There are 9 tree - site combinations

cNames <- c("soil", "species")
for(i in 1:nYears) {
	cNames <- c(cNames, paste("X",as.character(MelaYears[i]),sep=""))
}

st <- data.frame(t(rep(0,times=length(MelaYears)+2))) 
colnames(st) <- cNames
st <- st[-1,]       #empty

stockRow = 8

for(i in 0:8) {
	stock <- 1:nYears
	tcon <- textConnection(lines[stockRow+i])
	ddd <-read.table(tcon)
	close(tcon)
	soil <- ddd[1,1]
	species <- ddd[1,2]
	for(j in 1:nYears) {
	stock[j] = ddd[1,2+j]		
	}
	lo = length(st[,1])
	st[lo+1,] = c(soil, species, stock)	
}


if(InputFileDebug) {
     print("First stock line ")
     print(st[1,])
}


## Stem volume of natural mortality (1000 m3 / year)
# There are 9 tree - site combinations for Southern and Northern Finland
# Mortality is given as m3/year between two points of time. There are thus one less
# values of mortality than stock 

cNames <- c("soil", "species")
for(i in 1:(nYears-1)) {
	cNames <- c(cNames, paste("X",as.character(MelaYears[i]),sep=""))
}

mort <- data.frame(t(rep(0,times=length(MelaYears)-1+2))) 
colnames(mort) <- cNames
mort <- mort[-1,]       #empty

mortRow = 18

#Southern Finland == alku[1]

for(i in 0:8) {
	mstock <- 1:(nYears-1)
	tcon <- textConnection(lines[mortRow+i])
	ddd <-read.table(tcon)
	close(tcon)
	soil <- ddd[1,1]
	species <- ddd[1,2]
	for(j in 1:nYears-1) {
	mstock[j] = ddd[1,2+j]		
	}
	lo = length(mort[,1])
	mort[lo+1,] = c(soil, species, mstock)	
}


if(InputFileDebug) {
     print("First Natural mortality line ")
     print(mort[1,])
}


## Cutting waste (1000 t DM / year)
# There are 45 	soil, species, part combinations for Southern and Northern Finland
# Mortality is given as t DM / year between two points of time. There are thus one less
# values of mortality than stock 

cNames <- c("soil", "species", "part")
for(i in 1:(nYears-1)) {
	cNames <- c(cNames, paste("X",as.character(MelaYears[i]),sep=""))
}

waste <- data.frame(t(rep(0,times=length(MelaYears)-1+3))) 
colnames(waste) <- cNames
waste <- waste[-1,]       #empty

wasteRow = 28

for(i in 0:44) {
	wb <- 1:(nYears-1)
	tcon <- textConnection(lines[wasteRow+i])
	ddd <-read.table(tcon)
	close(tcon)
	soil <- ddd[1,1]
	species <- ddd[1,2]
	part <- ddd[1,3]
	for(j in 1:nYears-1) {
	wb[j] = ddd[1,3+j]		
	}
	lo = length(waste[,1])
	waste[lo+1,] = c(soil, species, part, wb)	
}


if(InputFileDebug) {
 print("First harvest waste line ")
print(waste[1,])
}


MelaStock <- st
MelaNatMortality <- mort
MelaWaste <- waste
