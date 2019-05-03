
list.of.pkgs <-  c("readr","dplyr","zoo","ggplot2", "reshape2", "data.table",
                   "future","listenv","readxl","purrr","e1071" ,"rcompanion","tictoc")

new.pkgs <- list.of.pkgs[!(list.of.pkgs %in% installed.packages()[, "Package"])]

if (length(new.pkgs)){ install.pkgs(new.pkgs) }
for (pkg in list.of.pkgs){ library(pkg,character.only = TRUE) }

#-------------------
#
# Read preprocessed P80 data sets
#
#-------------------
workdir= "Y:/proj/CFWI_WetlandStress/Update2018"
setwd(workdir)

SFWMD_P80b <- read_csv("./SFWMD/SFWMD_P80.csv")
SWFWMD_P80b <- read_csv("./SWFWMD/SWFWMD_P80.csv")
SJRWMD_P80b <- read_csv("./SJRWMD/SJRWMD_P80.csv")
AllP80 <-bind_rows(SFWMD_P80b,SWFWMD_P80b,SJRWMD_P80b)

EMT_ID <- read_csv("EMT_ID.csv")
AllP80 <-merge(EMT_ID,AllP80)
write.csv(AllP80,file='AllP80.csv',row.names=FALSE)

Class1Wetlands <- read_excel("Class 1 Wetland Info for Analysis ALLv1.xlsx", na = "NA")
Class1P80 <-merge(Class1Wetlands,AllP80, by.x='CFCA/EMT ID', by.y='EMT_ID')    
# Remove redundant 2006-2017_P80
Class1P80$`2006-2017_P80.y`<-NULL
names(Class1P80)[names(Class1P80)=='2006-2017_P80.x']<-"2006-2017_P80"
names(Class1P80)

#-------------------
#
# Calculate thetas
#
#-------------------
thetas = data.frame()
strStr <- "Stress Status in 2018"
physStr <- "Physiographic Region"

physVec <-c("Plain","Ridge")
stressVec <- c("Stressed","Not Stressed")
ranks = "2009-2017_P80"
rankVec <- c( "2009-2017_P80" )

for (ranks in rankVec) {
  theta = Class1P80$"Edge Reference Elevation (ft NAVD 88)" - Class1P80[,ranks]
  thetas = rbind(thetas,cbind.data.frame(EMT_ID=Class1P80$`CFCA/EMT ID`, rank=ranks,theta=as.numeric(theta)))
}
thetas <- merge(thetas,Class1P80[,c(1,3,4,5)], by.x='EMT_ID', by.y = "CFCA/EMT ID")

names(thetas)[names(thetas) == strStr] <-"Stress"
names(thetas)[names(thetas) == physStr] <-"phys"

#----------------------------------------------------------------------------
# Fs and Fu are fraction of stressed wetlands and unstressed wetlands		Equations: 10 & 11
#----------------------------------------------------------------------------
thetas$Fu = NA
thetas$Fs = NA
thetas$mean = NA
thetas$sd = NA
# #                                         Current Observed Percentages
# thetas[thetas$phys=='Ridge',]$Fu <-0.606  # .6785714
# thetas[thetas$phys=='Ridge',]$Fs <-0.394  # .3214286
# thetas[thetas$phys=='Plain',]$Fu <-0.824  # .75
# thetas[thetas$phys=='Plain',]$Fs <-0.176  # .25

for (phys in physVec) {
  
  #----------------------------------------------------------------------------
  # identify number of stressed vs unstressed and total for each physiographic type  
  #----------------------------------------------------------------------------
  stressKnt <- nrow(thetas[thetas$phys== phys 
                           & thetas$Stress == "Stressed",])
  UstressKnt <- nrow(thetas[thetas$phys== phys 
                            & thetas$Stress == "Not Stressed",])
  allKnt <- nrow(thetas[thetas$phys== phys ,])
  
  #----------------------------------------------------------------------------
  # Fs and Fu are fraction of stressed wetlands and unstressed wetlands		Equations: 10 & 11
  #----------------------------------------------------------------------------
  thetas[thetas$phys==phys,]$Fs <- stressKnt/allKnt
  thetas[thetas$phys==phys,]$Fu <- UstressKnt/allKnt
  
}
#Class 1
#  SFsu = 1.0  SFus = 1.0
#Class 2
#  SFsu = 1.0  SFus = 1.0
#Class 3
# phys   Urban      DisSim   SHA   sf_us  sf_su SFus  SFsu
#------ ----------  ------  -----  -----  ----- ----- -----
# Plain	low	        0.694	  0.82	 0.824	0.176	0.469	0.100
# Plain	Mod & High	0.616	  0.581	 0.824	0.176	0.295	0.063
# Ridge	All	        0.671	  1	     0.581	0.419	0.390	0.281
thetas$theta.logN <-NA

#----------------------------------------------------------------------------
# transform data by subsets using:
#     phys- Physiographic Region (Ridge or Plain)
#     stress- Wetland Stress Status in 2018, 
#----------------------------------------------------------------------------

for (phys in physVec) {
  for (stress in stressVec) {
    #----------------------------------------------------------------------------
    # mean and sd are provided for probability density function for the selected
    # physiographic region type and initial Stress Status in 2018
    #----------------------------------------------------------------------------
    thetas[thetas$Stress == stress & thetas$phys == phys, ]$mean <-
      mean(thetas[thetas$Stress == stress & thetas$phys == phys, ]$theta)
    thetas[thetas$Stress == stress & thetas$phys == phys, ]$sd <-
      sd(thetas[thetas$Stress == stress & thetas$phys == phys, ]$theta)
  }
}
thetaInterval = .1
thetaSeq<-seq(-25,25,thetaInterval)
deltas <- seq(-20, 15,thetaInterval)

Plain<- as.data.frame(thetaSeq)
names(Plain) <-c('theta')
newColumns <-c('phys','Ppu','Ps','Pu','Pps','PpAll','PsiU','PsiS')
Plain[newColumns]<-0.0
Plain$phys <- "Plain"

Ridge<- as.data.frame(thetaSeq)
names(Ridge) <-c('theta')
Ridge[newColumns]<-0.0
Ridge$phys <- "Ridge"

Wetlands <-rbind(Plain,Ridge)

#----------------------------------------------------------------------------
# dnorm function returns probability from density function at each theta value  Equations: 12 & 13
#----------------------------------------------------------------------------
for (phys in physVec) {
  if (phys == 'Plain') {
   Mean <- max(thetas[thetas$Stress =="Stressed" & thetas$phys==phys,]$mean)
   SD   <- max(thetas[thetas$Stress =="Stressed" & thetas$phys==phys,]$sd)
    # Mean <- 5.18
    # SD <- 1.75
    cat(paste("Stressed",phys,'Mean=',round(Mean,2),'StdDev=',round(SD,4)))
    cat('\n')
    Wetlands[Wetlands$phys == phys,]$Ps <- dnorm(Wetlands[Wetlands$phys == phys,]$theta, Mean, SD)
    
   Mean <- max(thetas[thetas$Stress =="Not Stressed" & thetas$phys==phys,]$mean)
   SD   <- max(thetas[thetas$Stress =="Not Stressed" & thetas$phys==phys,]$sd)
    # Mean <- 2.73
    # SD <- 0.95
    cat(paste("Not Stressed",phys,'Mean=',round(Mean,2),'StdDev=',round(SD,4)))
    cat('\n')
    
    Wetlands[Wetlands$phys == phys,]$Pu <- dnorm((Wetlands[Wetlands$phys == phys,]$theta), Mean, SD)
  }
  else if (phys == 'Ridge')
  {
   Mean <- max(thetas[thetas$Stress =="Stressed" & thetas$phys==phys,]$mean)
   SD   <- max(thetas[thetas$Stress =="Stressed" & thetas$phys==phys,]$sd)
    # Mean <- 7.86  
    # SD <- 2.55
    cat(paste("Stressed",phys,'Mean=',round(Mean,2),'StdDev=',round(SD,4)))
    cat('\n')
    
    Wetlands[Wetlands$phys == phys,]$Ps <- dnorm(Wetlands[Wetlands$phys == phys,]$theta,Mean, SD)

   Mean <- max(thetas[thetas$Stress =="Not Stressed" & thetas$phys==phys,]$mean)
   SD   <- max(thetas[thetas$Stress =="Not Stressed" & thetas$phys==phys,]$sd)
    # Mean <- 3.42  
    # SD <- 1.57
    cat(paste("Not Stressed",phys,'Mean=',round(Mean,2),'StdDev=',round(SD,4)))
    cat('\n')
    
    Wetlands[Wetlands$phys == phys,]$Pu <- dnorm(Wetlands[Wetlands$phys == phys,]$theta,Mean, SD)
  }
  
  #----------------------------------------------------------------------------
  # Pps and Ppu are Population-weighted contributions of stress and unstress
  # wetlands to the total population probability density of all wetlands at 
  # each wetland hydrologic index (theta)                                 Equations: 14 & 15
  #----------------------------------------------------------------------------
  Wetlands[Wetlands$phys == phys,]$Ppu <-Wetlands[Wetlands$phys == phys,]$Pu*max(thetas[thetas$phys==phys,]$Fu)
  Wetlands[Wetlands$phys == phys,]$Pps <-Wetlands[Wetlands$phys == phys,]$Ps*max(thetas[thetas$phys==phys,]$Fs)

  Wetlands[Wetlands$phys == phys,]$PpAll <-Wetlands[Wetlands$phys == phys,]$Ppu + Wetlands[Wetlands$phys == phys,]$Pps    
  #----------------------------------------------------------------------------
  # PsiU and PsiS Probability weighted Cumulative Probability             Equation 17 & 18
  #----------------------------------------------------------------------------
  Wetlands[Wetlands$phys == phys,]$PsiU <- Wetlands[Wetlands$phys == phys,]$Ppu /Wetlands[Wetlands$phys == phys,]$PpAll
  Wetlands[Wetlands$phys == phys,]$PsiS <- Wetlands[Wetlands$phys == phys,]$Pps /Wetlands[Wetlands$phys == phys,]$PpAll
}

write.csv(file='h:/Wetlands.csv',Wetlands)

#----------------------------------------------------------------------------
#   Returns stress appropriate PsiValue lookup from Wetlands Table 
#   using theta and final theta (or theta+delta)
#
#   type is not key, but used to subset data enable better performance
#   with multiple processors
#----------------------------------------------------------------------------
PsiVals <- function(type, status, hydIndex) {
  val <- round(hydIndex,2)
  if (status == 'Not Stressed' & !is.na(val)) {
    retVal<-(Wetlands[Wetlands$phys == type &
                        val == round(Wetlands[Wetlands$phys == type,]$theta, 2), ]$PsiU)
  }  else if (status == 'Stressed' & !is.na(val)) {
    retVal<-(Wetlands[Wetlands$phys == type &
                        val == round(Wetlands[Wetlands$phys == type,]$theta, 2),]$PsiS)
  }  else
  {
    retVal<-NA
  }
}

#----------------------------------------------------------------------------
#   Vectorize function to work with dataframes input
#----------------------------------------------------------------------------
vPsiVals <- Vectorize(PsiVals)

#----------------------------------------------------------------------------
# Function used to calculate zetas 
#----------------------------------------------------------------------------
makeZetas <- function(phys,stress,deltas,thetaSeq) {
  z = matrix(NA,length(thetaSeq),1+length(deltas))
  z[,1] <- vdf[,1]
  for (i in seq(2,1+length(deltas))){
    psiTheta2 <-unname(unlist(vPsiVals(phys,stress,vdf[,i])))
    psiTheta1 <-unname(vPsiVals(phys,stress,vdf[,1]))
    z[,i] = 1 -   ( psiTheta2/psiTheta1)
    z[is.nan(z[,i]) ,i] <- NA
    z[z[,i]<0,i] <- 0
    z[z[,i]>1,i] <- NA
  }
  StressZetas<- as.data.frame(cbind(phys,stress,z,
                                    Wetlands[Wetlands$phys==phys,]$Ps,
                                    Wetlands[Wetlands$phys==phys,]$Pu))
  names(StressZetas) <- c("phys","stress","theta",deltas,"Ps","Pu")
  cat(paste('Zetas Calculated for',stress, phys,'\n'))
  return(StressZetas)
}

# Define matrix/dataframe for intial and examples of possible thetas

vdf = c()
for (x in thetaSeq) {
  possibleThetas<- deltas+x
  vdf<-c(vdf,possibleThetas)
}

dim(vdf)<-c(length(deltas),length(thetaSeq))
vdf <- t(vdf)
vdf[vdf< min(thetaSeq)]<-NA
vdf[vdf> max(thetaSeq)]<-NA

# Add theta column to beginning 
vdf <-cbind(Wetlands[1:length(thetaSeq),]$theta,vdf)

physVec = c('Ridge','Plain')
stressVec = c('Not Stressed','Stressed')
ix = 0

plan(multiprocess)
data <- listenv()
#----------------------------------------------------------------------------
#   Create zetas using multiprocessing functions
#----------------------------------------------------------------------------
tic("Calculate Zetas")
for (phys in physVec){
  for (stress in stressVec){
    cat(paste(phys, stress, '\n'))
    ix = ix + 1
    data[[ix]] %<-% makeZetas(phys,stress,deltas,thetaSeq)
  }
}
xdata <- as.list(data)
zetas<- do.call(rbind,xdata)
zetaMelt <- melt(zetas,id=c("phys","stress","theta","Ps","Pu"),na.rm=T)
zetaMelt <-transform(zetaMelt, theta = as.numeric(as.character(theta)))
zetaMelt <-transform(zetaMelt, value = as.numeric(value))
names(zetaMelt) <- c("phys","stress","theta","Ps","Pu","DeltaTheta","zeta")

zetaMelt <-transform(zetaMelt, Ps = as.numeric(as.character(Ps)))
zetaMelt <-transform(zetaMelt, Pu = as.numeric(as.character(Pu)))

zetaMelt$ZetaSU <- NA
zetaMelt[zetaMelt$stress == 'Not Stressed',]$ZetaSU<- thetaInterval*
  zetaMelt[zetaMelt$stress == 'Stressed',]$zeta * 
  zetaMelt[zetaMelt$stress == 'Stressed',]$Ps

zetaMelt$ZetaUS <- NA
zetaMelt[zetaMelt$stress == 'Not Stressed',]$ZetaUS<- thetaInterval*
  zetaMelt[zetaMelt$stress == 'Not Stressed',]$zeta * 
  zetaMelt[zetaMelt$stress == 'Not Stressed',]$Pu

toc()
workdir= "Y:/proj/CFWI_WetlandStress/Update2018"
write.csv(file=paste0(workdir,'/Zetas.csv'),zetas,row.names=FALSE)
write.csv(file=paste0(workdir,'/ZetasMelt.csv'),zetaMelt)
write.csv(file=paste0(workdir,'/Wetlands.csv'),Wetlands)


wideTheta <- dcast(thetas,EMT_ID~rank,value.var='theta',mean)
thetaEval <- merge(wideTheta,Class1P80[,c(1,3,4,5,10,11,12)], by.x='EMT_ID', by.y = "CFCA/EMT ID")
write.csv(file=paste0(workdir,'/thetas4Eval.csv'),thetaEval)
write.csv(file=paste0(workdir,'/thetas.csv'),thetas)

OrigwideTheta<- dcast(thetas,EMT_ID~rank,value.var='theta',mean)
OrigTheta <-merge(OrigwideTheta,Class1P80[,c(1,3,4,5,10,11,12)], by.x='EMT_ID', by.y = "CFCA/EMT ID")

workdir= "Y:/proj/CFWI_WetlandStress/Update2018"


