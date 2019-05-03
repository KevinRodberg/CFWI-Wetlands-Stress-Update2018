
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
#                                         Current Observed Percentages
thetas[thetas$phys=='Ridge',]$Fu <-0.606  # .65517
thetas[thetas$phys=='Ridge',]$Fs <-0.394  # .34483
thetas[thetas$phys=='Plain',]$Fu <-0.824  # .7742
thetas[thetas$phys=='Plain',]$Fs <-0.176  # .2258
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

#----------------------------------------------------------------------------
# transform data by subsets using:
#     phys- Physiographic Region (Ridge or Plain)
#     stress- Wetland Stress Status in 2018, 
#----------------------------------------------------------------------------


for (phys in physVec) {
  #----------------------------------------------------------------------------
  # "Not Stressed Plains" thetas are transformed with a log function 
  #     to provide a more normal distribution 
  #----------------------------------------------------------------------------
  # mean and sd are provided for probability density function for the selected
  # physiographic region type and initial Stress Status in 2018
  #----------------------------------------------------------------------------
  if (phys == "Plain") {
    # Shift Plains thetas prior to log calculations
    
    swTest <- shapiro.test(thetas[thetas$phys ==phys & thetas$Stress == 'Not Stressed',]$theta)
    cat (paste0('"shapiro.test for Not Stressed,"',phys,'",'))
    cat(paste0(swTest$statistic, '  ', swTest$p.value, '\n'))
    swTest <- shapiro.test(thetas[thetas$phys ==phys & thetas$Stress == 'Stressed',]$theta)
    cat (paste0('"shapiro.test for Stressed,"',phys,'",'))
    cat(paste0(swTest$statistic, '  ', swTest$p.value, '\n'))
    
    swTest <- shapiro.test(log(thetas[thetas$phys ==phys & thetas$Stress == 'Not Stressed',]$theta))
    cat (paste0('"shapiro.test for log Not Stressed,"',phys,'",'))
    cat(paste0(swTest$statistic, '  ', swTest$p.value, '\n'))
    swTest <- shapiro.test(log(thetas[thetas$phys ==phys & thetas$Stress == 'Stressed',]$theta))
    cat (paste0('"shapiro.test for log Stressed,"',phys,'",'))
    cat(paste0(swTest$statistic, '  ', swTest$p.value, '\n'))
    
    thetas[thetas$phys ==phys & thetas$Stress == 'Not Stressed',]$theta <-
      (thetas[thetas$phys == phys & thetas$Stress == 'Not Stressed',]$theta+16)
    thetas[thetas$phys ==phys & thetas$Stress == 'Stressed',]$theta <-
      thetas[thetas$phys == phys & thetas$Stress == 'Stressed',]$theta+16

    swTest <- shapiro.test(log(thetas[thetas$phys ==phys & thetas$Stress == 'Not Stressed',]$theta))
    cat (paste0('"shapiro.test for log+16 Not Stressed,"',phys,'",'))
    cat(paste0(swTest$statistic, '  ', swTest$p.value, '\n'))
    swTest <- shapiro.test(log(thetas[thetas$phys ==phys & thetas$Stress == 'Stressed',]$theta))
    cat (paste0('"shapiro.test for log+16 Stressed,"',phys,'",'))
    cat(paste0(swTest$statistic, '  ', swTest$p.value, '\n'))
    
    # Perform log statistics for just Plains
    thetas[thetas$phys ==phys & thetas$Stress == 'Not Stressed',]$mean <-
      mean(log(thetas[thetas$phys == phys & thetas$Stress == 'Not Stressed',]$theta))   
    thetas[thetas$phys ==phys & thetas$Stress == 'Not Stressed',]$sd <-
      sd(log(thetas[thetas$phys == phys & thetas$Stress == 'Not Stressed',]$theta))
    
    thetas[thetas$phys ==phys & thetas$Stress == 'Stressed',]$mean <-
      mean(log(thetas[thetas$phys == phys & thetas$Stress == 'Stressed',]$theta))
    thetas[thetas$phys ==phys & thetas$Stress == 'Stressed',]$sd <-
      sd(log(thetas[thetas$phys == phys & thetas$Stress == 'Stressed',]$theta))
    
  }
  else 
  {
    swTest <- shapiro.test(thetas[thetas$phys ==phys & thetas$Stress == 'Not Stressed',]$theta)
    cat (paste0('"shapiro.test for Not Stressed,"',phys,'",'))
    cat(paste0(swTest$statistic, '  ', swTest$p.value, '\n'))
    swTest <- shapiro.test(thetas[thetas$phys ==phys & thetas$Stress == 'Stressed',]$theta)
    cat (paste0('"shapiro.test for Stressed,"',phys,'",'))
    cat(paste0(swTest$statistic, '  ', swTest$p.value, '\n'))
    
    # standard statistics for Ridges
    thetas[thetas$phys ==phys & thetas$Stress == 'Not Stressed',]$mean <-
      mean(thetas[thetas$phys == phys & thetas$Stress == 'Not Stressed',]$theta)       
    thetas[thetas$phys ==phys & thetas$Stress == 'Stressed',]$mean <-
        mean(thetas[thetas$phys == phys & thetas$Stress == 'Stressed',]$theta)   
    
    thetas[thetas$phys ==phys& thetas$Stress == 'Not Stressed',]$sd <-
      sd(thetas[thetas$phys == phys& thetas$Stress == 'Not Stressed',]$theta)    
    thetas[thetas$phys ==phys& thetas$Stress == 'Stressed',]$sd <-
      sd(thetas[thetas$phys == phys& thetas$Stress == 'Stressed',]$theta)
  }
}
thetaSeq<-seq(-15,25.0,.05)
deltas <- seq(-10, 10, .5)

Plain<- as.data.frame(thetaSeq)
names(Plain) <-c('theta')
Plain$phys <- "Plain"
Plain$Ppu <-0.0
Plain$Ps<-0.0
Plain$Pu<-0.0
Plain$Pps <-0.0
Plain$PpAll <-0.0 
Plain$PsiU <- 0.0
Plain$PsiS <- 0.0

Ridge<- as.data.frame(thetaSeq)
names(Ridge) <-c('theta')
Ridge$phys <- "Ridge"
Ridge$Ppu <-0.0
Ridge$Ps<-0.0
Ridge$Pu<-0.0
Ridge$Pps <-0.0
Ridge$PpAll <-0.0 
Ridge$PsiU <- 0.0
Ridge$PsiS <- 0.0

Wetlands <-rbind(Plain,Ridge)

#----------------------------------------------------------------------------
# dnorm & dlnorm functions returns probability from density function at each theta value  Equations: 12 & 13
#----------------------------------------------------------------------------
for (phys in physVec) {
  if (phys == 'Plain') {
    Mean <- max(thetas[thetas$Stress =="Stressed" & thetas$phys==phys,]$mean)
    SD   <- max(thetas[thetas$Stress =="Stressed" & thetas$phys==phys,]$sd)
    # Mean <- 5.18
    # SD <- 1.75

    # shift example Wetlands dataframe thetas by 16 to avoid log of zeros or negative numbers
    Wetlands[Wetlands$phys == phys,]$theta <- Wetlands[Wetlands$phys == phys,]$theta + 16
    
    # Wetlands[Wetlands$phys == phys,]$Ps <- dlnorm(Wetlands[Wetlands$phys == phys,]$theta, location, shape,log=TRUE)
    Wetlands[Wetlands$phys == phys,]$Ps <- dlnorm(Wetlands[Wetlands$phys == phys,]$theta, Mean, SD,log=FALSE)
    cat(paste("Stressed",phys,'Mean=',round(Mean,2),'StdDev=',round(SD,4)))
    cat('\n')
    
    Mean <- max(thetas[thetas$Stress =="Not Stressed" & thetas$phys==phys,]$mean)
    SD   <- max(thetas[thetas$Stress =="Not Stressed" & thetas$phys==phys,]$sd)
    # Mean <- 2.73
    # SD <- 0.95
    # Wetlands[Wetlands$phys == phys,]$Pu <- dlnorm(Wetlands[Wetlands$phys == phys,]$theta, location, shape,log=TRUE)
    Wetlands[Wetlands$phys == phys,]$Pu <- dlnorm(Wetlands[Wetlands$phys == phys,]$theta, Mean, SD,log=FALSE)
    cat(paste("Not Stressed",phys,'Mean=',round(Mean,2),'StdDev=',round(SD,4)))
    cat('\n')
  }
  else if (phys == 'Ridge')
  {
    Mean <- max(thetas[thetas$Stress =="Stressed" & thetas$phys==phys,]$mean)
    SD   <- max(thetas[thetas$Stress =="Stressed" & thetas$phys==phys,]$sd)
    # Mean <- 7.86  
    # SD <- 2.55
    Wetlands[Wetlands$phys == phys,]$Ps <- dnorm(Wetlands[Wetlands$phys == phys,]$theta,Mean, SD)
    cat(paste("Stressed",phys,'Mean=',round(Mean,2),'StdDev=',round(SD,4)))
    cat('\n')
    
    Mean <- max(thetas[thetas$Stress =="Not Stressed" & thetas$phys==phys,]$mean)
    SD   <- max(thetas[thetas$Stress =="Not Stressed" & thetas$phys==phys,]$sd)
    # Mean <- 3.42  
    # SD <- 1.57
    Wetlands[Wetlands$phys == phys,]$Pu <- dnorm(Wetlands[Wetlands$phys == phys,]$theta,Mean, SD)
    cat(paste("Not Stressed",phys,'Mean=',round(Mean,2),'StdDev=',round(SD,4)))
    cat('\n')
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
  
  # shift example hydIndex by 16 to match shifted Plains thetas used
  # avoid log of zeros or negative numbers
  if (type == 'Plain'){ val <- val + 16}
  if (status == 'Not Stressed' & !is.na(val)) {
    retVal<-(Wetlands[Wetlands$phys == type &
                        val == round(Wetlands[Wetlands$phys == type,]$theta, 2), ]$PsiU)
  }
  else if (status == 'Stressed' & !is.na(val)) {
    retVal<-(Wetlands[Wetlands$phys == type &
                        val == round(Wetlands[Wetlands$phys == type,]$theta, 2),]$PsiS)
  }
  else
  {
    retVal<- NA
  }
  if (identical(retVal,numeric(0))){
    return(0)
  }
  else {
    return(retVal)
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
    psiTheta2 <-(vPsiVals(phys,stress,vdf[,i]))
    psiTheta2[psiTheta2 <= 0]<- NA
    psiTheta1 <-(vPsiVals(phys,stress,vdf[,1]))
    psiTheta1[psiTheta1 <= 0]<- NA
    
    z[,i] = 1 -   ( psiTheta2/psiTheta1)
    z[is.nan(z[,i]) ,i] <- NA
    z[z[,i]<0,i] <- NA
    z[z[,i]>1,i] <- NA
  }
  StressZetas<- as.data.frame(cbind(phys,stress,z,
                                    Wetlands[Wetlands$phys==phys,]$Ps,
                                    Wetlands[Wetlands$phys==phys,]$Pu))
  #deltaNames <- sprintf("delta_%s",deltas)
  names(StressZetas) <- c("phys","stress","theta",deltas,"Ps","Pu")
  cat(paste('Zetas Calculated for',stress, phys,'\n'))
  return(StressZetas)
}


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
  vdf = c()
  for (x in thetaSeq) {
    possibleThetas<- deltas+x
    vdf<-c(vdf,possibleThetas)
  }
  dim(vdf)<-c(length(deltas),length(thetaSeq))
  vdf <- t(vdf)
  vdf[vdf< min(thetaSeq)]<-NA
  vdf[vdf> max(thetaSeq)]<-NA

  vdf <-cbind(Wetlands[Wetlands$phys==phys,]$theta,vdf)

  for (stress in stressVec){
    cat(paste(phys, stress, '\n'))
    ix = ix + 1
    data[[ix]] %<-% makeZetas(phys,stress,deltas,thetaSeq)
  }
}
xdata <- as.list(data)
zetas<- do.call(rbind,xdata)

# reshift example Wetlands dataframe thetas by -16
zetas[zetas$phys=='Plain',]$theta <- 
  round(as.numeric(as.character(zetas[zetas$phys=='Plain',]$theta)) - 16.0,2)
zetaMelt <- melt(zetas,id=c("phys","stress","theta","Ps","Pu"),na.rm=T)
toc()
write.csv(file='h:/Zetas.csv',zetas,row.names=FALSE)
write.csv(file='h:/ZetasMelt.csv',zetaMelt)
write.csv(file='h:/Wetlands.csv',Wetlands)


wideTheta <- dcast(thetas,EMT_ID~rank,value.var='theta',mean)
thetaEval <- merge(wideTheta,Class1P80[,c(1,3,4,5,11)], by.x='EMT_ID', by.y = "CFCA/EMT ID")
write.csv(file='h:/thetas4Eval.csv',thetaEval)
write.csv(file='h:/thetasTransformed.csv',thetas)

OrigwideTheta<- dcast(thetas,EMT_ID~rank,value.var='theta',mean)
OrigTheta <-merge(OrigwideTheta,Class1P80[,c(1,3,4,5,11)], by.x='EMT_ID', by.y = "CFCA/EMT ID")

workdir= "Y:/proj/CFWI_WetlandStress/Update2018"


