
list.of.pkgs <-  c("readr","dplyr","zoo","ggplot2", "reshape2", "data.table",
                   "future","listenv","readxl","purrr","e1071" ,"rcompanion","tictoc")

new.pkgs <- list.of.pkgs[!(list.of.pkgs %in% installed.packages()[, "Package"])]

if (length(new.pkgs)){ install.pkgs(new.pkgs) }
for (pkg in list.of.pkgs){ library(pkg,character.only = TRUE) }

#-------------------
#
#  Define plotting functions
#
#-------------------
HistoDens <- function(One, ranks,phys,source){
  graphics.off
  filename = paste0('DensityHisto',source,ranks,phys, '.png')
  legend_title <- paste(source,'\n',"Wetland Condition",'\n', 
                        format(Sys.time(), "%a %b %d %X %Y"))
  p <- ggplot(One,
              aes(x=One$theta, 
                  group=interaction(One$stress, 
                                    One$phys),
                  col=interaction(One$stress, 
                                  One$phys))) +
    geom_density(alpha=.4,aes(One$theta),position="identity") +
    labs(title=paste0(ranks),x = "Theta (Feet)",y="Density") +
    scale_fill_manual(legend_title) +
    theme_classic()
  
  ggsave(filename=filename,width=10,height=6.66,units="in",dpi=300)
  dev.off()  
}

plotHisto <- function(One, ranks,source){
  graphics.off
  filename = paste0('DensityHisto',source,ranks, '.png')
  legend_title <- paste(source,'\n',"Wetland Condition",'\n', 
                        format(Sys.time(), "%a %b %d %X %Y"))
  p <- ggplot(One[!is.na(One$theta),],
              aes(x=One$theta, 
                  group=interaction(One$stress, One$phys),
                  col=interaction(One$stress, One$phys),
                  fill=interaction(One$stress, One$phys))) +
    geom_histogram(alpha=.4,aes(theta),position="identity") +
    labs(title=paste0(ranks),x = "Theta (Feet)",y="Count") +
    scale_fill_manual(legend_title) +
    theme_classic()
  
  ggsave(filename=filename,width=10,height=6.66,units="in",dpi=300)
  dev.off()  
}
makeQQplots <- function(One, ranks, stress, phys, source) {
  filename = paste0('QQplot',source,stress,phys,ranks, '.png')
  png(filename)
  qqnorm(One[,2],
         main= paste(source, '\n',"Class 1 ",phys, " Wetlands",stress,'\n', ranks, '\n',
                     format(Sys.time(), "%a %b %d %X %Y")))
  qqline(One[,2],col=2,qtype=2)
  dev.off()  
} 

#-------------------
#
# Read preprocessed P80 data sets
#
#-------------------
workdir= "Y:/proj/CFWI_WetlandStress/Update2018"
setwd(workdir)

#SFWMD_P80 <- read_csv("SFWMD_P80.csv")
SFWMD_P80b <- read_csv("./SFWMD/SFWMD_P80.csv")
#SFWMD_P80x <- merge(SFWMD_P80,SFWMD_P80b,by='STATION')

#SWFWMD_P80 <- read_csv("SWFWMD_P80.csv")
SWFWMD_P80b <- read_csv("./SWFWMD/SWFWMD_P80.csv")
#SWFWMD_P80x <- merge(SWFWMD_P80,SWFWMD_P80b,by='STATION')

#SJRWMD_P80 <- read_csv("SJRWMD_P80.csv")
SJRWMD_P80b <- read_csv("./SJRWMD/SJRWMD_P80.csv")
#SJRWMD_P80x <- merge(SJRWMD_P80,SJRWMD_P80b,by='STATION')

AllP80 <-bind_rows(SFWMD_P80b,SWFWMD_P80b,SJRWMD_P80b)
#AllP80 <-bind_rows(SFWMD_P80x,SWFWMD_P80x,SJRWMD_P80x)
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
#rankVec <- c( "2006-2017_P80","2007-2017_P80","2008-2017_P80","2009-2017_P80","2010-2017_P80" )

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

# Ridge  Fu = 0.606  Fs = 0.394
# Plain  Fu = 0.824  Fs = 0.176

thetas$Fu = NA
thetas$Fs = NA
thetas$mean = NA
thetas$sd = NA
thetas[thetas$phys=='Ridge',]$Fu <-0.606
thetas[thetas$phys=='Ridge',]$Fs <-0.394
thetas[thetas$phys=='Plain',]$Fu <-0.824
thetas[thetas$phys=='Plain',]$Fs <-0.176

thetas$theta.logN <-NA

#----------------------------------------------------------------------------
# transform data by subsets using:
#     phys- Physiographic Region (Ridge or Plain)
#     stress- Wetland Stress Status in 2018, 
#----------------------------------------------------------------------------

for (phys in physVec) {
  #----------------------------------------------------------------------------
  # "Plains" thetas are transformed with a log function 
  #     to provide a more normal distribution 
  # "Ridge" thetas are copied in fill the column
  #----------------------------------------------------------------------------
  if (phys == "Plain") {
    thetas[thetas$phys ==phys,]$theta.logN <-log(thetas[thetas$phys == phys,]$theta+10)
  }
  else 
  {
    thetas[thetas$phys ==phys,]$theta.logN <-thetas[thetas$phys == phys,]$theta
  }
  for (stress in stressVec) {
    #----------------------------------------------------------------------------
    # mean and sd are provided for probability density function for the selected 
    # physiographic region type and initial Stress Status in 2018
    #----------------------------------------------------------------------------
    
    thetas[thetas$Stress ==stress & thetas$phys==phys,]$mean <-
      mean(thetas[thetas$Stress ==stress & thetas$phys==phys,]$theta.logN)
    thetas[thetas$Stress ==stress & thetas$phys==phys,]$sd <-
      sd(thetas[thetas$Stress ==stress & thetas$phys==phys,]$theta.logN)
  }
}

Plain<- as.data.frame(seq(-5.0,20.0,.01))
names(Plain) <-c('theta')
Plain$phys <- "Plain"
Plain$Ppu <-0.0
Plain$Ps<-0.0
Plain$Pu<-0.0
Plain$Pps <-0.0
Plain$PpAll <-0.0 
Plain$PsiU <- 0.0
Plain$PsiS <- 0.0


Ridge<- as.data.frame(seq(-5.0,20.0,.01))
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
# dnorm function returns probability from density function at each theta value  Equations: 12 & 13
#----------------------------------------------------------------------------
for (phys in physVec) {
  if (phys == 'Plain') {
    Mean <- max(thetas[thetas$Stress =="Stressed" & thetas$phys==phys,]$mean)
    SD   <- max(thetas[thetas$Stress =="Stressed" & thetas$phys==phys,]$sd)
    Wetlands[Wetlands$phys == phys,]$Ps <- dnorm(log(Wetlands[Wetlands$phys == phys,]$theta+10), Mean, SD)
    
    Mean <- max(thetas[thetas$Stress =="Not Stressed" & thetas$phys==phys,]$mean)
    SD   <- max(thetas[thetas$Stress =="Not Stressed" & thetas$phys==phys,]$sd)
    
    Wetlands[Wetlands$phys == phys,]$Pu <- dnorm(log(Wetlands[Wetlands$phys == phys,]$theta+10), Mean, SD)
  }
  else
  {
    Mean <- max(thetas[thetas$Stress =="Stressed" & thetas$phys==phys,]$mean)
    SD   <- max(thetas[thetas$Stress =="Stressed" & thetas$phys==phys,]$sd)
    Wetlands[Wetlands$phys == phys,]$Ps <- dnorm(Wetlands[Wetlands$phys == phys,]$theta,Mean, SD)
    
    Mean <- max(thetas[thetas$Stress =="Not Stressed" & thetas$phys==phys,]$mean)
    SD   <- max(thetas[thetas$Stress =="Not Stressed" & thetas$phys==phys,]$sd)
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

PsiVals <- function(type, status, value) {
  val <- value
  if (status == 'Not Stressed') {
    if (is.na(val)){
      NA
    }
    else
    {
      if (!is.na(Wetlands[Wetlands$phys == type & round(val, 2) ==
                          round(Wetlands[Wetlands$phys == type, ]$theta, 2), ]$PsiU)) {
        Wetlands[Wetlands$phys == type & round(val, 2) ==
                   round(Wetlands[Wetlands$phys == type, ]$theta, 2), ]$PsiU
      }
    }
  }
  else
  {
    if (is.na(val)){
      NA
    }
    else
    {
      if (!is.na(Wetlands[Wetlands$phys == type &
                          round(val, 2) == round(Wetlands[Wetlands$phys == type,]$theta, 2),]$PsiS)) {
        Wetlands[Wetlands$phys == type &
                   round(val, 2) == round(Wetlands[Wetlands$phys == type,]$theta, 2),]$PsiS
      }
    }
  }
}
vPsiVals <- Vectorize(PsiVals)

makeZetas <- function() {
  z = matrix(NA,length(seq(-5.0,20.0,.01)),1+length(seq(-3, 3, .2)))
  z[,1] <- vdf[,1]
  for (i in seq(2,32)){
    if(i < 17){
      z[,i] = NA
    }
    else 
    {
      z[,i] = 1 -   ( vPsi2(phys,stress,vdf[,i])/
                        vPsi2(phys,stress,vdf[,1]))
    }
  }
  StressZetas<- as.data.frame(cbind(phys,stress,z))
  #StressZetas$phys <- phys
  deltaNames <- sprintf("delta_%s",seq(-3, 3, .2))
  names(StressZetas) <- c("phys","stress","theta",deltaNames)
  return(StressZetas)
}

vdf = c()
for (x in seq(-5.0,20.0,.01)) {
  possibleThetas<- seq(-3, 3, .2)+x
  vdf<-c(vdf,possibleThetas)
}
dim(vdf)<-c(31,2501)
vdf <- t(vdf)
vdf[vdf< -5.0]<-NA
vdf[vdf> 20.0]<-NA
# Add theta column to beginning 
vdf <-cbind(Wetlands[1:2501,]$theta,vdf)

physVec = c('Ridge','Plain')
stressVec = c('Not Stressed','Stressed')
ix = 0
plan(multiprocess)
d <- listenv()
#d <-as.list(length(4))
tic("started Calculating Zetas")
for (phys in physVec){
  for (stress in stressVec){
    cat(paste(phys, stress, '\n'))
    ix = ix + 1
    d[[ix]] %<-% makeZetas()
  }
}
x <- as.list(d)
Zetas<- do.call(rbind,x)
toc()
write.csv(file='h:/Zetas.csv',Zetas)




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


for (ranks in rankVec) {
  for (stress in stressVec) {
    for (phys in physVec) {
      plotNormalHistogram(thetas[thetas$Stress == stress
                                    & thetas$phys == phys,]$theta.logN,
                          main=paste(stress,' ',phys,' ',ranks),xlim=c(0,4),
                          xlab=expression("Transformed Hydrologic Index "~~ {theta}))
    }
  }
}

wideTheta <- dcast(thetas,EMT_ID~rank,value.var='theta.logN',mean)
thetaEval <- merge(wideTheta,Class1P80[,c(1,3,4,5,11)], by.x='EMT_ID', by.y = "CFCA/EMT ID")
write.csv(file='h:/thetas4Eval.csv',thetaEval)
write.csv(file='h:/thetasTransformed.csv',thetas)

OrigwideTheta<- dcast(thetas,EMT_ID~rank,value.var='theta',mean)
OrigTheta <-merge(OrigwideTheta,Class1P80[,c(1,3,4,5,11)], by.x='EMT_ID', by.y = "CFCA/EMT ID")

workdir= "Y:/proj/CFWI_WetlandStress/Update2018"

for (phys in physVec) {
  for (stress in stressVec) {
    One <- thetaEval[thetaEval$'Stress Status in 2018' == stress &
                       thetaEval$'Physiographic Region' == phys,
                     c('EMT_ID',ranks,strStr,physStr)]
    Orig <- OrigTheta[OrigTheta$'Stress Status in 2018' == stress &
                        OrigTheta$'Physiographic Region' == phys,
                      c('EMT_ID',ranks,strStr,physStr)]
    
    names(One)[names(One) == ranks] <- "theta"
    names(One)[names(One) == strStr] <-"stress"
    names(One)[names(One) == physStr] <-"phys"

    names(Orig)[names(Orig) == ranks] <- "theta"
    names(Orig)[names(Orig) == strStr] <-"stress"
    names(Orig)[names(Orig) == physStr] <-"phys"

    source = 'Transformed'
    makeQQplots(One, ranks, stress, phys, source)
    
    source = 'Original'
    makeQQplots(Orig, ranks, stress, phys, source)
    
    source = 'Original'
    cat(paste(source, ' '))
    swTest <- shapiro.test(Orig$theta)
    cat (paste0('"shapiro.test for ","',stress,'","',phys,'", "',ranks,'",'))
    cat(paste0(swTest$statistic, '  ', swTest$p.value, '\n'))
    
    source = 'Transformed'
    cat(paste(source, ' '))
    swTest <- shapiro.test(One$theta)
    cat (paste0('"shapiro.test for ","',stress,'","',phys,'", "',ranks,'",'))
    cat(paste0(swTest$statistic, '  ', swTest$p.value, '\n'))
  }
  One <- thetaEval[, c('EMT_ID',ranks,strStr,physStr)]
  names(One)[names(One) == ranks] <- "theta"
  names(One)[names(One) == strStr] <-"stress"
  names(One)[names(One) == physStr] <-"phys"

  if (phys == 'Plain'){    
    source = 'Transformed'
    HistoDens(One[One$phys=='Plain' & One$phys =='Plain', ], ranks, phys, source)
  }
  
  Orig <-
    OrigTheta[, c('EMT_ID',ranks,strStr,physStr)]
  names(Orig)[names(Orig) == ranks] <- "theta"
  names(Orig)[names(Orig) == strStr] <-"stress"
  names(Orig)[names(Orig) == physStr] <-"phys"
  if (phys == 'Ridge'){    
    source = 'Original'
    HistoDens(Orig[Orig$phys=='Ridge',], ranks, phys, source)
  }
  else {
    source = 'Original'
    HistoDens(Orig[Orig$phys=='Plain', ], ranks, phys, source)
  }
}


