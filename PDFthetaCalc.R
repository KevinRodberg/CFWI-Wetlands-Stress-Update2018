

        Currently this Code does not run



list.of.pkgs <-  c("readr","dplyr","zoo","ggplot2", "reshape2", "data.table",
                   "future","listenv","readxl","purrr","e1071" ,"rcompanion")

new.pkgs <- list.of.pkgs[!(list.of.pkgs %in% installed.packages()[, "Package"])]

if (length(new.pkgs)){ install.packages(new.pkgs) }
for (pkg in list.of.pkgs){ library(pkg,character.only = TRUE) }

#-------------------
#
#  Define plotting functions
#
#-------------------
plotHistoDens <- function(onestation, ranks,phys,source){
  graphics.off
  filename = paste0('DensityHisto',source,ranks,phys, '.png')
  legend_title <- paste(source,'\n',"Wetland Condition",'\n', 
                        format(Sys.time(), "%a %b %d %X %Y"))
  p <- ggplot(onestation[!is.na(onestation$theta),],
              aes(x=onestation$theta, 
                  group=interaction(onestation$stress, onestation$phys),
                  col=interaction(onestation$stress, onestation$phys))) +
    geom_density(alpha=.4,aes(theta),position="identity") +
    labs(title=paste0(ranks),x = "Theta (Feet)",y="Density") +
    scale_fill_manual(legend_title) +
    theme_classic()
  
  ggsave(filename=filename,width=10,height=6.66,units="in",dpi=300)
  dev.off()  
}

plotHisto <- function(onestation, ranks,source){
  graphics.off
  filename = paste0('DensityHisto',source,ranks, '.png')
  legend_title <- paste(source,'\n',"Wetland Condition",'\n', 
                        format(Sys.time(), "%a %b %d %X %Y"))
  p <- ggplot(onestation[!is.na(onestation$theta),],
              aes(x=onestation$theta, 
                  group=interaction(onestation$stress, onestation$phys),
                  col=interaction(onestation$stress, onestation$phys),
                  fill=interaction(onestation$stress, onestation$phys))) +
    geom_histogram(alpha=.4,aes(theta),position="identity") +
    labs(title=paste0(ranks),x = "Theta (Feet)",y="Count") +
    scale_fill_manual(legend_title) +
    theme_classic()
  
  ggsave(filename=filename,width=10,height=6.66,units="in",dpi=300)
  dev.off()  
}
makeQQplots <- function(onestation, ranks, stress, phys, source) {
  filename = paste0('QQplot',source,stress,phys,ranks, '.png')
  png(filename)
  qqnorm(onestation[,2],
         main= paste(source, '\n',"Class 1 ",phys, " Wetlands",stress,'\n', ranks, '\n',
                     format(Sys.time(), "%a %b %d %X %Y")))
  qqline(onestation[,2],col=2,qtype=2)
  dev.off()  
} 

#-------------------
#
# Read preprocessed P80 data sets
#
#-------------------
workdir= "Y:/proj/CFWI_WetlandStress/Update2018"
setwd(workdir)

SFWMD_P80 <- read_csv("SFWMD_P80.csv")
SFWMD_P80b <- read_csv("./SFWMD/SFWMD_P80.csv")
SFWMD_P80x <- merge(SFWMD_P80,SFWMD_P80b,by='STATION')

SWFWMD_P80 <- read_csv("SWFWMD_P80.csv")
SWFWMD_P80b <- read_csv("./SWFWMD/SWFWMD_P80.csv")
SWFWMD_P80x <- merge(SWFWMD_P80,SWFWMD_P80b,by='STATION')

SJRWMD_P80 <- read_csv("SJRWMD_P80.csv")
SJRWMD_P80b <- read_csv("./SJRWMD/SJRWMD_P80.csv")
SJRWMD_P80x <- merge(SJRWMD_P80,SJRWMD_P80b,by='STATION')

AllP80 <-bind_rows(SFWMD_P80x,SWFWMD_P80x,SJRWMD_P80x)
EMT_ID <- read_csv("EMT_ID.csv")
AllP80 <-merge(EMT_ID,AllP80)
write.csv(AllP80,file='AllP80.csv',row.names=FALSE)

Class1Wetlands <- read_excel("Class 1 Wetland Info for Analysis ALLv1.xlsx", na = "NA")

Class1P80 <-merge(Class1Wetlands,AllP80, by.x='CFCA/EMT ID', by.y='EMT_ID')    
# Remove redundant 2006-2017_P80
Class1P80$`2006-2017_P80.y`<-NULL
names(Class1P80)[names(Class1P80)=='2006-2017_P80.x']<-"2006-2017_P80"
names(Class1P80)
write_csv(Class1P80,"./Class1P80.csv")
#-------------------
#
# Calculate thetas
#
#-------------------
thetas = data.frame()
ranks = "2009-2017_P80"
#rankVector <- c( "2006-2017_P80","2007-2017_P80","2008-2017_P80","2009-2017_P80","2010-2017_P80"  )
rankVec <- c( "2009-2017_P80" )
for (ranks in rankVec) {
  theta = Class1P80$"Edge Reference Elevation (ft NAVD 88)" - Class1P80[,ranks]
  thetas = rbind(thetas,cbind.data.frame(EMT_ID=Class1P80$`CFCA/EMT ID`, rank=ranks,theta=as.numeric(theta)))
}
thetas <- merge(thetas,Class1P80[,c(1,3,4,5)], by.x='EMT_ID', by.y = "CFCA/EMT ID")
																	
names(thetas)[names(thetas) == "Stress Status"] <-"Stress"
names(thetas)[names(thetas) == "Physiographic Region"] <-"phys"
physVec <-c("Plain","Ridge")
stressVec <- c("Stressed","Not Stressed")

#-------------------
#
# Cumulative Density Function Plot
#
#-------------------
plot(ecdf(thetas[thetas$Stress == stressVec[1] &
                      thetas$phys == physVec[1],]$theta),
     main='CDF 2009-2017',ylab='density',
     xlab=expression("Hydrologic Index "~~ {theta}),col="orange")
lines(ecdf(thetas[thetas$Stress == stressVec[2]&
                       thetas$phys == physVec[2],]$theta),col="Green")
lines(ecdf(thetas[thetas$Stress == stressVec[2]&
                       thetas$phys == physVec[1],]$theta),col="blue")
lines(ecdf(thetas[thetas$Stress == stressVec[1]&
                       thetas$phys == physVec[2],]$theta),col="red")
legend('right',legend=c(paste(stressVec[1],physVec[1]),
                paste(stressVec[2],physVec[2]),
                paste(stressVec[2],physVec[1]),
                paste(stressVec[1],physVec[2])),  # text in the legend
       col=c("orange","Green","blue","red"),pch=15,cex=0.5)
             
newColumns <-c('transformed','trans','Pu','Ppu','Ps','Pps','Fu','Fs','PpAll','theta.logN','PsiU','PsiS')
thetas[newColumns]<-NA
thetas$PsiU <-0
thetas$PsiS <-0

# These aren't needed right now
thetas$transformed <-NULL
thetas$theta.trans<-NULL
thetas$theta.logN <-NULL

Plain<- as.data.frame(seq(-10.0,15.0,.01))
names(Plain) <-c('theta')
newColumns <-c('phys','Ppu','Ps','Pu','Pps','PpAll','PsiU','PsiS')
Plain$phys <- "Plain"
Plain[newColumns]<-0.0

Ridge<- as.data.frame(seq(-10.0,15.0,.01))
names(Ridge) <-c('theta')
Ridge$phys <- "Ridge"
Ridge[newColumns]<-0.0

Wetlands <-rbind(Plain,Ridge)

#----------------------------------------------------------------------------
# transform data by subsets using:
#     phys- Physiographic Region (Ridge or Plain)
#     stress- Wetland Stress Status, 
#----------------------------------------------------------------------------

for (phys in physVec) {
  for (stress in stressVec) {
    plotNormalHistogram(thetas[thetas$Stress == stress
                                  & thetas$phys == phys,]$theta,
                        main=paste(stress,' ',phys,' ',ranks),xlim=c(0,16),
                        xlab=expression("Hydrologic Index "~~ {theta}))
    # if (phys == "Plain") {
    #   thetas[thetas$Stress ==stress 
    #          & thetas$phys ==phys,]$theta.logN <- 
    #     log(thetas[thetas$Stress == stress
    #                & thetas$phys == phys,]$theta)
    # }
    # else 
    # {
    #   thetas[thetas$Stress ==stress 
    #             & thetas$phys ==phys,]$theta.logN <- 
    #     thetas[thetas$Stress == stress
    #               & thetas$phys == phys,]$theta
    # }
  }
  #----------------------------------------------------------------------------
  # density function provides probability density function for the selected 
  # physiographic region type and initial stress status
  #----------------------------------------------------------------------------
  # dxStress <- density(thetas[thetas$Stress ==stressVec[1]
  #                            & thetas$phys==phys,]$theta.logN)
  # dxNotStress <- density(thetas[thetas$Stress ==stressVec[2]
  #                               & thetas$phys==phys,]$theta.logN)  
  dxStress <- density(thetas[thetas$Stress ==stressVec[1]
                             & thetas$phys==phys,]$theta)
  dxNotStress <- density(thetas[thetas$Stress ==stressVec[2]
                                & thetas$phys==phys,]$theta)
  #----------------------------------------------------------------------------
  # approx function returns probability from density function at each theta value  Equations: 12 & 13
  #----------------------------------------------------------------------------
  thetas[thetas$phys==phys,]$Ps <-
    approx(dxStress$x,dxStress$y,xout=thetas[thetas$phys ==phys,]$theta)$y
  thetas[thetas$phys==phys,]$Pu <-
    approx(dxNotStress$x,dxNotStress$y,xout=thetas[thetas$phys ==phys,]$theta)$y
  # thetas[thetas$phys==phys,]$Ps <-
  #   approx(dxStress$x,dxStress$y,xout=thetas[thetas$phys ==phys,]$theta.logN)$y
  # thetas[thetas$phys==phys,]$Pu <-
  #   approx(dxNotStress$x,dxNotStress$y,xout=thetas[thetas$phys ==phys,]$theta.logN)$y  
  if (phys == 'Plain') {
    if (!is.null(Wetlands[Wetlands$theta > 0, ]$theta)) {
      Wetlands[Wetlands$theta > 0, ]$Ps <-
        approx(dxStress$x, dxStress$y, xout =
                 log(Wetlands[Wetlands$theta > 0, ]$theta))$y
      Wetlands[Wetlands$theta > 0, ]$Pu <-
        approx(dxNotStress$x, dxNotStress$y, xout =
                 log(Wetlands[Wetlands$theta > 0, ]$theta))$y
    }
    
  }
  else 
  {
    for (phys in physVec) {
    #example$RidgePs <- approx(dxStress$x, dxStress$y, xout = example$theta)$y
    Wetlands[Wetlands$phys == phys,]$Ps <- approx(dxStress$x, dxStress$y, xout =
                                                    Wetlands[Wetlands$phys == phys ,]$theta)$y
    Wetlands[Wetlands$phys == phys,]$Pu <- approx(dxNotStress$x, dxNotStress$y, xout =
                                                    Wetlands[Wetlands$phys == phys ,]$theta)$y
    #example$RidgePu <- approx(dxNotStress$x, dxNotStress$y, xout = example$theta)$y
    }
  }
  
  # set NA to 0
  thetas[thetas$phys==phys,]$Pu <- 
    ifelse( is.na( thetas[thetas$phys==phys,]$Pu ), 0, thetas[thetas$phys==phys,]$Pu) 
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
  
  #----------------------------------------------------------------------------
  # Pps and Ppu are Population-weighted contributions of stress and unstress
  # wetlands to the total population probability density of all wetlands at 
  # each wetland hydrologic index (theta)                                 Equations: 14 & 15
  #----------------------------------------------------------------------------
  thetas[thetas$phys==phys,]$Ppu <- thetas[thetas$phys==phys,]$Pu*thetas[thetas$phys==phys,]$Fu
  thetas[thetas$phys==phys,]$Pps <- thetas[thetas$phys==phys,]$Ps*thetas[thetas$phys==phys,]$Fs
  # [and PpAll]                                                           Equation: 16
  thetas[thetas$phys==phys,]$PpAll <- thetas[thetas$phys==phys,]$Ppu + thetas[thetas$phys==phys,]$Pps
  if (phys == "Plain"){
    if (!is.null(Wetlands[Wetlands$PlainPu >0,]$PlainPu)){
      Wetlands$PlainPpu <-Wetlands$PlainPu*max(thetas[thetas$phys==phys,]$Fu)
    }
    if (!is.null(Wetlands[Wetlands$PlainPs >0,]$PlainPs)){
      Wetlands$PlainPps <-Wetlands$PlainPs*max(thetas[thetas$phys==phys,]$Fs)
    }
    Wetlands$PlainPpAll <-Wetlands$PlainPpu + Wetlands$PlainPps 
    Wetlands$PlainPsiU <- Wetlands$PlainPpu /Wetlands$PlainPpAll
    Wetlands$PlainPsiS <- Wetlands$PlainPps /Wetlands$PlainPpAll
    }
  else 
    {
      Wetlands$RidgePpu <-Wetlands$RidgePu*max(thetas[thetas$phys==phys,]$Fu)
      Wetlands$RidgePps <-Wetlands$RidgePs*max(thetas[thetas$phys==phys,]$Fs)
      Wetlands$RidgePpAll <-Wetlands$RidgePpu + Wetlands$RidgePps    
      Wetlands$RidgePsiU <- Wetlands$RidgePpu /Wetlands$RidgePpAll
      Wetlands$RidgePsiS <- Wetlands$RidgePps /Wetlands$RidgePpAll
    }
  
  #----------------------------------------------------------------------------
  # PsiU and PsiS Probability weighted Cumulative Probability             Equation 17 & 18
  #----------------------------------------------------------------------------
  thetas[thetas$phys==phys,]$PsiU  <-thetas[thetas$phys==phys,]$Ppu/thetas[thetas$phys==phys,]$PpAll
  thetas[thetas$phys==phys,]$PsiS  <-thetas[thetas$phys==phys,]$Pps/thetas[thetas$phys==phys,]$PpAll
}

longTab <- as.data.frame(seq(-10.0,15.0,.01))
names(longTab) <-c('theta')
longTab$theta2 <- seq(-9.95,15.05,.01)
val=6.0
type ='Plain'
status='Not Stressed'
Psi2 <- function(type,status,val){ 
  if (type == 'Plain') {
    if (status == 'Not Stressed'){
      return(Wetlands[Wetlands$theta==val,]$PlainPsiU )
    }
    else {
      return(Wetlands[Wetlands$theta==val,]$PlainPsiS)
    }
    }
  else
  {
    if (status == 'Not Stressed'){
      return(Wetlands[Wetlands$theta==val,]$RidgePsiU )
  }
    else {
      return(Wetlands[Wetlands$theta==val,]$RidgePsiS)
    }
  }
}
Psi2('Ridge','Not Stressed',6.0)
#Psi2(longTab$theta2)
longTab$PlainPsiu2 <-Vectorize(Psi2,c('Plain','Not Stressed',longTab$theta2))

#                         PlainPsiu2=Wetlands[Wetlands$theta==theta2,]$PlainPsiU)
xlim=c(min(Wetlands[Wetlands$RidgePps > 0,]$RidgePs, na.rm = T),
       max(Wetlands[Wetlands$RidgePps > 0,]$RidgePs, na.rm = T))

plot(ecdf(Wetlands[Wetlands$RidgePps > 0,]$RidgePs),
     main='CDF 2009-2017',ylab='density', col="orange",xlim=xlim,
     xlab=expression("Transformed Hydrologic Index "~~ {theta}))

lines(ecdf(Wetlands[Wetlands$RidgePps > 0,]$RidgePs),col="Green")
lines(ecdf(Wetlands[Wetlands$RidgePpu > 0,]$RidgePs),col="Red")

legend('bottomright',legend=c(paste(stressVec[1],physVec[1]),
                              paste(stressVec[2],physVec[1])),  # text in the legend
       col=c("orange","Green"),pch=15,cex=0.75)


# xlim=c(min(thetas[thetas$phys == physVec[1],]$theta.logN),
#        max(thetas[thetas$phys == physVec[1] ,]$theta.logN))
# plot(ecdf(thetas[thetas$Stress == stressVec[1] &
#                    thetas$phys == physVec[1],]$theta.logN),
#      main='CDF 2009-2017',ylab='density', col="orange",xlim=xlim,
#      xlab=expression("Transformed Hydrologic Index "~~ {theta}))
# lines(ecdf(thetas[thetas$Stress == stressVec[2]&
#                     thetas$phys == physVec[1],]$theta.logN),col="Green")
# legend('bottomright',legend=c(paste(stressVec[1],physVec[1]),
#                               paste(stressVec[2],physVec[1])),  # text in the legend
#        col=c("orange","Green"),pch=15,cex=0.75)
# 
# xlim=c(min(thetas[thetas$phys == physVec[2],]$theta.logN),
#        max(thetas[thetas$phys == physVec[2] ,]$theta.logN))
# plot(ecdf(thetas[thetas$Stress == stressVec[1]&
#                    thetas$phys == physVec[2],]$theta.logN),
#      main=paste(rankVec[1],physVec[2]),col="blue",xlim=xlim,
#      xlab=expression("Hydrologic Index "~~ {theta}))
# 
# lines(ecdf(thetas[thetas$Stress == stressVec[2]&
#                     thetas$phys == physVec[2],]$theta.logN),col="red")
# legend('bottomright',legend=c(paste(stressVec[1],physVec[2]),
#                               paste(stressVec[2],physVec[2])),  # text in the legend
#        col=c("blue","red"),pch=15,cex=0.75)
# for (ranks in rankVec) {
#   for (stress in stressVec) {
#     for (phys in physVec) {
#       plotNormalHistogram(thetas[thetas$Stress == stress
#                                  & thetas$phys == phys,]$theta.logN,
#                           main=paste(stress,' ',phys,' ',ranks),xlim=c(0,4),
#                           xlab=expression("Transformed Hydrologic Index "~~ {theta}))
#     }
#   }
# }
# 
# 
# wideTheta <- dcast(thetas,EMT_ID~rank,value.var='theta.logN',mean)
# thetaEval <- merge(wideTheta,Class1P80[,c(1,3,4,5,11)], by.x='EMT_ID', by.y = "CFCA/EMT ID")
# write.csv(file='h:/thetas4Eval.csv',thetaEval)
# write.csv(file='h:/thetasTransformed.csv',thetas)
# write.csv(file='h:/Wetlands.csv',Wetlands)

xlim=c(min(thetas[thetas$phys == physVec[1],]$theta),
       max(thetas[thetas$phys == physVec[1] ,]$theta))

plot(ecdf(thetas[thetas$Stress == stressVec[1] &
                       thetas$phys == physVec[1],]$theta),
     main='CDF 2009-2017',ylab='density', col="orange",xlim=xlim,
     xlab=expression("Transformed Hydrologic Index "~~ {theta}))

lines(ecdf(thetas[thetas$Stress == stressVec[2]&
                       thetas$phys == physVec[1],]$theta),col="Green")
legend('bottomright',legend=c(paste(stressVec[1],physVec[1]),
                        paste(stressVec[2],physVec[1])),  # text in the legend
       col=c("orange","Green"),pch=15,cex=0.75)

xlim=c(min(thetas[thetas$phys == physVec[2],]$theta),
       max(thetas[thetas$phys == physVec[2] ,]$theta))
plot(ecdf(thetas[thetas$Stress == stressVec[1]&
                       thetas$phys == physVec[2],]$theta),
     main=paste(rankVec[1],physVec[2]),col="blue",xlim=xlim,
     xlab=expression("Hydrologic Index "~~ {theta}))

lines(ecdf(thetas[thetas$Stress == stressVec[2]&
                       thetas$phys == physVec[2],]$theta),col="red")
legend('bottomright',legend=c(paste(stressVec[1],physVec[2]),
                              paste(stressVec[2],physVec[2])),  # text in the legend
       col=c("blue","red"),pch=15,cex=0.75)


for (ranks in rankVec) {
  for (stress in stressVec) {
    for (phys in physVec) {
      plotNormalHistogram(thetas[thetas$Stress == stress
                                 & thetas$phys == phys,]$theta,
                          main=paste(stress,' ',phys,' ',ranks),xlim=c(0,4),
                          xlab=expression("Transformed Hydrologic Index "~~ {theta}))
    }
  }
}

wideTheta <- dcast(thetas,EMT_ID~rank,value.var='theta',mean)
thetaEval <- merge(wideTheta,Class1P80[,c(1,3,4,5,11)], by.x='EMT_ID', by.y = "CFCA/EMT ID")
write.csv(file='h:/thetas4Eval.csv',thetaEval)
write.csv(file='h:/thetasTransformed.csv',thetas)
write.csv(file='h:/Wetlands.csv',Wetlands)

OrigwideTheta<- dcast(thetas,EMT_ID~rank,value.var='theta',mean)
OrigTheta <-merge(OrigwideTheta,Class1P80[,c(1,3,4,5,11)], by.x='EMT_ID', by.y = "CFCA/EMT ID")


workdir= "Y:/proj/CFWI_WetlandStress/Update2018"
#rankVec <- c( "2006-2017_P80","2007-2017_P80","2008-2017_P80","2009-2017_P80","2010-2017_P80" )
rankVec <- c("2009-2017_P80" )
stressVec <- c("Stressed","Not Stressed")
physVec <-c("Plain","Ridge")

for (phys in physVec) {
  for (stress in stressVec) {
    onestation <-
      thetaEval[thetaEval$'Stress Status' == stress &
                  thetaEval$'Physiographic Region' == phys ,
                c('EMT_ID',ranks,'Stress Status','Physiographic Region','Hydrologically Altered')]
    
    origstation <-
      OrigTheta[OrigTheta$'Stress Status' == stress &
                  OrigTheta$'Physiographic Region' == phys ,
                c('EMT_ID',ranks,'Stress Status','Physiographic Region','Hydrologically Altered')]
    
    names(onestation)[names(onestation) == ranks] <- "theta"
    names(onestation)[names(onestation) == 'Stress Status'] <-"stress"
    names(onestation)[names(onestation) == "Physiographic Region"] <-"phys"

    names(origstation)[names(origstation) == ranks] <- "theta"
    names(origstation)[names(origstation) == 'Stress Status'] <-"stress"
    names(origstation)[names(origstation) == "Physiographic Region"] <-"phys"

    source = 'Transformed'
    makeQQplots(onestation, ranks, stress, phys, source)
    
    source = 'Original'
    makeQQplots(origstation, ranks, stress, phys, source)
    
    source = 'Original'
    cat(paste(source, ' '))
    swTest <- shapiro.test(origstation$theta)
    cat (paste0('"shapiro.test for ","',stress,'","',phys,'", "',ranks,'",'))
    cat(paste0(swTest$statistic, '  ', swTest$p.value, '\n'))
    
    source = 'Transformed'
    cat(paste(source, ' '))
    swTest <- shapiro.test(onestation$theta)
    cat (paste0('"shapiro.test for ","',stress,'","',phys,'", "',ranks,'",'))
    cat(paste0(swTest$statistic, '  ', swTest$p.value, '\n'))
  }
  onestation <-
    thetaEval[, c('EMT_ID',ranks,'Stress Status','Physiographic Region','Hydrologically Altered')]
  names(onestation)[names(onestation) == ranks] <- "theta"
  names(onestation)[names(onestation) == 'Stress Status'] <-"stress"
  names(onestation)[names(onestation) == "Physiographic Region"] <-"phys"
  names(onestation)[names(onestation) == "Hydrologically Altered"] <-"Altered"
  
  if (phys == 'Plain'){    
    source = 'Transformed'
    plotHistoDens(onestation[onestation$phys=='Plain', ], ranks, phys, source)
  }
  
  origstation <-
    OrigTheta[, c('EMT_ID',ranks,'Stress Status','Physiographic Region','Hydrologically Altered')]
  names(origstation)[names(origstation) == ranks] <- "theta"
  names(origstation)[names(origstation) == 'Stress Status'] <-"stress"
  names(origstation)[names(origstation) == "Physiographic Region"] <-"phys"
  if (phys == 'Ridge'){    
    source = 'Original'
    plotHistoDens(origstation[origstation$phys=='Ridge' ,], ranks, phys, source)
  }
  else {
    source = 'Original'
    plotHistoDens(origstation[origstation$phys=='Plain', ], ranks, phys, source)
  }
}


