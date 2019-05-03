
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
                  group=interaction(onestation$stress, onestation$Phys),
                  col=interaction(onestation$stress, onestation$Phys))) +
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
                  group=interaction(onestation$stress, onestation$Phys),
                  col=interaction(onestation$stress, onestation$Phys),
                  fill=interaction(onestation$stress, onestation$Phys))) +
    geom_histogram(alpha=.4,aes(theta),position="identity") +
    labs(title=paste0(ranks),x = "Theta (Feet)",y="Count") +
    scale_fill_manual(legend_title) +
    theme_classic()
  
  ggsave(filename=filename,width=10,height=6.66,units="in",dpi=300)
  dev.off()  
}
makeQQplots <- function(onestation, ranks, stress, Phys, source) {
  filename = paste0('QQplot',source,stress,Phys,ranks, '.png')
  png(filename)
  qqnorm(onestation[,2],
         main= paste(source, '\n',"Class 1 ",Phys, " Wetlands",stress,'\n', ranks, '\n',
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
Class1P80$'2006-2017_P80.y'<-NULL
names(Class1P80)[names(Class1P80)=='2006-2017_P80.x']<-"2006-2017_P80"
names(Class1P80)

#-------------------
#
# Calculate thetas
#
#-------------------
thetas = data.frame()
ranks = "2009-2017_P80"
#rankVector <- c( "2006-2017_P80","2007-2017_P80","2008-2017_P80","2009-2017_P80","2010-2017_P80"  )
rankVector <- c( "2009-2017_P80" )
for (ranks in rankVector) {
  theta = Class1P80$"Edge Reference Elevation (ft NAVD 88)" - Class1P80[,ranks]
  thetas = rbind(thetas,cbind.data.frame(EMT_ID=Class1P80$`CFCA/EMT ID`, rank=ranks,theta=as.numeric(theta)))
}
thetaTran <- merge(thetas,Class1P80[,c(1,3,4,5)], by.x='EMT_ID', by.y = "CFCA/EMT ID")

names(thetaTran)[names(thetaTran) == "Stress Status in 2018"] <-"Stress"
names(thetaTran)[names(thetaTran) == "Physiographic Region"] <-"Phys"

physVector <-c("Plain","Ridge")
stressVec <- c("Stressed","Not Stressed")

#-------------------
#
# Cumulative Density Function Plot
#
#-------------------
plot(ecdf(thetaTran$theta),main=paste(rankVector[1]))
lines(ecdf(thetaTran[thetaTran$rank == rankVector[1] &
                      thetaTran$Stress == stressVec[1] &
                      thetaTran$Phys == physVector[1],]$theta),col="orange")
lines(ecdf(thetaTran[thetaTran$rank == rankVector[1] &
                       thetaTran$Stress == stressVec[2]&
                       thetaTran$Phys == physVector[2],]$theta),col="Green")
lines(ecdf(thetaTran[thetaTran$rank == rankVector[1] &
                       thetaTran$Stress == stressVec[2]&
                       thetaTran$Phys == physVector[1],]$theta),col="blue")
lines(ecdf(thetaTran[thetaTran$rank == rankVector[1] &
                       thetaTran$Stress == stressVec[1]&
                       thetaTran$Phys == physVector[2],]$theta),col="red")
legend('bottomright',legend=c("All",
                paste(stressVec[1],physVector[1]),
                paste(stressVec[2],physVector[2]),
                paste(stressVec[2],physVector[1]),
                paste(stressVec[1],physVector[2])),  # text in the legend
       col=c("black","orange","Green","blue","red"),pch=15,cex=0.5)
             
thetaTran$transformed <-NA
thetaTran$theta.trans<-NA
# These two aren't needed right now
thetaTran$transformed <-NULL
thetaTran$theta.trans<-NULL

thetaTran$best.c <-NA
thetaTran$theta.logNskew <-NA

#-------------------
#
# transform data by subsets using:
#     rank- P80rank date range, 
#     stress- Wetland Stress Status, 
#     phys- Physiographic property (Ridge or Plain)
#
#-------------------

# Skewness optimization function
skew.score <- function(c, x){
  if ((c + min(x)) > 0) {
    (skewness(log(x + c))) ^ 2
  }
  else
  {
    99
  }
}

for (ranks in rankVector) {
  for (stress in stressVec) {
    for (phys in physVector) {
      plotNormalHistogram(thetaTran[thetaTran$rank==ranks 
                                    & thetaTran$Stress == stress
                                    & thetaTran$Phys == phys,]$theta,
                          main=paste(stress,' ',phys,' ',ranks),xlim=c(0,16),
                          xlab=expression("Hydrologic Index "~~ {theta}))
      
      # Identifies skewness using function
      # default method is type 3 or similar to BMDP or Minitab
      #   b_1 = m_3 / s^3 = g_1 ((n-1)/n)^(3/2)
      # other options are available if needed.
      # type = 1 :
      #   g_1 = m_3 / m_2^(3/2)
      # type = 2 :
      #   G_1 = g_1 * sqrt(n(n-1)) / (n-2)
      # Not used in transform other than info
      skewTheta <- skewness(thetaTran[thetaTran$rank==ranks 
                                      & thetaTran$Stress == stress
                                      & thetaTran$Phys == phys,]$theta)
      kurtosisTheta <- kurtosis(thetaTran[thetaTran$rank==ranks 
                                            & thetaTran$Stress == stress
                                            & thetaTran$Phys == phys,]$theta)
      # Optimize skewness calculation
      # skew.score <- function(c, x) (skewness(log(x + c)))^2
      best.c <- optimise(skew.score, c(0, 5),
                         x = thetaTran[thetaTran$rank ==
                                         ranks & thetaTran$Stress ==
                                         stress & thetaTran$Phys ==
                                         phys, ]$theta)$minimum 
      if (phys == "Ridge" ) {
        best.c <- 0.0
        thetaTran[thetaTran$rank==
                    ranks & thetaTran$Stress ==
                    stress & thetaTran$Phys ==
                    phys,]$theta.logNskew <- thetaTran[thetaTran$rank==ranks 
                                                       & thetaTran$Stress == stress
                                                       & thetaTran$Phys == phys,]$theta  
      }
      else {
        thetaTran[thetaTran$rank==
                    ranks & thetaTran$Stress ==
                    stress & thetaTran$Phys ==
                    phys,]$theta.logNskew <- log(thetaTran[thetaTran$rank==ranks 
                                                       & thetaTran$Stress ==stress 
                                                       & thetaTran$Phys ==phys,]$theta + best.c)
      }
      thetaTran[thetaTran$rank==ranks 
                & thetaTran$Stress == stress
                & thetaTran$Phys == phys,]$best.c <-best.c
    }
  }
}

xlim=c(min(thetaTran[thetaTran$rank==rankVector[1] &
                       thetaTran$Phys == physVector[1],]$theta.logNskew),
       max(thetaTran[thetaTran$rank==rankVector[1] &
                       thetaTran$Phys == physVector[1] ,]$theta.logNskew))
plot(ecdf(thetaTran[thetaTran$rank == rankVector[1] &
                       thetaTran$Stress == stressVec[1] &
                       thetaTran$Phys == physVector[1],]$theta.logNskew),
     main=paste(rankVector[1],' ',physVector[1]),col="orange",xlim=xlim,
     xlab=expression("Transformed Hydrologic Index "~~ {theta}))
lines(ecdf(thetaTran[thetaTran$rank == rankVector[1] &
                       thetaTran$Stress == stressVec[2]&
                       thetaTran$Phys == physVector[1],]$theta.logNskew),col="Green")
legend('bottomright',legend=c(paste(stressVec[1],physVector[1]),
                        paste(stressVec[2],physVector[1])),  # text in the legend
       col=c("orange","Green"),pch=15,cex=0.75)

xlim=c(min(thetaTran[thetaTran$rank==rankVector[1] &
                       thetaTran$Phys == physVector[2],]$theta.logNskew),
       max(thetaTran[thetaTran$rank==rankVector[1] &
                       thetaTran$Phys == physVector[2] ,]$theta.logNskew))
plot(ecdf(thetaTran[thetaTran$rank == rankVector[1] &
                       thetaTran$Stress == stressVec[1]&
                       thetaTran$Phys == physVector[2],]$theta.logNskew),
     main=paste(rankVector[1],physVector[2]),col="blue",xlim=xlim,
     xlab=expression("Hydrologic Index "~~ {theta}))

lines(ecdf(thetaTran[thetaTran$rank == rankVector[1] &
                       thetaTran$Stress == stressVec[2]&
                       thetaTran$Phys == physVector[2],]$theta.logNskew),col="red")
legend('bottomright',legend=c(paste(stressVec[1],physVector[2]),
                              paste(stressVec[2],physVector[2])),  # text in the legend
       col=c("blue","red"),pch=15,cex=0.75)

for (ranks in rankVector) {
  for (stress in stressVec) {
    for (phys in physVector) {
      plotNormalHistogram(thetaTran[thetaTran$rank==ranks 
                                    & thetaTran$Stress == stress
                                    & thetaTran$Phys == phys,]$theta.logNskew,
                          main=paste(stress,' ',phys,' ',ranks),xlim=c(0,4),
                          xlab=expression("Transformed Hydrologic Index "~~ {theta}))
    }
  }
}

wideTheta <- dcast(thetaTran,EMT_ID~rank,value.var='theta.logNskew',mean)
thetaEval <- merge(wideTheta,Class1P80[,c(1,3,4,5,11)], by.x='EMT_ID', by.y = "CFCA/EMT ID")
write.csv(file='h:/thetas4Eval.csv',thetaEval)
write.csv(file='h:/thetasTransformed.csv',thetaTran)

OrigwideTheta<- dcast(thetaTran,EMT_ID~rank,value.var='theta',mean)
OrigTheta <-merge(OrigwideTheta,Class1P80[,c(1,3,4,5,11)], by.x='EMT_ID', by.y = "CFCA/EMT ID")


workdir= "Y:/proj/CFWI_WetlandStress/Update2018"
library(car)
#rankVector <- c( "2006-2017_P80","2007-2017_P80","2008-2017_P80","2009-2017_P80","2010-2017_P80" )
rankVector <- c("2009-2017_P80" )
stressVec <- c("Stressed","Not Stressed")
physVector <-c("Plain","Ridge")

#rankVector <- c( "2009-2017_P80" );stressVec <- c("Stressed");physVector <-c("Plain")




for (ranks in rankVector) {
  for (Phys in physVector) {
    for (stress in stressVec) {
      onestation <-
        thetaEval[thetaEval$'Stress Status in 2018' == stress &
                    thetaEval$'Physiographic Region' == Phys ,
                  c('EMT_ID',ranks,'Stress Status in 2018','Physiographic Region')]
      
      origstation <-
        OrigTheta[OrigTheta$'Stress Status in 2018' == stress &
                    OrigTheta$'Physiographic Region' == Phys ,
                  c('EMT_ID',ranks,'Stress Status in 2018','Physiographic Region')]
      
      names(onestation)[names(onestation) == ranks] <- "theta"
      names(onestation)[names(onestation) == 'Stress Status in 2018'] <-"stress"
      names(onestation)[names(onestation) == "Physiographic Region"] <-"Phys"
      names(onestation)[names(onestation) == "Hydrologically Altered"] <-"Altered"
      
      names(origstation)[names(origstation) == ranks] <- "theta"
      names(origstation)[names(origstation) == 'Stress Status in 2018'] <-"stress"
      names(origstation)[names(origstation) == "Physiographic Region"] <-"Phys"
      names(origstation)[names(origstation) == "Hydrologically Altered"] <-"Altered"
      
      source = 'Transformed'
      makeQQplots(onestation, ranks, stress, Phys, source)
      
      source = 'Original'
      makeQQplots(origstation, ranks, stress, Phys, source)
      
      source = 'Original'
      cat(paste(source, ' '))
      swTest <- shapiro.test(origstation$theta)
      cat (paste0('"shapiro.test for ","',stress,'","',Phys,'", "',ranks,'",'))
      cat(paste0(swTest$statistic, '  ', swTest$p.value, '\n'))
      
      source = 'Transformed'
      cat(paste(source, ' '))
      swTest <- shapiro.test(onestation$theta)
      cat (paste0('"shapiro.test for ","',stress,'","',Phys,'", "',ranks,'",'))
      cat(paste0(swTest$statistic, '  ', swTest$p.value, '\n'))
    }
    onestation <-
      thetaEval[, c('EMT_ID',ranks,'Stress Status in 2018','Physiographic Region')]
    names(onestation)[names(onestation) == ranks] <- "theta"
    names(onestation)[names(onestation) == 'Stress Status in 2018'] <-"stress"
    names(onestation)[names(onestation) == "Physiographic Region"] <-"Phys"

    if (phys == 'Plain'){    
      source = 'Transformed'
      plotHistoDens(onestation[onestation$Phys=='Plain' ], ranks, phys, source)
    }
    
    origstation <-
      OrigTheta[, c('EMT_ID',ranks,'Stress Status in 2018','Physiographic Region')]
    names(origstation)[names(origstation) == ranks] <- "theta"
    names(origstation)[names(origstation) == 'Stress Status in 2018'] <-"stress"
    names(origstation)[names(origstation) == "Physiographic Region"] <-"Phys"
    if (phys == 'Ridge'){    
      source = 'Original'
      plotHistoDens(origstation[origstation$Phys=='Ridge' ,], ranks, phys, source)
    }
    else {
      plotHistoDens(origstation[origstation$Phys=='Plain', ], ranks, phys, source)
    }
  }
  
}

