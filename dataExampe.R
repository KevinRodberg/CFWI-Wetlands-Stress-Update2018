library(TTR)
library(zoo)
library(dplyr)
library(scales)
library(lubridate)

workdir= "Y:/proj/CFWI_WetlandStress/Update2018"
setwd(workdir)
EMT_ID <- read_csv("EMT_ID.csv")
Class1Wetlands <- read_excel("Class 1 Wetland Info for Analysis ALLv1.xlsx", na = "NA")

names(Wetlands.SFWMD)<-c("STATION","DBKEY","DATE","Value","Qualifer")
allWetlands <- rbind(Wetlands.SFWMD[,c(1,3,4)],Wetlands.SJR)
allWetlands <- rbind(allWetlands,Wetlands.SWF)

AllWtlsByID <- merge(allWetlands, EMT_ID, by="STATION")
colnames(Class1Wetlands)[1]<-"EMT_ID"
AllWtlsByID <- merge(AllWtlsByID,Class1Wetlands[,c(1,2,4,6,10)], by="EMT_ID" )
StoNS <- c('SF-YK', 'SJ-AJ','SJ-LH','SW-LF', 'SJ-LI','SW-QD')

someWtlds <-AllWtlsByID[AllWtlsByID$EMT_ID %in% StoNS,]
colnames(someWtlds)[8]<-"ERE88"
someWtlds$theta <- someWtlds$ERE88 - someWtlds$Value
write.csv(file='someWtlds.csv',someWtlds)

colnames(AllWtlsByID)[8]<-"ERE88"
AllWtlsByID$theta <- AllWtlsByID$ERE88 - AllWtlsByID$Value
graphics.off
filename = paste0('thetaPlots.png')

p <- ggplot(data=subset(someWtlds, !is.na(theta)), aes(DATE,theta,color=EMT_ID)) +    
  geom_line() +
  theme(legend.position="bottom") + 
  scale_y_continuous(limits=c(-5,12)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_x_date(breaks = pretty_breaks(18)) +
  labs(y = "Theta (ERE - WL NAVD88)")
ggsave(filename=filename,width=10,height=6.66,units="in",dpi=300)
dev.off()  


someWtlds$monthly <- as.Date(paste0(substr(someWtlds$DATE,1,4),'-',
                                    substr(someWtlds$DATE,6,7),'-15'),"%Y-%m-%d")
AllWtlsByID$monthly <- as.Date(paste0(substr(AllWtlsByID$DATE,1,4),'-',
                                      substr(AllWtlsByID$DATE,6,7),'-15'),"%Y-%m-%d")

someWtldsMonthlymean <- aggregate(someWtlds$Value,list(someWtlds$EMT_ID,someWtlds$ERE88, 
                                                       someWtlds$monthly), 
                                  FUN=mean, na.rm=TRUE, na.action=NULL)
AllWtlsByIDMonthlymean <- aggregate(AllWtlsByID$Value,list(AllWtlsByID$EMT_ID,AllWtlsByID$ERE88, 
                                                AllWtlsByID$monthly), 
                         FUN=mean, na.rm=TRUE, na.action=NULL)

                                                                                                         FUN=mean, na.rm=TRUE, na.action=NULL)
names(someWtldsMonthlymean) <- c('EMT_ID','ERE88','DATE','Value')
names(AllWtlsByIDMonthlymean) <- c('EMT_ID','ERE88','DATE','Value')


someWtldsMonthlymean$theta <- someWtldsMonthlymean$ERE88 - someWtldsMonthlymean$Value
AllWtlsByIDMonthlymean$theta <- AllWtlsByIDMonthlymean$ERE88 - AllWtlsByIDMonthlymean$Value


graphics.off
filename = paste0('thetaMonthlyPlots.png')
p <- ggplot(data=subset(someWtldsMonthlymean, !is.na(theta)), aes(DATE,theta,color=EMT_ID)) +    
  geom_line() +
  theme(legend.position="bottom") + 
  scale_y_continuous(limits=c(-5,12)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_x_date(breaks = pretty_breaks(18)) +
  labs(y = "Theta (ERE - WL NAVD88)")
ggsave(filename=filename,width=10,height=6.66,units="in",dpi=300)
dev.off() 

write.csv(file='someWtldsMonthly.csv',someWtldsMonthlymean)
write.csv(file='allWtldsMonthly.csv',AllWtlsByIDMonthlymean)

fnrollmean <- function (x) {
  if (length(x) < 12) {
    rep(NA,length(x)) 
  } else {
    rollmean(x,12,align="left",na.pad=TRUE)
  }
}

movingAvg <- as.data.table(arrange(someWtldsMonthlymean[someWtldsMonthlymean$DATE>='2005-01-01' &
                                                          someWtldsMonthlymean$DATE <'2018-01-01' ,],EMT_ID,DATE))
result <- movingAvg %>% group_by(EMT_ID) %>% 
  mutate(rollavg=fnrollmean(theta))
write.csv(file='movingAvg.csv',result)

movingAvg <- as.data.table(arrange(AllWtlsByIDMonthlymean[AllWtlsByIDMonthlymean$DATE>='2005-01-01' &
                                                            AllWtlsByIDMonthlymean$DATE <'2018-01-01' ,],EMT_ID,DATE))
result <- movingAvg %>% group_by(EMT_ID) %>% 
  mutate(rollavg=fnrollmean(theta))
write.csv(file='AllmovingAvg.csv',result)


graphics.off
filename = paste0('movingAvgPlots.png')
p <- ggplot(data=subset(result, !is.na(rollavg)), aes(DATE,rollavg,color=EMT_ID)) +    
  geom_line() +
  theme(legend.position="bottom") + 
  scale_y_continuous(limits=c(-5,12)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_x_date(breaks = pretty_breaks(18)) +
  labs(y = "12 Month rolling avg. Theta (ERE - WL NAVD88)")
ggsave(filename=filename,width=10,height=6.66,units="in",dpi=300)
dev.off() 
