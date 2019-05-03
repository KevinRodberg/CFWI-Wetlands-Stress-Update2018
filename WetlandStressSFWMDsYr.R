#-------------------------------------------------------------------------------
#  Developed by: Kevin A. Rodberg, Science Supervisor 
#                 Resource Evaluation Section, Water Supply Bureau, SFWMD
#                 (561) 682-6702
#
#  January 2019
#
#  Script is provided to import spreadsheet data 
#  and calculate percentile rankings and plot figures
#
#-------------------------------------------------------------------------------

#--
#   package management:
#     provide automated means for first time use of script to automatically 
#	  install any new packages required for this code, with library calls 
#	  wrapped in a for loop.
#--
list.of.pkgs <-  c("readr","dplyr","zoo","ggplot2", "reshape2", "data.table",
                   "future","listenv","readxl","purrr")

new.pkgs <- list.of.pkgs[!(list.of.pkgs %in% installed.packages()[, "Package"])]

if (length(new.pkgs)){ install.packages(new.pkgs) }
for (pkg in list.of.pkgs){ library(pkg,character.only = TRUE) }

workdir = "//ad.sfwmd.gov/dfsroot/data/wsd/SUP/proj/CFWI_WetlandStress/Update2018/SFWMD/"
workOutdir = "//ad.sfwmd.gov/dfsroot/data/wsd/SUP/proj/CFWI_WetlandStress/Update2018/SFWMD/StartYr/"

Station.Coordinates <- utils::read.csv(paste0(workdir,"StationCoordinates.csv"))
Station.DatumAdj <- readr::read_csv(paste0(workdir,"StationDatumAdj.csv"), skip = 6)
Station.DatumAdj <- as.data.frame(Station.DatumAdj[,names(Station.DatumAdj)[c(1,4)]])
stations.SFWMD <- base::merge(Station.Coordinates,Station.DatumAdj, by.x="DBKEY", by.y="Point" )
stations.SFWMD$Point <- NULL

#---
#  Read and merge 3 csv files for wetland waterlevels
#---
TibetButler <- read_csv(paste0(workdir,"TibetButler.csv"),
                        col_types = cols(`Daily Date` = col_date(format = "%d-%b-%y"), 
                                         `Revision Date` = col_skip()))
Wetlands.SFWMD <- as.data.frame(TibetButler[!is.na(TibetButler[,"DBKEY"]),c(1,2,3,4,5)])

WalkerRanch <- readr::read_csv(paste0(workdir,"WalkerRanch.csv"),
                        col_types = cols(`Daily Date` = col_date(format = "%d-%b-%y"),
                                         `Revision Date` = col_skip()))
Wetlands.SFWMD <- base::rbind(Wetlands.SFWMD,
                              as.data.frame(WalkerRanch[!is.na(WalkerRanch[,"DBKEY"]),
                                                        c(1,2,3,4,5)]))

SplitOak <- readr::read_csv(paste0(workdir,"SplitOak.csv"),
                            col_types = cols(`Daily Date` = col_date(format = "%d-%b-%Y"), 
                                             `Revision Date` = col_skip()))
Wetlands.SFWMD <- base::rbind(Wetlands.SFWMD,
                              as.data.frame(SplitOak[!is.na(SplitOak[,"DBKEY"]),c(1,2,3,4,5)]))

names(Wetlands.SFWMD) <- c("Station","DBKEY","DATE","Value","Qualifer" )

# Assign NA to records with certain qualifiers
skipQualifiers = c('M', 'N', 'PT',  '?',  'U')
Wetlands.SFWMD[Wetlands.SFWMD$Qualifer %in% skipQualifiers, ]$`Data Value` = NA                      
unique.dbkeys <- unique(Wetlands.SFWMD$DBKEY)
AllStations_SF <- data.frame()

cat (paste0('Interpolating and imputing missing data','\n'))

drange = as.data.frame(seq.Date(as.Date('2006/1/1'),as.Date('2017/12/31'),by=1))
names(drange)= 'DATE'

for (dbk in unique.dbkeys){
  cat(paste(dbk,'\n'))
  OneStation <- Wetlands.SFWMD[Wetlands.SFWMD$DBKEY ==dbk,c(3,4)]
  OneStation.Alldates<-merge(drange,OneStation, all.x=TRUE) %>% 
    mutate(approx = na.approx(Value,rule=1,na.rm=FALSE)) 
  OneStation.Alldates <- cbind(dbk,OneStation.Alldates)
  AllStations_SF <- rbind(AllStations_SF,OneStation.Alldates)
}

# -- Next 2 assignments statements for NGVD to NAVD adjustment specific to SFWMD

AllStations_SF <- merge(stations.SFWMD[,c("STATION","DBKEY","Height")], 
                     AllStations_SF[AllStations_SF$DATE >= '2006-01-01' 
                                 & AllStations_SF$DATE < '2018-01-01', ], 
                     by ="DBKEY",by.y="dbk")
AllStations_SF$approx <- AllStations_SF$approx + AllStations_SF$Height
AllStations_SF$Height <- NULL

# Full Date range handled in previous steps
# start = 2006
# end = 2017

# for (drange in seq(start,end)){
#   ich = paste0('2006-',drange)
#   AllStations_SF[format.Date(AllStations_SF$DATE, "%Y") <= as.character(drange),ich] <-as.double(drange)
# }

cat (paste0('Calculating Percentile Ranks','\n'))
start = 2006
end = 2011

PivotPranks <- NULL
for (yr in seq(start,end)){
  ich = paste0(yr,'-2017')
  qStations <- AllStations_SF[!is.na(AllStations_SF$approx) & 
                             AllStations_SF$DATE >= as.Date(paste0(yr,'-01-01')),]
  
  QByYr<-as.data.table(qStations)[,as.list(quantile(approx,probs=c(.2, .5))), by=STATION]
  
  names(QByYr)= c("STATION","P80","P50")
  QByYr$drange <- ich
  PivotPranks<-rbind(PivotPranks,QByYr)
}

cat (paste0('Exporting data from calculations','\n'))

Pranks <- melt(as.data.frame(PivotPranks))
names(Pranks)=c("STATION","DateRange","prank","value")
PrankFile = paste0(workOutdir,'../SFWMD_Pranks.csv')
csvStatus %<-% write.csv(Pranks,PrankFile, row.names=FALSE)

DataTable = paste0(workOutdir,'../SFWMD_DataTable.csv')
AllStations_SF <- AllStations_SF[order(AllStations_SF$STATION,AllStations_SF$DATE),]
csvStatus %<-% write.csv(AllStations_SF[,-c(1)],DataTable, row.names=FALSE)

p80<-dcast(Pranks[Pranks$prank=='P80',],STATION~DateRange+prank,mean )
PrankFile = paste0(workOutdir,'../SFWMD_P80.csv')
csvStatus %<-% write.csv(p80,PrankFile, row.names=FALSE)

cat (paste0('Exporting charts','\n'))
#---
#  Define plotting functions
#---
plotLines <- function(fileName,OneStation){
  graphics.off()
  p <- ggplot(OneStation, aes(DateRange,value,group=prank),label=value) +    
    geom_line(aes(color=prank),size=1) + 
    geom_point(aes(color=prank),size=2) + 
    geom_text(aes(label=value), hjust=-.2,  vjust=0) +
    theme(legend.position="bottom") + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    labs(title=stn,y = "Water Level (Feet NAVD88)")
  ggsave(filename=fileName,width=10,height=6.66,units="in",dpi=300)
}
plotHisto <- function(fileName,OneStation){
  graphics.off()
  p <- ggplot(OneStation[!is.na(OneStation$approx),], aes(approx)) +    
    geom_histogram(bins=20,color="black", fill="lightblue") +
    labs(title=stn,x = "Water Level (Feet NAVD88)")
  ggsave(filename=fileName,width=10,height=6.66,units="in",dpi=300)
}
plotHistoDens <- function(fileName,OneStation){
  graphics.off()
  p <- ggplot(OneStation[!is.na(OneStation$approx),], aes(approx)) +    
    geom_histogram(aes(y=..density..),bins=30,color="black", fill="white") +
    geom_density(alpha=.2,fill="#FF6666") +
    labs(title=stn,x = "Water Level (Feet NAVD88)")
  ggsave(filename=fileName,width=10,height=6.66,units="in",dpi=300)
}
plotTS <- function(fileName,OneStation){
  graphics.off()
  p <- ggplot(OneStation, aes(DATE,approx)) +    
    geom_line( ) +
    stat_smooth(aes(x = DATE), 
                se = F, method = "lm", formula = y ~ poly(x, 10)) +
    labs(title=stn,y = "Water Level (Feet NAVD88)") +
    scale_x_date(date_breaks = "12 month", date_labels =  "%m-%d-%Y")  +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 
  ggsave(filename=fileName,width=10,height=6.66,units="in",dpi=300)
}

#--
#  Set environment for mutliprocessing
#--
plan(multisession, gc = TRUE)
results <- listenv()

unique.stations <- unique(Pranks$STATION)
Pranks$value <- round(Pranks$value,2)

#---
#  Create plots for each station using multiprocessing "future" function
#---
x = 0
for (stn in unique.stations){
  x= x + 1
  cat(paste0(stn,'\n'))
  filename =paste0(workOutdir,'figures/',stn,'_ranks.png')
  OneStation <- Pranks[Pranks$STATION ==stn,]
  results[[x]] <- future({plotLines(filename,OneStation)})
  
  filename =paste0(workOutdir,'figures/',stn,'_histo.png')
  OneStation <- AllStations_SF[AllStations_SF$STATION ==stn,]
  results[[x]] <- future({plotHisto(filename,OneStation)})
  
  x= x + 1
  filename =paste0(workOutdir,'figures/',stn,'_histoDensity.png')
  results[[x]] <- future({plotHistoDens(filename,OneStation)})
  
  x= x + 1
  filename =paste0(workOutdir,'figures/',stn,'_hydrog.png')
  results[[x]] <- future({plotTS(filename,OneStation)})
}

plan(sequential)