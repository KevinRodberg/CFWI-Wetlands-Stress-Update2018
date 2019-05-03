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

if (length(new.pkgs)){ install.pkgs(new.pkgs) }
for (pkg in list.of.pkgs){ library(pkg,character.only = TRUE) }


workdir = "//ad.sfwmd.gov/dfsroot/data/wsd/SUP/proj/CFWI_WetlandStress/Update2018/SJRWMD/"
workOutdir = "//ad.sfwmd.gov/dfsroot/data/wsd/SUP/proj/CFWI_WetlandStress/Update2018/SJRWMD/StartYr/"

#Station.Coordinates <- utils::read.csv(paste0(workdir,"StationCoordinates.csv"))
#Station.DatumAdj <- readr::read_csv(paste0(workdir,"StationDatumAdj.csv"), skip = 6)
#Station.DatumAdj <- as.data.frame(Station.DatumAdj[,names(Station.DatumAdj)[c(1,4)]])
#stations.SFWMD <- base::merge(Station.Coordinates,Station.DatumAdj, by.x="DBKEY", by.y="Point" )
#stations.SFWMD$Point <- NULL

df <-NULL
file <- paste0(workdir ,'Class 1 Wetlands NAVD 88.xlsx')
sheets <- excel_sheets(file)
df <- map_df(sheets, ~ read_excel(file, sheet = .x, skip = 0))
names (df)
df$DATE <- as.Date(df$DATE)
drange = as.data.frame(seq.Date(as.Date('2006/1/1'),as.Date('2017/12/31'),by=1))
names(drange)= 'DATE'
dfPOR<-merge(drange,df[df$DATE>= as.Date('2006/01/01'),], by='DATE')
SJR_unpivot <- melt(dfPOR,id='DATE')
names(SJR_unpivot)<-c('DATE','STATION','value')
Wetlands.SJR<- SJR_unpivot[order(SJR_unpivot$STATION,SJR_unpivot$DATE),]
Wetlands.SJR<- Wetlands.SJR[,c('STATION','DATE','value')]

# SJR_Pivot = dcast(Wetlands.SJR,DATE ~ STATION,mean)

names(Wetlands.SJR) <- c("STATION","DATE","Value")

unique.stations <- unique(Wetlands.SJR$STATION)
AllStations_SJ <- data.frame()

cat (paste0('Interpolating and imputing missing data','\n'))

for (dbk in unique.stations[unique.stations != 'Date']){
  cat(paste(dbk,'\n'))
  OneStation <- Wetlands.SJR[Wetlands.SJR$STATION ==dbk,c(2,3)] %>% 
    mutate(approx = na.approx(Value,rule=2)) 
  OneStation <- cbind(dbk,OneStation)
  AllStations_SJ <- rbind(AllStations_SJ,OneStation)
}
names(AllStations_SJ)[names(AllStations_SJ) == 'dbk'] <- 'STATION'


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
  qStations <- AllStations_SJ[!is.na(AllStations_SJ$approx) & 
                             AllStations_SJ$DATE >= as.Date(paste0(yr,'-01-01')),]
  QByYr<-as.data.table(qStations)[,as.list(quantile(approx,probs=c(.2, .5))), by=STATION]
  
  names(QByYr)= c("STATION","P80","P50")
  QByYr$drange <- ich
  PivotPranks<-rbind(PivotPranks,QByYr)
}

cat (paste0('Exporting data from calculations','\n'))

unique.stations <-unique(PivotPranks$STATION)
Pranks <- melt(PivotPranks)
names(Pranks)=c("STATION","DateRange","prank","value")

PrankFile = paste0(workOutdir,'../SJRWMD_Pranks.csv')
csvStatus %<-% write.csv(Pranks,PrankFile, row.names=FALSE)
DataTable = paste0(workOutdir,'../SJRWMD_DataTable.csv')
csvStatus %<-% write.csv(AllStations_SJ[,1:4],DataTable, row.names=FALSE)

p80<-dcast(Pranks[Pranks$prank=='P80',],STATION~DateRange+prank,mean )
PrankFile = paste0(workOutdir,'../SJRWMD_P80.csv')
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
  OneStation <- AllStations_SJ[AllStations_SJ$STATION ==stn,]
  results[[x]] <- future({plotHisto(filename,OneStation)})
  
  x= x + 1
  filename =paste0(workOutdir,'figures/',stn,'_histoDensity.png')
  results[[x]] <- future({plotHistoDens(filename,OneStation)})
  
  x= x + 1
  filename =paste0(workOutdir,'figures/',stn,'_hydrog.png')
  results[[x]] <- future({plotTS(filename,OneStation)})
}

plan(sequential)