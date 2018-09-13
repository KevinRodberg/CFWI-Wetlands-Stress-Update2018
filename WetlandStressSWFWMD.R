
list.of.pkgs <-  c("readr","dplyr","zoo","ggplot2", "reshape2", "data.table",
                   "future","listenv","readxl","purrr")

new.pkgs <- list.of.pkgs[!(list.of.pkgs %in% installed.packages()[, "Package"])]

if (length(new.pkgs)){ install.pkgs(new.pkgs) }
for (pkg in list.of.pkgs){ library(pkg,character.only = TRUE) }


workdir = "//ad.sfwmd.gov/dfsroot/data/wsd/SUP/proj/CFWI_WetlandStress/Update2018/SWFWMD/"

#Station.Coordinates <- utils::read.csv(paste0(workdir,"StationCoordinates.csv"))
#Station.DatumAdj <- readr::read_csv(paste0(workdir,"StationDatumAdj.csv"), skip = 6)
#Station.DatumAdj <- as.data.frame(Station.DatumAdj[,names(Station.DatumAdj)[c(1,4)]])
#stations.SFWMD <- base::merge(Station.Coordinates,Station.DatumAdj, by.x="DBKEY", by.y="Point" )
#stations.SFWMD$Point <- NULL

drange = as.data.frame(seq.Date(as.Date('2006/1/1'),as.Date('2017/12/31'),by=1))
names(drange)= 'DATE'
dfPOR <- drange
setwd(workdir)
xlFiles <-list.files(pattern = "*.xlsx")
SWF_unpivot<- NULL
for (file in xlFiles){

  sheets <- excel_sheets(file)
  for (sht in sheets) {
    cat(paste0(file,'::',sht,'\n'))
  }
  
  df <- map_df(sheets, ~ read_excel(file, sheet = .x, skip = 0))
  names(df) <- c("Site ID","STATION","Parameter","DATE","value",
                 "Units","No of Records","Data Source","Status","Quality Description")
  df$DATE <- as.Date(df$DATE)
  if (nrow(df[is.na(df$STATION),])) {cat(paste0(file,':[',sheets,']'))}
  df.Wide <- dcast(df,DATE~STATION,mean)
  df.AllDates<-merge(drange,df.Wide[df.Wide$DATE>= as.Date('2006/01/01'),], all.x=TRUE)
  df.unpivot <- melt(df.AllDates,id='DATE')
  SWF_unpivot <-rbind(SWF_unpivot,df.unpivot)
}
names(SWF_unpivot)<-c('DATE','STATION','value')
SWF_unpivot[is.nan(SWF_unpivot$value),]$value=NA
Wetlands.SWF<- SWF_unpivot[order(SWF_unpivot$STATION,SWF_unpivot$DATE),]
Wetlands.SWF<- Wetlands.SWF[,c('STATION','DATE','value')]

# SWF_Pivot = dcast(Wetlands.SWF,DATE ~ STATION,mean)

names(Wetlands.SWF) <- c("STATION","DATE","Value")

unique.stations <- unique(Wetlands.SWF$STATION)
AllStations <- data.frame()

cat (paste0('Interpolating and imputing missing data','\n'))

drange = as.data.frame(seq.Date(as.Date('2006/1/1'),as.Date('2017/12/31'),by=1))
names(drange)= 'DATE'

for (dbk in unique.stations){
  cat(paste(dbk,'\n'))
  OneStation <- Wetlands.SWF[Wetlands.SWF$STATION ==dbk,c(2,3)] 
  OneStation.Alldates<-merge(drange,OneStation, all.x=TRUE) %>% 
    mutate(approx = na.approx(Value,rule=1,na.rm=FALSE)) 
  OneStation.Alldates <- cbind(dbk,OneStation.Alldates)
  AllStations <- rbind(AllStations,OneStation.Alldates)
}

AllStations <-AllStations[AllStations$DATE >= '2006-01-01' 
                                 & AllStations$DATE < '2018-01-01', ]

names(AllStations)[names(AllStations) == 'dbk'] <- 'STATION'
start = 2011
end = 2017
drange = 2011
for (drange in seq(start,end)){
  ich = paste0('2006-',drange)
  AllStations[format.Date(AllStations$DATE, "%Y") <= as.character(drange),ich] <-as.double(drange)
}


cat (paste0('Calculating Percentile Ranks','\n'))

PivotPranks <- NULL
for (yr in seq(start,end)){
  ich = paste0('2006-',yr)
  qStations <- AllStations[!is.na(AllStations$approx) & 
                             AllStations$DATE <= as.Date(paste0(yr,'-12-31')),]
  
  QByYr<-as.data.table(qStations)[,as.list(quantile(approx,probs=c(.2, .5))), by=STATION]
  
  names(QByYr)= c("STATION","P80","P50")
  QByYr$drange <- ich
  PivotPranks<-rbind(PivotPranks,QByYr)
}

cat (paste0('Exporting data from calculations','\n'))

unique.stations <-unique(PivotPranks$STATION)
Pranks <- melt(PivotPranks)
names(Pranks)=c("STATION","DateRange","prank","value")

PrankFile = paste0(workdir,'SWFWMD_Pranks.csv')
csvStatus %<-% write.csv(Pranks,PrankFile, row.names=FALSE)

DataTable = paste0(workdir,'SWFWMD_DataTable.csv')
csvStatus %<-% write.csv(AllStations[,1:4],DataTable, row.names=FALSE)

p80<-dcast(Pranks[Pranks$prank=='P80',],STATION~DateRange+prank,mean )
PrankFile = paste0(workdir,'../SWFWMD_P80.csv')
csvStatus %<-% write.csv(p80,PrankFile, row.names=FALSE)

cat (paste0('Exporting charts','\n'))
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


plan(multiprocess)
results <- listenv()

unique.stations <- unique(Pranks$STATION)
Pranks$value <- round(Pranks$value,2)

x = 0
for (stn in unique.stations){
  x= x + 1
  cat(paste0(stn,'\n'))
  filename =paste0(workdir,'figures/',stn,'_ranks.png')
  OneStation <- Pranks[Pranks$STATION ==stn,]
  results[[x]] <- future({plotLines(filename,OneStation)})
  
  filename =paste0(workdir,'figures/',stn,'_histo.png')
  OneStation <- AllStations[AllStations$STATION ==stn,]
  results[[x]] <- future({plotHisto(filename,OneStation)})
  
  x= x + 1
  filename =paste0(workdir,'figures/',stn,'_histoDensity.png')
  results[[x]] <- future({plotHistoDens(filename,OneStation)})
  
  x= x + 1
  filename =paste0(workdir,'figures/',stn,'_hydrog.png')
  results[[x]] <- future({plotTS(filename,OneStation)})
}

