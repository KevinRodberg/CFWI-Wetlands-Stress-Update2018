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


workdir = "//ad.sfwmd.gov/dfsroot/data/wsd/SUP/proj/CFWI_WetlandStress/Update2018/SWFWMD/"
workOutdir = "//ad.sfwmd.gov/dfsroot/data/wsd/SUP/proj/CFWI_WetlandStress/Update2018/SWFWMD/StartYr/"


drange = as.data.frame(seq.Date(as.Date('2006/1/1'),as.Date('2017/12/31'),by=1))
names(drange)= 'DATE'
dfPOR <- drange
setwd(workdir)
xlFiles <-list.files(pattern = "*.xlsx")
SWF_unpivot<- NULL
file = xlFiles[34]
#for (file in xlFiles[30:35]){
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
#  df.Wide <- dcast(df,DATE~STATION+`Site ID`,mean)
  df.AllDates<-merge(drange,df.Wide[df.Wide$DATE>= as.Date('2006/01/01'),], all.x=TRUE)
  df.unpivot <- melt(df.AllDates,id='DATE')
  SWF_unpivot <-rbind(SWF_unpivot,df.unpivot)
}
# quickList<- as.data.frame(unique(SWF_unpivot$variable))
# write.csv(quickList,'h:/quiclist.csv')
names(SWF_unpivot)<-c('DATE','STATION','value')
result <- tryCatch({SWF_unpivot[is.nan(SWF_unpivot$value),]$value=NA}, 
                   warning = function(war) {  print(paste("MY_WARNING:  ",war))}, 
                   error = function(err) {print("No NA's found") }) 
Wetlands.SWF<- SWF_unpivot[order(SWF_unpivot$STATION,SWF_unpivot$DATE),]
Wetlands.SWF<- Wetlands.SWF[,c('STATION','DATE','value')]

# SWF_Pivot = dcast(Wetlands.SWF,DATE ~ STATION,mean)

names(Wetlands.SWF) <- c("STATION","DATE","Value")

unique.stations <- unique(Wetlands.SWF$STATION)
AllStations_SW <- data.frame()

cat (paste0('Interpolating and imputing missing data','\n'))

drange = as.data.frame(seq.Date(as.Date('2006/1/1'),as.Date('2017/12/31'),by=1))
names(drange)= 'DATE'

for (dbk in unique.stations){
  cat(paste(dbk,'\n'))
  OneStation <- Wetlands.SWF[Wetlands.SWF$STATION ==dbk,c(2,3)] 
  OneStation.Alldates<-merge(drange,OneStation, all.x=TRUE) %>% 
    mutate(approx = na.approx(Value,rule=1,na.rm=FALSE)) 
  OneStation.Alldates <- cbind(dbk,OneStation.Alldates)
  AllStations_SW <- rbind(AllStations_SW,OneStation.Alldates)
}

AllStations_SW <-AllStations_SW[AllStations_SW$DATE >= '2006-01-01' 
                                 & AllStations_SW$DATE < '2018-01-01', ]

names(AllStations_SW)[names(AllStations_SW) == 'dbk'] <- 'STATION'



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
  qStations <- AllStations_SW[!is.na(AllStations_SW$approx) & 
                             AllStations_SW$DATE >= as.Date(paste0(yr,'-01-01')),]
  QByYr<-as.data.table(qStations)[,as.list(quantile(approx,probs=c(.2, .5))), by=STATION]
  
  names(QByYr)= c("STATION","P80","P50")
  QByYr$drange <- ich
  PivotPranks<-rbind(PivotPranks,QByYr)
}

cat (paste0('Exporting data from calculations','\n'))

unique.stations <-unique(PivotPranks$STATION)
Pranks <- melt(PivotPranks)
names(Pranks)=c("STATION","DateRange","prank","value")

PrankFile = paste0(workOutdir,'../SWFWMD_Pranks.csv')
csvStatus %<-% write.csv(Pranks,PrankFile, row.names=FALSE)
DataTable = paste0(workOutdir,'../SWFWMD_DataTable.csv')
csvStatus %<-% write.csv(AllStations_SW[,1:4],DataTable, row.names=FALSE)

p80<-dcast(Pranks[Pranks$prank=='P80',],STATION~DateRange+prank,mean )
PrankFile = paste0(workOutdir,'../SWFWMD_P80.csv')
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
  OneStation <- AllStations_SW[AllStations_SW$STATION ==stn,]
  results[[x]] <- future({plotHisto(filename,OneStation)})
  
  x= x + 1
  filename =paste0(workOutdir,'figures/',stn,'_histoDensity.png')
  results[[x]] <- future({plotHistoDens(filename,OneStation)})
  
  x= x + 1
  filename =paste0(workOutdir,'figures/',stn,'_hydrog.png')
  results[[x]] <- future({plotTS(filename,OneStation)})
}
plan(sequential)
