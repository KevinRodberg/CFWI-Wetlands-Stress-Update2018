
list.of.pkgs <-  c("readr","dplyr","zoo","ggplot2", "reshape2", "data.table", 
                   "scales", "purrr","readxl","future","listenv")

new.pkgs <- list.of.pkgs[!(list.of.pkgs %in% installed.packages()[, "Package"])]

if (length(new.pkgs)){ install.pkgs(new.pkgs) }
for (pkg in list.of.pkgs){ library(pkg,character.only = TRUE) }

baseDir <- "//ad.sfwmd.gov/dfsroot/data/wsd/SUP/proj/CFWI_WetlandStress/Update2018/"
files <- c(paste0(baseDir,"SFWMD_P20.csv"),
                paste0(baseDir,"SWFWMD_P20.csv"),
                paste0(baseDir,"SJRWMD_P20.csv"))

AllP20<-map_df(files, read.table, sep=",",header = TRUE, stringsAsFactors = FALSE)

baseDir <- "//ad.sfwmd.gov/dfsroot/data/wsd/SUP/proj/CFWI_WetlandStress/Update2018/"
files <- c(paste0(baseDir,"/SFWMD/SFWMD_DataTable.csv"),
           paste0(baseDir,"/SWFWMD/SWFWMD_DataTable.csv"),
           paste0(baseDir,"/SJRWMD/SJRWMD_DataTable.csv"))

AllWL<-map_df(files, read.table, sep=",",header = TRUE, stringsAsFactors = FALSE)

Monthly<- AllWL %>%
  group_by(STATION,format(as.Date(DATE), "%Y-%m-01")) %>%
  summarize(MeanWL = mean(Value, na.rm=TRUE))

names(Monthly)<- c("STATION",'YRMONTH',"MeanWL")
Monthly$YRMONTH <- as.Date(Monthly$YRMONTH)
unique.stations <-unique(Monthly$STATION)
unique.stations<-unique.stations[!is.na(unique.stations)]


plotHisto <- function(fileName,OneStation){
  graphics.off()
  p <- ggplot(OneStation[!is.na(OneStation$MeanWL),], aes(MeanWL)) +    
    geom_histogram(bins=20,color="black", fill="lightblue") +
    labs(title=stn,x = "Mean Water Level (Feet NAVD88)")
  ggsave(filename=fileName)
}
plotTS <- function(fileName,OneStation){
  graphics.off()
  p <- ggplot(OneStation, aes(YRMONTH,MeanWL)) +    
    geom_line( ) +
    stat_smooth(aes(x = YRMONTH), 
                se = F, method = "lm", formula = y ~ poly(x, 10)) +
    labs(title=stn,y = "Mean Water Level (Feet NAVD88)") +
    scale_x_date(date_breaks = "12 month", date_labels =  "%m-%d-%Y")  +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 
  ggsave(filename=fileName)
}

x = 0
plan(multiprocess)
results <- listenv()

for (stn in unique.stations){
  x= x + 1
  cat(paste0(stn,'\n'))
  filename =paste0(baseDir,'CFWIfigures/monthly_',stn,'_histo.png')
  OneStation <- Monthly[Monthly$STATION ==stn,]
  results[[x]] <- future({plotHisto(filename,OneStation)})
}

for (stn in unique.stations){
  cat(paste0(stn,'\n'))
  filename =paste0(baseDir,'CFWIfigures/monthly_',stn,'_hydrog.png')
  OneStation <- Monthly[Monthly$STATION ==stn,]
  results[[x]] <- future({plotTS(filename,OneStation)})
}
