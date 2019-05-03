#-------------------------------------------------------------------------------
#   Developed by: Kevin A. Rodberg, Science Supervisor 
#                 Resource Evaluation Section, Water Supply Bureau, SFWMD
#                 (561) 682-6702
#
#   April 2019
#-------------------------------------------------------------------------------
#
#   Script:   EMTreportGraphs.R 
#   Purpose:  Uses data from previous runs of the following scripts:
#                WetlandStressSWFWMDsYr.R
#                WetlandStressSFWMDsYr.R
#                WetlandStressSJRWMDsYr.R
#             or use the Update2018.rdata 
#             and plot figures for the EMT report
#
#-------------------------------------------------------------------------------

#--
#   package management:
#     provide automated means for first time use of script to automatically 
#	    install any new packages required for this code, with library calls 
#	    wrapped in a for loop.
#--
list.of.pkgs <-  c("ggplot2")

new.pkgs <- list.of.pkgs[!(list.of.pkgs %in% installed.packages()[, "Package"])]

if (length(new.pkgs)){ install.packages(new.pkgs) }
for (pkg in list.of.pkgs){ library(pkg,character.only = TRUE) }


dfList <- list(AllStations_SJ,AllStations_SW,AllStations_SF[,2:5])
AllStations<- merge(x=do.call("rbind",dfList), 
                    y=Class1P80[,c(1,3,6,7,12,16,20)], 
                    by.x="STATION",by.y="STATION")

names(AllStations) <-  c("STATION",    "DATE",    "WLValue",    "approx",
                         "EMT_ID" ,    "SiteName" ,    "Status" ,    "Phys",
                         "RefEdge",    "P802009_2017"  )
write.csv(Class1P80[,c(1,3,6,7,12,16,20)],paste0('C:\\Users\\krodberg\\Desktop\\Class1P80.csv'))

AllStations$STATION <- as.character(AllStations$STATION)
plotTStheta <- function(fileName,AllStations,stress,phys){
  graphics.off()
  stn = paste(stress,phys)
  p <- ggplot(AllStations[AllStations$DATE > as.Date('2009-01-01'),], aes(x=DATE,y=approx-RefEdge,colour=STATION)) +    
    geom_line( ) +
    labs(title=stn,y = "Hydrologic Index (theta in feet)") +
    scale_x_date(date_breaks = "12 month", date_labels =  "%m-%d-%Y")  +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          legend.position ="bottom") 
    
    ggsave(filename=fileName,width=10,height=6.66,units="in",dpi=300)
}
plotTSwl <- function(fileName,AllStations,stress,phys){
  graphics.off()
  stn = paste(stress,phys)
#  p <- ggplot(data=AllStations, aes(x=DATE,y=approx,colour=STATION)) +    
  p <- ggplot(data=AllStations, aes(x=DATE)) +    
    geom_point(aes(y=WLValue,colour=STATION),na.rm=T,size=.6) +
    labs(title=stn,y = "Hydrologic Index (theta in feet)") +
    scale_x_date(date_breaks = "12 month", date_labels =  "%m-%d-%Y")  +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          legend.position ="bottom") 
  ggsave(filename=fileName,width=10,height=6.66,units="in",dpi=300)
}
for (phys in unique(AllStations$Phys)){
  for (stress in unique(AllStations$Status)){
    fileName = paste0('C:\\Users\\krodberg\\Desktop\\',stress,'_',phys,'_theta.png')
    plotTStheta(fileName,
           AllStations[AllStations$Status==stress & 
                         AllStations$Phys==phys,],
           stress,phys)
    fileName = paste0('C:\\Users\\krodberg\\Desktop\\',stress,'_',phys,'_wl.png')
  
    plotTSwl(fileName,
           AllStations[AllStations$Status==stress & 
                         AllStations$Phys==phys,],
           stress,phys)
  }
}

