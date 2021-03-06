############################
# BRC_Plots.R
# BRC_DailyReportVisuals.Rproj

# Generates plots of data from BRC daily data

# Created August 24, 2015
# A Putt
#############################

library(lattice)

source("BRC_DailyUpload.R") #allData contains the uploaded data

head(allData)

#############
# Create a dummy vector for x axis labels
temp <- subset(allData,year=="2010")
temp <- subset(temp,shortdate %in% c("Jan-01","Feb-01","Mar-01","Apr-01","May-01","Jun-01","Jul-01","Aug-01","Sep-01","Oct-01","Nov-01","Dec-01"))
at <- temp$daynumber
labels <- temp$shortdate

############
# CARPENTER
# If you want to add anything to the plot, such as the date of ko arrival, it is convenient to be able to plot the day number
# You will need to pull the dates manually and find the day number so that they can be plotted for any year; i.e., the axis is general to all years
# Create day number vectors for dates of ko in streamwalks and bt up streams in 2014
#vertical <- subset(allData,shortdate %in% c("Aug-19","Sep-03","Sep-16","Oct-01") & year=="2010")$daynumber
vertical <- subset(allData,shortdate %in% c("May-14","Jun-01","Jun-12","Aug-19","Sep-03") & year=="2010")$daynumber

levelsplot.carpenter <- 
  xyplot(allData$carpenterelev~allData$daynumber,groups=as.factor(allData$year),type="l",
         xlab=list(label=NULL),
         ylab=list(label="Reservoir Elevation (m)"),
         main="Carpenter Reservoir Elevation",
         col=1:7,lwd=2,
         scales=list(tck=c(1,0),alternating=1,x=list(at=at,labels=labels,axs="i")),
         key=list(corner=c(1,0.05),
                  lines=list(col=c(1:7,"black","red","grey","darkblue","blue"),lwd=2,lty=c(1,1,1,1,1,1,1,2,2,2,2,2)),
                  text=list(labels=c(levels(as.factor(allData$year)),"2014 RB Migration","2015 VR2W Installation","2015 RB Migration","2014 KO Migration","2015 KO Migration"))),
         panel=function(...) {
           #panel.rect(xleft=c(vertical[1]), xright=c(vertical[4]),ybottom=0, ytop=700,col=c("lightgrey"),border=NA)
           panel.abline(v=vertical,lty=2,lwd=2,col=c("black","red","grey","darkblue","blue"))
           panel.xyplot(...)
         })

windows()
#quartz()
print(levelsplot.carpenter)

png("CarpReservoirElevations.png",width=11,height=8.5,units="in",res=77)
levelsplot.carpenter
dev.off()

##########
# DOWNTON

levelsplot.downton <- 
  xyplot(allData$downtonelev~allData$daynumber,groups=as.factor(allData$year),type="l",
         xlab=list(label=NULL),
         ylab=list(label="Reservoir Elevation (m)"),
         main="Downton Reservoir Elevation",
         col=1:7,lwd=2,
         scales=list(tck=c(1,0),alternating=1,x=list(at=at,labels=labels,axs="i")),
         key=list(corner=c(1,0.05),lines=list(col=c(1:7),lwd=2,lty=c(1,1,1,1,1,1,1)),
             text=list(labels=c(levels(as.factor(allData$year)))))
)

windows()
#quartz()
print(levelsplot.downton)

png("DowntonReservoirElevations.png",width=11,height=8.5,units="in",res=77)
levelsplot.downton
dev.off()

##########
# TERZAGHI FLOW

tzflowplot <- 
  xyplot(subset(allData,terzaghiflow!="NA")$terzaghiflow~subset(allData,terzaghiflow!="NA")$daynumber,
         groups=as.factor(subset(allData,terzaghiflow!="NA")$year),type="l",
         xlab=list(label=NULL),col=1:2,lwd=2,
         ylab=list(label="Terzaghi Flow (m3/s)"),
         main="Terzaghi Flow",
         scales=list(tck=c(1,0),alternating=1,x=list(at=at,labels=labels,axs="i")),
         key=list(corner=c(1,0.95),lines=list(col=c(1:2),lwd=2),
                  text=list(labels=c(levels(factor(subset(allData,terzaghiflow!="NA")$year)))))
  )

windows()
#quartz()
print(tzflowplot)

png("TerzaghiFlow.png",width=11,height=8.5,units="in",res=77)
tzflowplot
dev.off()

############
# Middle Bridge River 

mbrstageplot <- 
  xyplot(subset(allData,mbrstage!="NA")$mbrstage~subset(allData,mbrstage!="NA")$daynumber,
         groups=as.factor(subset(allData,mbrstage!="NA")$year),type="l",
         xlab=list(label=NULL),col=1:2,lwd=2,
         ylab=list(label="MBR Stage (m)"),
         ylim=c(0,1),xlim=c(0,365),
         main="Middle Bridge River Stage Heights (Daily Avg)",
         scales=list(tck=c(1,0),alternating=1,x=list(at=at,labels=labels,axs="i")),
         key=list(corner=c(1,0.95),lines=list(col=c(1:2),lwd=2),
                  text=list(labels=c(levels(factor(subset(allData,mbrstage!="NA")$year)))))
  )

windows()
#quartz()
print(mbrstageplot)

png("MBRStages.png",width=11,height=8.5,units="in",res=77)
mbrstageplot
dev.off()
