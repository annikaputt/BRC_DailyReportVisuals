############################
# BRC_PanelPlot.R
# BRC_DailyReportVisuals.Rproj

# Generates very basic plots of each variable based on 2015 data only

# Created September 1, 2015
# A Putt
#############################

library(ggplot2)
library(plyr) # For re-levelling factors
library(scales)

source("BRC_DailyUpload.R") #allData contains the uploaded data

# Create a subset data frame of 2015
brc2015 <- subset(allData,year==2015)[,c("fulldate","carpenterelev", "cayooshflow", "downtonelev","setondamelev"
                                           ,"setonriverflow","terzaghiflow")] # Subset only 2015 and only the columns I want in long format
# Create a subset data frame of 2016
brc2016 <- subset(allData,year==2016)[,c("fulldate","carpenterelev", "cayooshflow", "downtonelev","setondamelev"
                                           ,"setonriverflow","terzaghiflow")] # Subset only 2015 and only the columns I want in long format


YearPlotFunc <- function(brcSubset,year,xlimits) {
    
  toplot <- reshape(brcSubset, direction="long",varying=names(brcSubset)[2:7],v.names="value",timevar="measure",
                    times=names(brcSubset)[1:6]) # Reshape to long for plotting
  toplot$measure <- as.factor(toplot$measure) # Turn into a factor for re-labelling
  
  # Change the factor levels to have more descriptive facet labels
  toplot$measure <- mapvalues(toplot$measure, from = levels(toplot$measure), to = c("Carpenter Elevation (m)", "Cayoosh Flow (m3/s)", "Downton Elevation (m)","Seton Dam Elevation (m)"
                                                       ,"Seton River Flow (m3/s)","Terzaghi Dam Flow (m3/s")) 
  # Create a very basic plot
  basicplot <- ggplot(toplot,aes(as.Date(fulldate),value)) +
    geom_line() +
    facet_wrap(~measure,ncol=3,scales="free") +
    labs(y="",x="") + 
    scale_x_date(date_breaks = "2 month", 
                 labels=date_format("%b"),
                 limits = xlimits)  
  png(sprintf("%sBridgeMeasuresGraphic.png",year),width=16,height=8.5,units="in",res=77)
  print(basicplot)
  dev.off()
}

YearPlotFunc(brc2015,"2015",as.Date(c('2015-01-01','2015-12-31')))
YearPlotFunc(brc2016,"2016",as.Date(c('2016-01-01','2016-12-31')))

