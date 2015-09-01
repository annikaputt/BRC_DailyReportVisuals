############################
# BRC_PanelPlot.R
# BRC_DailyReportVisuals.Rproj

# Generates very basic plots of each variable based on 2015 data only

# Created September 1, 2015
# A Putt
#############################

library(ggplot2)
library(plyr) # For re-levelling factors

source("BRC_DailyUpload.R") #allData contains the uploaded data

# Create a 2015 data frame
brc2015 <- subset(allData,year=="2015")[,c(2:5,7,9,14)] # Subset only 2015 and only the columns I want in long format

toplot <- reshape(brc2015, direction="long",varying=names(brc2015)[1:6],v.names="value",timevar="measure",
                  times=names(brc2015)[1:6]) # Reshape to long for plotting
toplot$measure <- as.factor(toplot$measure) # Turn into a factor for re-labelling

# Change the factor levels to have more descriptive facet labels
toplot$measure <- mapvalues(toplot$measure, from = levels(toplot$measure), to = c("Carpenter Elevation (m)", "Cayoosh Flow (m3/s)", "Downton Elevation (m)","Seton Dam Elevation (m)"
                                                     ,"Seton River Flow (m3/s)","Terzaghi Dam Flow (m3/s")) 
# Create a very basic plot
basicplot <- ggplot(toplot,aes(fulldate,value)) +
  geom_line() +
  facet_wrap(~measure,ncol=3,scales="free") +
  labs(y="",x="Date (2015)")

png("2015BridgeMeasuresGraphic.png",width=16,height=8.5,units="in",res=77)
basicplot
dev.off()
