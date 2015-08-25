############################
# BRC_DataUpload.R
# BRC_DailyReportVisuals.Rproj

# Uploads historic data obtained by Dorian Turner, then uploads any files 
# that contain the text "BRC Daiy Report Mail"

# Created August 21, 2015
# A Putt
#############################

# Upload historic data provided by Dorian Turner (this is only for Downton and Carpenter)
cr <- read.csv("Carp_2010_Nov52014.csv",head=TRUE)
dr <- read.csv("Down_2010_Nov52014.csv",head=TRUE)

# Create a function to average daily levels
DailyLevels <- function(data) {
  data$Date <- as.Date(data$Date,format="%Y-%m-%d")
  daily <- data.frame(do.call("rbind",as.list(by(data[,"Value"],data$Date,mean))))
  daily$Date <- row.names(daily)
  colnames(daily) <- c("Levels","Date")
  row.names(daily) <- 1:nrow(daily)
  daily$Date <- as.Date(daily$Date,"%Y-%m-%d")
  return(daily)
}

crdaily <- DailyLevels(cr)
drdaily <- DailyLevels(dr)

##########
# Upload all other BRC files. These are downloaded from the email and saved as .csv files in 
# the working directory.
file_list  <- list.files(path="DailyReportMails", pattern="*.csv") # Pull all files
names <- paste(substr(file_list,1,3),substr(file_list,23,26),sep="") # Pull the file names

for(i in 1:length(file_list)){
  assign(names[i], read.csv(paste("DailyReportMails/",file_list[i],sep=""),skip=9)) # Read in the data
}
df_list = lapply(ls(pattern = "BRC"), get) # Turn into a list
names(df_list) <- ls(pattern = "BRC")

# Manipulate each new data frame to add the month and remove unwanted lines at the end
newList <- list()
for(i in 1:length(df_list) ){
  temp <- df_list[[i]]
  removeUnwanted <- temp[1:31,1:11]
  colnames(removeUnwanted) <- c("day","downtonelev","carpenterelev","terzaghiflow","setondamelev","setonsyphons","setonriverflow","setonrivercfs",
                                "cayooshflow","dilutionpercent","fraserhopeflow")
  removeUnwanted$year <- paste("20",substr(names(df_list[i]),4,5), sep="")
  removeUnwanted$month <- substr(names(df_list[i]),6,7)
  removeNA <- removeUnwanted[complete.cases(removeUnwanted[4]),] # remove any data where the second column has an na (gets rid of day 31 for months with 30 days)
  removeNA$downtonelev <- as.numeric(as.character(removeNA$downtonelev)) # Because there were some phone numbers in the file these were factors. Need to convert them to characters and then numbers
  removeNA$carpenterelev <- as.numeric(as.character(removeNA$carpenterelev))
  newList[[i]] <- removeNA
}

# Combine the list into one large data frame
DailyMail <- do.call("rbind",newList)

# Add the historic data to the DailyMail data
Historic <- merge(crdaily,drdaily,by="Date",all=TRUE)
names(Historic) <- c("Date","carpenterelev","downtonelev")
Historic$year <- as.character(substr(Historic$Date,1,4))
Historic$month <- as.character(substr(Historic$Date,6,7))
Historic$day <- as.factor(substr(Historic$Date,9,10))
Historic2 <- data.frame(day=Historic$day,downtonelev=Historic$downtonelev,carpenterelev=Historic$carpenterelev,setondamelev=NA,
                        terzaghiflow=NA,setonsyphons=NA,setonriverflow=NA,setonrivercfs=NA,cayooshflow=NA,dilutionpercent=NA,fraserhopeflow=NA,
                        year=Historic$year,month=Historic$month)

# Combine the historic data with the DailyMail data
allData <- rbind(DailyMail,Historic2)
allData$fulldate <- paste( allData$year, allData$month, allData$day, sep = "/")
allData$fulldate <- as.POSIXct(allData$fulldate)

# Add some more date columns to the main data frame
allData$shortdate <- format(allData$fulldate,"%b-%d")
allData$fulldate <- as.POSIXlt(allData$fulldate)
allData$daynumber <- allData$fulldate$yday # January 1 is 0
