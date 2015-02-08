## Dedicated function to read and optionally download the 
## data needed for the 4 plots associated with Course Project 1
## for Exploratory Data Analysis.
##
## This function is repeated in each plot file.
getPlotData<-function(){
        ## Needed for fread function
        suppressWarnings(require(data.table))
        
        
        ## Ensure data directory exists
        datadir="./data"
        if(!file.exists(datadir))
                dir.create(datadir)
        
        ## Set up names and download/unzip file if necessary.
        filename<-"household_power_consumption.txt"
        fullname<-paste(datadir, filename, sep="/")
        url<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
        if(!file.exists( fullname )){
                temp<-tempfile()
                print(temp)
                download.file(url, temp, mode="wb")
                unzip(zipfile=temp, files=filename, exdir=datadir)
        }
        
        ## Read and clean selected records.
        plotData<-fread(fullname,colClasses="character",data.table=FALSE, na.strings="?")
        plotData<-subset(plotData, Date %in% c("1/2/2007", "2/2/2007"))
        plotData[,3:9]<-lapply(plotData[,3:9],as.numeric)
        plotData$Time<-as.POSIXct(strptime(paste(plotData$Date, plotData$Time), "%d/%m/%Y %H:%M:%S"))
        plotData$Date<-as.Date(strptime(plotData$Date,"%d/%m/%Y"))
        
        ## Return the prepared data.frame.
        plotData
}



## Plot 1 is a historgram of Global Actve Power with red bars and some labels.
##
## Debug mode uses the default graphic device.
plot1<-function(debug.mode=FALSE){
        plotData<-getPlotData()
   
        if(!debug.mode)
                png("plot1.png", 480, 480)
   
        hist(
                plotData$Global_active_power,
                bg="transparent",
                col="red",
                xlab="Global Active Power (kilowatts)",
                main="Global Active Power"
        )
        
        if(!debug.mode)
                dev.off(dev.cur())
}        