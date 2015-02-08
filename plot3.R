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


## Plot 3 is a combination of 3 line plots on the same axes. The plot shows
## each of the Sub Metering measurements over time. The plot includes a 
## legend and colored lines.
##
## Debug mode uses the default graphic device.
plot3<-function(debug.mode=FALSE){
        plotData<-getPlotData()
        
        if(!debug.mode)
                png("plot3.png", 480, 480)
        
        # Set up the axes, then add the line plots and legend.
        with( plotData,
                {
                        plot( Sub_metering_1 ~ Time,
                              type="n",
                              bg="transparent",
                              ylab="Energy sub metering",
                              xlab=""
                        )
                        lines( Sub_metering_1 ~ Time,
                              col="black"
                        )
                        lines( Sub_metering_2 ~ Time,
                              col="red"
                        )
                        lines( Sub_metering_3 ~ Time,
                              col="blue"
                        )
                        
                        legend(
                                x="topright",
                                legend=
                                        c(
                                               "Sub_metering_1",
                                               "Sub_metering_2",
                                               "Sub_metering_3"
                                          ),
                                col=c("black","red","blue"),
                                lty=1
                        )
                }

        )
        
        if(!debug.mode)
                dev.off(dev.cur())
}