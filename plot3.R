plot3 <- function()
{
  #This function loads data and produces Plot 3 for assignment 1 of Exploratory Data Analyses.
  
  #Reading data into data.frame.
  data <- read.table("household_power_consumption.txt", sep=";", header=TRUE)
  
  #Convert factor column into correct date format.
  data$Date <- as.Date(data$Date, "%d/%m/%Y")
  
  #Select only the relevant days for plotting.
  filter <- data[data$Date >= "2007-02-01" & data$Date <= "2007-02-02", ]
  
  #Convert factor columns to numeric columns that is going to be used in the plot.
  filter <-  transform(filter, Sub_metering_1 = as.numeric(as.character(Sub_metering_1)),
                       Sub_metering_2 = as.numeric(as.character(Sub_metering_2)))
  
  #Combine the Date and Time columns to produce appropriate time series column.
  filter$DateTime <- strptime(paste(filter$Date, filter$Time), "%Y-%m-%d %H:%M:%S")
  
  #Specifying the png plotting device first like this produce better quality png plots.
  png( file="plot3.png", width=480, heigh=480, units="px")
  
  #Create the Energy sub metering plot and add three line series to the plot.
  plot(filter$DateTime, filter$Sub_metering_1, xlab= "", ylab = "Energy sub metering", type="n")
  
  lines(filter$DateTime, filter$Sub_metering_1)
  
  lines(filter$DateTime, filter$Sub_metering_2, col = "red")
  
  lines(filter$DateTime, filter$Sub_metering_3, col = "blue") 
  
  #Add a legend to the plot.
  legend("topright", c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"), 
         lty=c(1,1,1), col=c("black","red","blue"), cex=1, pt.cex=1.5)
  
  #Shut down the png device.
  dev.off()
}