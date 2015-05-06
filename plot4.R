plot4 <- function()
{
  #This function loads data and produces Plot 4 for assignment 1 of Exploratory Data Analyses.
  
  #Reading data into data.frame.
  data <- read.table("household_power_consumption.txt", sep=";", header=TRUE)
  
  #Convert factor column into correct date format.
  data$Date <- as.Date(data$Date, "%d/%m/%Y")
  
  #Select only the relevant days for plotting.
  filter <- data[data$Date >= "2007-02-01" & data$Date <= "2007-02-02", ]
  
  #Convert factor columns to numeric columns that is going to be used in the plot.
  filter <-  transform(filter, Sub_metering_1 = as.numeric(as.character(Sub_metering_1)),
                       Sub_metering_2 = as.numeric(as.character(Sub_metering_2)),
                       Voltage = as.numeric(as.character(Voltage)),
                       Global_reactive_power = as.numeric(as.character(Global_reactive_power)))
  
  #Combine the Date and Time columns to produce appropriate time series column.
  filter$DateTime <- strptime(paste(filter$Date, filter$Time), "%Y-%m-%d %H:%M:%S")
  
  #Specifying the png plotting device first like this produce better quality png plots.
  png( file="plot4.png", width=480, heigh=480, units="px")
  
  #Configure a plot with 2 rows and 2 columns.
  par(mfrow=c(2,2))
  
  #Adding Global Active Power plot.
  plot(filter$DateTime, filter$Global_active_power, xlab ="" , ylab="Global Active Power", type = "n")
  
  lines(filter$DateTime, filter$Global_active_power)
  
  #Add Voltage plot.
  plot(filter$DateTime, filter$Voltage, xlab="datetime", ylab="Voltage", type="n")
  
  lines(filter$DateTime, filter$Voltage)
  
  #Add Energy Sub Metering plot.  
  plot(filter$DateTime, filter$Sub_metering_1, xlab="", ylab="Energy sub metering", type="n")
  
  lines(filter$DateTime, filter$Sub_metering_1)
  
  lines(filter$DateTime, filter$Sub_metering_2, col = "red")
  
  lines(filter$DateTime, filter$Sub_metering_3, col = "blue") 
  
  legend("topright", c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"), 
         lty=c(1,1,1), col=c("black","red","blue"), cex=1, pt.cex=1.5, bty="n")
  
  #Add Global reactive power plot.
  plot(filter$DateTime, filter$Global_reactive_power, xlab="datetime", ylab="Global_reactive_power", type="n")
  
  lines(filter$DateTime, filter$Global_reactive_power)
  
  #Shut down the png device.
  dev.off()
}