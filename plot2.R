plot2 <- function()
{
  #This function loads data and produces Plot 2 for assignment 1 of Exploratory Data Analyses.
  
  #Reading data into data.frame.
  data <- read.table("household_power_consumption.txt", sep=";", header=TRUE)
  
  #Convert factor column into correct date format.
  data$Date <- as.Date(data$Date, "%d/%m/%Y")
  
  #Select only the relevant days for plotting.
  filter <- data[data$Date >= "2007-02-01" & data$Date <= "2007-02-02", ]
  
  #Convert factor column to numeric column for plotting.
  filter <-  transform(filter, Global_active_power=as.numeric(as.character(Global_active_power)))
  
  #Combine the Date and Time columns to produce appropriate time series column.
  filter$DateTime <- strptime(paste(filter$Date, filter$Time), "%Y-%m-%d %H:%M:%S")
 
  #Specifying the png plotting device first like this produce better quality png plots.
  png( file="plot2.png", width=480, heigh=480, units="px")
  
  #Create plot and add line series to it.
  plot(filter$DateTime, filter$Global_active_power, xlab = "" , ylab = "Global Active Power (kilowatts)", type = "n")
  
  lines(filter$DateTime, filter$Global_active_power)
  
  #Shut down the png device.
  dev.off()
}