plot1 <- function()
{
  #This function loads data and produces Plot 1 for assignment 1 of Exploratory Data Analyses.
  
  #Reading data into data.frame.
  df <- read.table("household_power_consumption.txt", sep=";", header=TRUE)
  
  #Convert factor column into correct date format.
  df$Date <- as.Date(df$Date, "%d/%m/%Y")
  
  #Select only the relevant days for plotting.
  filter <- df[df$Date >= "2007-02-01" & df$Date <= "2007-02-02", ]
  
  #Convert factor column to numeric column for plotting.
  filter <-  transform(filter, Global_active_power=as.numeric(as.character(Global_active_power)))
    
  #Specifying the png plotting device first like this produce better quality png plots
  png( file="plot1.png", width=480, heigh=480, units="px")
  
  #Create the histogram plot
  hist(filter$Global_active_power, xlab = "Global Active Power (kilowatts)", main ="Global Active Power", col = "red")
  
  #Shut down the png device.
  dev.off()
}
