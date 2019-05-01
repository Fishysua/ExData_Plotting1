#Load Libraries
library(data.table)

filename <- "Household Power Consumption.zip"

#Download Files
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip "
  download.file(fileURL, filename, method="curl")
}  

#Unzip Files
if (!file.exists("household_power_consumption.txt")) { 
  unzip(filename) 
}

#Filter data on date
if (!file.exists("power_consumption_filtered.txt")){
  fulldata <- fread("household_power_consumption.txt", sep = ";", header = TRUE, na.strings = "?")
  
  #Limit data
  DataDate <- as.Date(fulldata$Date, "%d/%m/%Y")
  DateFilter <- year(DataDate) == 2007 & month(DataDate) == 2 & (mday(DataDate) == 1 | mday(DataDate) == 2)
  FilteredData <- fulldata[DateFilter]
  fwrite(FilteredData, "power_consumption_filtered.txt")
  rm(fulldata)
  rm(DataDate)
  rm(DateFilter)
  rm(FilteredData)
}

#Load data for plotting and create Date Time
PlottingData <- fread("power_consumption_filtered.txt")
DateTime <- with(PlottingData, as.POSIXct(paste(Date, Time), format = "%d/%m/%Y%H:%M:%S"))
names(DateTime) = "Date Time"
PlottingData <- cbind(DateTime, PlottingData)

#Plot 1
png(filename = "Plot1.png")
hist(PlottingData$Global_active_power, xlab = "Global Active Power (kilowatts)", main = "Global Active Power", col = "red", bg = "white")
dev.off()

#Plot 2
png(filename = "Plot2.png")
plot(PlottingData$DateTime, PlottingData$Global_active_power, xlab = "", ylab = "Global Active Power (kilowatts)", type = "l", bg = "white")
dev.off()

#Plot 3
png(filename = "Plot3.png")
plot(PlottingData$DateTime, PlottingData$Sub_metering_1, type = "l", ylab = "Energy Sub Metering")
points(PlottingData$DateTime, PlottingData$Sub_metering_2, type = "l", col = "red")
points(PlottingData$DateTime, PlottingData$Sub_metering_3, type = "l", col = "blue")
legend("topright", legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), lty = c(1,1,1), col = c("black", "red", "blue"))
dev.off()

#Plot 4
png(filename = "Plot4.png")
par(mfrow = c(2,2))
plot(PlottingData$DateTime, PlottingData$Global_active_power, xlab = "", ylab = "Global Active Power (kilowatts)", type = "l", bg = "white")
plot(PlottingData$DateTime, PlottingData$Voltage, xlab = "datetime", ylab = "Voltage", type = "l", bg = "white")
plot(PlottingData$DateTime, PlottingData$Sub_metering_1, type = "l", ylab = "Energy Sub Metering")
points(PlottingData$DateTime, PlottingData$Sub_metering_2, type = "l", col = "red")
points(PlottingData$DateTime, PlottingData$Sub_metering_3, type = "l", col = "blue")
legend("topright", legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), lty = c(1,1,1), col = c("black", "red", "blue"))
plot(PlottingData$DateTime, PlottingData$Global_reactive_power, xlab = "datetime", ylab = "Global_reactive_power", type = "l", bg = "white")
dev.off()