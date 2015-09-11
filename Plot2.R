plot2 <- function() {
	household_power_consumption <- read.table("./household_power_consumption.txt", sep=";", na.strings="?",header=TRUE)

	household_power_consumption$Date <- as.Date(household_power_consumption$Date, format = "%d/%m/%Y")
  
	dates <- subset(household_power_consumption, Date >= as.Date("2007-02-01") & Date <= as.Date("2007-02-02"),select="Date")
  
	my_data <- subset(household_power_consumption, Date >= as.Date("2007-02-01") & Date <= as.Date("2007-02-02"), select=c("Global_active_power","Time"))
	
	#Combine date and times into one field
	my_data$Time <- strptime(paste(dates$Date,my_data$Time),format = "%Y-%m-%d %H:%M:%S")
	
	par(mfrow = c(1,1))

	#open png graphics device
	png("./Plot2.png", height = 480, width = 480)

	#Plot time series with invisible points
	mplot <- plot(my_data$Time,my_data$Global_active_power,pch = NA_integer_,ylab="Global Active Power (kilowatts)", xlab="",ylim=c(0,8),yaxt="n")
	
	#add lines connecting all data points
	lines(my_data$Time,my_data$Global_active_power)
	
	#add proper y-axis
	axis(2,at= c(0,2,4,6),labels=c(0,2,4,6))
	
	#print plot to png file
	print(mplot)
	dev.off()
}
