plot1 <- function() {
	#Read data
	household_power_consumption <- read.csv("./household_power_consumption.txt",
                                          sep=";", na.strings="?")
	#Convert to Date object
	household_power_consumption$Date <- as.Date(household_power_consumption$Date,  format = "%d/%m/%Y")
  
	#subset only required dates
	mydata <- subset(household_power_consumption, Date >= as.Date("2007-02-01") & 
                   Date <= as.Date("2007-02-02"),select="Global_active_power")
	
	par(mfrow = c(1,1))

	png("./Plot1.png", height = 480, width = 480)

	#create histogram
	mhist <- hist(mydata$Global_active_power,col = "red", xlab="Global Active Power (kilowatts)",
		main = "Global Active Power", ylim=c(0,1200),xaxt="n")
	
	#create correct x axis
	axis(1,at = c(0,2,4,6,8),tick = FALSE,labels=FALSE)
	axis(1,at= c(0,2,4,6),labels=c(0,2,4,6))
	
	print(mhist)
	dev.off()
}
