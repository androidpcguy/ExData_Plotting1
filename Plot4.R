#USAGE: only call plot4() from R console to create all four plots directly

#Helper function
makeplot2 <- function(mdata) {
	x <- plot(mdata$Time,mdata$Global_active_power,pch = NA_integer_,ylab="Global Active Power", xlab="",ylim=c(0,8),yaxt="n")
  
	lines(mdata$Time,mdata$Global_active_power)
  
	axis(2,at= c(0,2,4,6),labels=c(0,2,4,6))

	return(x)
}

#Helper function
makeplot3 <- function(mdata) {
	#Submetering 1 points
	x <- plot(mdata$Time,mdata$Sub_metering_1,pch = NA_integer_,ylab="Energy sub metering", xlab="",ylim=c(0,40),yaxt="n", cex=.7)
	lines(mdata$Time,mdata$Sub_metering_1,col = "black")
	
	#submetering 2 points
	points(mdata$Time,mdata$Sub_metering_2,pch = NA_integer_)
	lines(mdata$Time,mdata$Sub_metering_2,col = "red")
  
	#submetering 3 points
	points(mdata$Time,mdata$Sub_metering_3,pch = NA_integer_)
	lines(mdata$Time,mdata$Sub_metering_3,col = "blue")
  
	axis(2,at= seq(0,30,10),labels=seq(0,30,10))
  
	#legend
	legend("topright", c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), lty = c("solid"),
	       pch = c(NA), col = c("black","red","blue"), lwd = c(2), cex = .7)
	
	return(x)
}

#Helper function
makevoltageplot <- function(mdata) {

	x <- plot(mdata$Time,mdata$Voltage,pch = NA_integer_,ylab = "Voltage",xlab = "datetime",
		  ylim = c(232,247),yaxt = "n")
	
	lines(mdata$Time, mdata$Voltage)
  
	axis(side = 2, at = seq(234,246,2),labels=c(TRUE,FALSE))
  
	return(x)
}

#Helper function
makereacpowerplot <- function(mdata) {
	x <- plot(mdata$Time,mdata$Global_reactive_power,pch = NA_integer_,ylab = "Global_reactive_power",xlab = "datetime",
		  ylim = c(0,0.5),yaxt = "n")
	
	lines(mdata$Time, mdata$Global_reactive_power)
  
	axis(side = 2, at = seq(0,0.5,0.1),labels=TRUE)
	return(x)
}

#MAIN FUNCTION
plot4 <- function() {
	#Set up data frames for inputting points onto plots
  
	household_power_consumption <- read.table("./household_power_consumption.txt",
                                                  sep=";", na.strings="?",header=TRUE)
  
	household_power_consumption$Date <- as.Date(household_power_consumption$Date,
                                                    format = "%d/%m/%Y")
  
	dates <- subset(household_power_consumption, Date >= as.Date("2007-02-01") & 
                    Date <= as.Date("2007-02-02"),select="Date")
  
	my_data <- subset(household_power_consumption, Date >= as.Date("2007-02-01") & Date <= as.Date("2007-02-02"),
			  select=c("Sub_metering_1","Sub_metering_2","Sub_metering_3", "Voltage","Global_reactive_power",
				  "Global_active_power","Time"))

	my_data$Time <- strptime(paste(dates$Date,my_data$Time),format = "%Y-%m-%d %H:%M:%S")
  
	png("./Plot4.png",height = 480,width = 480)
	par(mfrow = c(2,2))
  
	print(makeplot2(my_data))
	print(makevoltageplot(my_data))
	print(makeplot3(my_data))
	print(makereacpowerplot(my_data))
	dev.off()
}
