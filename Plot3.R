plot3 <- function() {
  household_power_consumption <- read.table("./household_power_consumption.txt",
                                            sep=";", na.strings="?",header=TRUE)
  
  household_power_consumption$Date <- as.Date(household_power_consumption$Date,
                                              format = "%d/%m/%Y")
  
  dates <- subset(household_power_consumption, Date >= as.Date("2007-02-01") & 
                    Date <= as.Date("2007-02-02"),select="Date")
  
  my_data <- subset(household_power_consumption, Date >= as.Date("2007-02-01") & 
                      Date <= as.Date("2007-02-02"),
                    select=c("Sub_metering_1","Sub_metering_2","Sub_metering_3","Time"))
  
  my_data$Time <- strptime(paste(dates$Date,my_data$Time),format = "%Y-%m-%d %H:%M:%S")
 
  png("./Plot3.png",height = 480,width = 480)
  
  par(mfrow=c(1,1))
  
  mplot <- plot(my_data$Time,my_data$Sub_metering_1,pch = NA_integer_,ylab="Energy sub metering", xlab="",
                ylim=c(0,40),yaxt="n")
  lines(my_data$Time,my_data$Sub_metering_1,col = "black")
  points(my_data$Time,my_data$Sub_metering_2,pch = NA_integer_)
  lines(my_data$Time,my_data$Sub_metering_2,col = "red")
  points(my_data$Time,my_data$Sub_metering_3,pch = NA_integer_)
  lines(my_data$Time,my_data$Sub_metering_3,col = "blue")
  
  legend("topright",c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lty=c("solid"),
         pch = c(NA,NA,NA), col = c("black","red","blue"),lwd=c(2), cex = .7)
  axis(2,at= seq(0,30,10),labels=seq(0,30,10))
  
  
  print(mplot)
  dev.off()
  
}
