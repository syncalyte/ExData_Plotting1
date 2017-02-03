plot4 <- function () {
        #Reads the data into a variable called "data"
        data <- read.table("household_power_consumption.txt", sep = ";", na.strings = "?", header = TRUE)
        
        #Changes the Dates into date format
        data$Date <- as.Date(data$Date, format = "%d/%m/%Y")
        
        #Sets the dates that we want to look at
        Thursday <- as.Date("2007-02-01")
        Friday <- as.Date("2007-02-02")
        
        #Subsets the data so that only those days we're interested in are included
        relevant <- data[((data$Date == Thursday) | (data$Date == Friday)),]
        
        #Creates a new column with the combined information from Date and Time columns
        relevant <- transform(relevant, newcol = paste(Date, Time, sep = ":"))
        
        #Changes the newly created column into Year-Month-Day-Hours-Minutes-Seconds format
        relevant$newcol <- strptime(relevant$newcol, format = "%Y-%m-%d:%H:%M:%S")
        
        #Makes a png file and plots date against energy sub metering 1, 2 and 3
        png (file = "plot4.png")
        
        #Makes a 2x2 grid to fill in with plots
        par(mfrow = c(2,2))
        
        #First plot of date against global active power
        plot(relevant$newcol, relevant$Global_active_power, xlab = "", ylab = "Global Active Power", type = "n")
        lines(relevant$newcol, relevant$Global_active_power)
        
        #Second plot of date vs voltage
        plot(relevant$newcol, relevant$Voltage, xlab = "datetime", ylab = "Voltage", type = "n")
        lines(relevant$newcol, relevant$Voltage)
        
        #Third plot of date vs energy sub metering
        plot(relevant$newcol, relevant$Sub_metering_1, xlab = "", ylab = "Energy sub metering", type = "n")
        lines(relevant$newcol, relevant$Sub_metering_1, col = "black")
        lines(relevant$newcol, relevant$Sub_metering_2, col = "red")
        lines(relevant$newcol, relevant$Sub_metering_3, col = "blue")
        legend("topright", lty = c(1,1,1), col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), bty = "n")
        
        
        #Fourth plot of date vs global reactive power
        plot(relevant$newcol, relevant$Global_reactive_power, xlab = "datetime", ylab = "Global_reactive_power", type = "n")
        lines(relevant$newcol, relevant$Global_reactive_power)
        dev.off()
        
}