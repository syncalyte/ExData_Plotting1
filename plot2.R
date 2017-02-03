plot2 <- function () {
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
        
        #Makes a png file and plots date and global active power
        png (file = "plot2.png")
        plot(relevant$newcol, relevant$Global_active_power, xlab = "", ylab = "Global Active Power (kilowatts)", type = "n")
        lines(relevant$newcol, relevant$Global_active_power)
        dev.off()
        
}