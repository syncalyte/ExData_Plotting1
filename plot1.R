plot1 <- function () {
        #Reads the data into a variable called "data"
        data <- read.table("household_power_consumption.txt", sep = ";", na.strings = "?", header = TRUE)
        
        #Changes the Dates into date format
        data$Date <- as.Date(data$Date, format = "%d/%m/%Y")
        
        #Sets the dates that we want to look at
        Thursday <- as.Date("2007-02-01")
        Friday <- as.Date("2007-02-02")
        
        #Subsets the data so that only those days we're interested in are included
        relevant <- data[((data$Date == Thursday) | (data$Date == Friday)),]
        
        #Makes a png file and plots Global Active Power against Frequency
        png (file = "plot1.png")
        hist(relevant$Global_active_power, col = "red", main = "Global Active Power", xlab = "Global Active Power(kilowatts)")
        dev.off()
        
}