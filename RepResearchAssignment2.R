# Set the working directory
pwd <- getwd() 
if (!is.null(pwd)) { 
  setwd(pwd) 
} else { 
  print("Directory not found") 
}

# Dowloading data if it's not already done
if(!file.exists("stormData.csv.bz2")) { #use any file name
        download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
        destfile = "stormData.csv.bz2") # give destination
}


# Loading data tests
system.time(data <- data.table(read.csv(bzfile("stormData.csv.bz2"), sep = ",", header = T)))

system.time(data2 <- read.table("stormData.csv.bz2", header = T, sep = ","))

system.time(data3 <- as.data.table(read.csv(bzfile("stormData.csv.bz2"), sep = ",", header = T, stringsAsFactors = F)))


# Change parameter names to lowercase.
colnames(data) <- tolower(colnames(data))

    # Subset on the parameters of interest.
    data <- subset(x=data, 
                   subset=(evtype != "?" & 
                               (injuries > 0 | fatalities > 0 | propdmg > 0 | cropdmg > 0)),
                   select=c("evtype", 
                            "fatalities", 
                            "injuries", 
                            "propdmg", 
                            "propdmgexp", 
                            "cropdmg", 
                            "cropdmgexp"))  


# Map property damage alphanumeric exponents to numeric values.
      
propDmg <-  c("\"\"" = 10^0,
                "-" = 10^0, 
                "+" = 10^0,
                "H" = 10^2,
                "K" = 10^3,
                "M" = 10^6,
                "B" = 10^9)

data$propdmgexp <- propDmgKey[as.character(data$propdmgexp)]
data$propdmgexp[is.na(data$propdmgexp)] <- 10^0

# Map crop damage alphanumeric exponents to numeric values
cropDmg <-  c("\"\"" = 10^0,
                 "K" = 10^3,
                 "M" = 10^6,
                 "B" = 10^9)

data$cropdmgexp <- cropDmgKey[as.character(data$cropdmgexp)]
data$cropdmgexp[is.na(data$cropdmgexp)] <- 10^0


### HEALTH DATA
# Aggregating number of fatalities and injuries per evtype into healthData dataframe
healthData <- aggregate(cbind(fatalities, injuries) ~ evtype, data=data, FUN=sum)

# Adding fatalities and injuries
healthData$tot <- healthData$fatalities + healthData$injuries

# Removing rows with zero values
healthData <- healthData[healthData$tot > 0, ]

# Sorting data in descending order
healthData <- healthData[order(healthData$tot, decreasing=TRUE), ]

# Filtering the dataframe to show the top 10 impact event types
healthDataTop <- healthData[1:10, ]

# Removing the column for totals
healthDataTop$tot <- NULL

# Changing the data set format from wide to long for graphing
healthDataTopMelt <- melt(healthDataTop, id.vars="evtype")


### ECONOMIC DATA

# Combining propdmg and propdmgexp parameters into a single parameter "propertyloss"
data$propertyloss <- data$propdmg * data$propdmgexp

# Combining cropdmg and cropdmgexp parameters into a single parameter "croploss"
data$croploss <- data$cropdmg * data$cropdmgexp

# Aggregating amount of proploss and croploss per evtype into economicData dataframe
econData <- aggregate(cbind(propertyloss, croploss) ~ evtype, data=data, FUN=sum)

# Adding total loss column to economicData
econData$tot <- econData$propertyloss + econData$croploss

# Removing rows with zero values
econData <- econData[econData$tot > 0, ]

# Sorting the economy data in descending order
econData <- econData[order(econData$tot, decreasing=TRUE), ]

# Creating a dataframe of highest economy impacting event types
econDataTop <- econData[1:10, ]

# Removing the column for totals
econDataTop$tot <- NULL

# Changing the data set format from wide to long for graphing
econDataTopMelt <- melt(econDataTop, id.vars="evtype")

economicDataTopMelt <- melt(economicDataTop, id.vars="evtype")



### Results
# Creating the chart for health injuries
healthChart <- ggplot(healthDataTopMelt, aes(x = reorder(evtype, -value), y = value)) +
                        geom_bar(stat = "identity", aes(fill = variable)) +
                        scale_y_sqrt("Event Frequency") +
                        xlab("Event Type") +
                        theme(axis.text.x = element_text(angle=45, hjust = 1)) +
                        ggtitle("Health Impact of Top 10 US Weather Events")

# Display the chart
print(healthChart)

# Creating the chart for economic damages
econChart <- ggplot(econDataTopMelt, aes(x = reorder(evtype, -value), y = value)) + 
        geom_bar(stat = "identity", aes(fill = variable)) + 
        scale_y_sqrt("Damage [$]") + 
        xlab("Event Type") + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
        ggtitle("Economic Impact of Top 10 US Weather Events")

# Displaying the chart
print(econChart)

