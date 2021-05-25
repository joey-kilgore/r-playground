# input the data from csv
hgate8 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/Trial8/HGate.csv", FALSE)
kcur8 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/Trial8/IK.csv", FALSE)
lcur8 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/Trial8/IL.csv", FALSE)
nacur8 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/Trial8/INa.csv", FALSE)
mgate8 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/Trial8/MGate.csv", FALSE)
mpgate8 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/Trial8/NGate.csv", FALSE)
sgate8 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/Trial8/SGate.csv", FALSE)
v8 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/Trial8/Voltage.csv", FALSE)

vSine <- read.csv(file = "C:/Users/Joey/Desktop/Comparison Waveform/Data/Sine10kBlock/Voltage.csv")
vSquare <- read.csv(file = "C:/Users/Joey/Desktop/Comparison Waveform/Data/Square10kBlock/Voltage.csv")
vTri <- read.csv(file = "C:/Users/Joey/Desktop/Comparison Waveform/Data/Tri10kBlock/Voltage.csv")
nacurSine <- read.csv(file = "C:/Users/Joey/Desktop/Comparison Waveform/Data/Sine10kBlock/INa.csv", FALSE)
nacurTri <- read.csv(file = "C:/Users/Joey/Desktop/Comparison Waveform/Data/Tri10kBlock/INa.csv", FALSE)
nacurSquare <- read.csv(file = "C:/Users/Joey/Desktop/Comparison Waveform/Data/Square10kBlock/INa.csv", FALSE)

h1 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/DCAmpComparison/trial1/HGate.csv", FALSE)
h2 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/DCAmpComparison/trial2/HGate.csv", FALSE)
h3 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/DCAmpComparison/trial3/HGate.csv", FALSE)
h4 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/DCAmpComparison/trial4/HGate.csv", FALSE)
h5 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/DCAmpComparison/trial5/HGate.csv", FALSE)
h6 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/DCAmpComparison/trial6/HGate.csv", FALSE)
h7 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/DCAmpComparison/trial7/HGate.csv", FALSE)
h8 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/DCAmpComparison/trial8/HGate.csv", FALSE)
h9 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/DCAmpComparison/trial9/HGate.csv", FALSE)
m10 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/DCAmpComparison/trial10/MGate.csv", FALSE)


# load packages
#install.packages("Rcpp")
suppressWarnings(
  if (!require("ggplot2", quietly = TRUE))
    install.packages("ggplot2", quiet = TRUE))

suppressWarnings(
  if (!require("gapminder", quietly = TRUE))
    install.packages("gapminder", quiet = TRUE))

suppressWarnings(
  if (!require("reshape2", quietly = TRUE))
    install.packages("reshape2", quiet = TRUE))

library("ggplot2", quietly = TRUE)
library("gapminder", quietly = TRUE)
library("reshape2", quietly = TRUE)

# x: the vector
# n: the number of samples
# centered: if FALSE, then average current sample and previous (n-1) samples
#           if TRUE, then average symmetrically in past and future. (If n is even, use one more sample from future.)
movingAverage <- function(x, n = 1, centered = FALSE) {
  if (centered) {
    before <- floor((n - 1) / 2)
    after <- ceiling((n - 1) / 2)
  } else {
    before <- n - 1
    after <- 0
  }
  # Track the sum and count of number of non-NA items
  s <- rep(0, length(x))
  count <- rep(0, length(x))
  # Add the centered data 
  new <- x
  # Add to count list wherever there isn't a 
  count <- count + !is.na(new)
  # Now replace NA_s with 0_s and add to total
  new[is.na(new)] <- 0
  s <- s + new
  # Add the data from before
  i <- 1
  while (i <= before) {
    # This is the vector with offset values to add
    new <- c(rep(NA, i), x[1:(length(x) - i)])
    
    count <- count + !is.na(new)
    new[is.na(new)] <- 0
    s <- s + new
    
    i <- i + 1
  }
  # Add the data from after
  i <- 1
  while (i <= after) {
    # This is the vector with offset values to add
    new <- c(x[(i + 1):length(x)], rep(NA, i))
    count <- count + !is.na(new)
    new[is.na(new)] <- 0
    s <- s + new
    i <- i + 1
  }
  # return sum divided by count
  s / count
}
movingAverageDF <- function(df, n) {
  newDF <- df
  for (name in colnames(df)) {
    if (!identical(name, "Time")) {
      # loop through every column of the data (which is every node)
      newDF[[name]] <- movingAverage(df[[name]], n, FALSE) # find the moving average
      names(newDF)[num] <- paste(name) # label the new column ("Node<num>")
    }
  }
  newDF
}
slopeDF <- function(df, collumns) {
  newDF <- df
  for (name in collumns) {
    if (!identical(name, "Time")) {
      # loop through every column of the data (which is every node)
      for (row in 2:nrow(df)) {
        
        newDF[row, name] = (df[row, name] - df[row - 1, name])
      }
    }
  }
  newDF
}

csvToDF <- function(csv){
  tempData <- csv
  times <- tempData[1:nrow(tempData), 1] # read the times listed in the first column
  tempDF <- data.frame("Time" = times) # Make the dataframe, with just the time data
  names(tempDF)[1] <- "Time" # Title the name of the time data
  for (num in 2:(ncol(tempData) - 1)) {
    # loop through every column of the data (which is every node)
    tempDF <- cbind(tempDF, NODE = tempData[1:nrow(tempData), num]) # add the next column of the data to the data frame
    names(tempDF)[num] <- paste("Node", num - 1, sep = "") # label the new column ("Node<num>")
  }
  tempDF
}



v10AVGDF <- movingAverageDF(v10DF, 10)
v10AVGSlopeDF <- slopeDF(v10AVGDF, c("Node51", "Node50", "Node49", "Node48", "Node47", "Node46", "Node45", "Node44", "Node43", "Node42", "Node41"))

# Convert csv data to a more useful data frame
# change 'ngate2' on the line below to whatever data from csv you want
tempData <- h5
times <- tempData[1:nrow(tempData), 1] # read the times listed in the first column
tempDF <- data.frame("Time" = times) # Make the dataframe, with just the time data
names(tempDF)[1] <- "Time" # Title the name of the time data
for (num in 2:(ncol(tempData) - 1)) {
  # loop through every column of the data (which is every node)
  tempDF <- cbind(tempDF, NODE = tempData[1:nrow(tempData), num]) # add the next column of the data to the data frame
  names(tempDF)[num] <- paste("Node", num - 1, sep = "") # label the new column ("Node<num>")
}
# change 'ngate2DF' to where you would like to store the new dataframe
h5DF <- tempDF

m5DF <- csvToDF(m5)

# The following will create a set of plots, each being the specified data at multiple time points
numValuesPerTime <- ncol(hgate1) - 2    # set the time values
x <- 2:numValuesPerTime                 # create list of numbers used for nodes
data <- hgate1                          # set the data being used
for (num in c(4000, 4100, 4200, 4300, 4400, 4500, 4600, 4700, 4800, 4900)) {    # loop through a set of times
  y <- data[num, x]   # read data row, which is the values at a specific time
  y <- t(y)           # transpose y to be a vertical vector
  y <- unname(y)      # unname so that it can be named when making the dataframe
  dataFrameNew <- data.frame("NODE" = x, "HGate" = y) # create the dataframe
  print(head(dataFrameNew))                           # output the dataframe head
  p4 <- ggplot(dataFrameNew, aes(NODE, HGate)) + geom_point(color = 'BLUE') +
    coord_cartesian(ylim = c(0, 1)) +
    ggtitle(paste('H Gate @ Time = ', data[num, 1], 'ms'))  # make basic plot
  print(p4)   # neet to print the plot because it is within a for loop
}

# The following will create a set of plots, each being the specified data at multiple time points
dataCsv <- vTri
numValuesPerTime <- ncol(dataCsv) - 1 # set the time values
numValuesPerTime <- ncol(dataCsv) - 1 # set the time values
x <- 2:numValuesPerTime # create list of numbers used for nodes
data <- dataCsv # set the data being used
for (num in c(2000, 2050, 2100)) {
  # loop through a set of times
  y <- data[num, x] # read data row, which is the values at a specific time
  y <- t(y) # transpose y to be a vertical vector
  y <- unname(y) # unname so that it can be named when making the dataframe
  dataFrameNew <- data.frame("NODE" = x, "Value" = y) # create the dataframe
  print(head(dataFrameNew)) # output the dataframe head
  p4 <- ggplot(dataFrameNew, aes(NODE, Value)) + geom_point(color = 'BLUE') +
    ggtitle(paste('Value @ Time = ', data[num, 1], 'ms')) # make basic plot
  print(p4) # neet to print the plot because it is within a for loop
}



# take current dataframe and make a moving average dataframe
tempDF <- v10DF
newDF <- data.frame("Time" = times)

movingAvg <- movavg(v10DF["Node51"], 10, "s")

# Plotting m^3 * h
tempm <- m10
temph <- h10
times <- temph[1:nrow(temph), 1] # read the times listed in the first column
tempmhDF <- data.frame("Time" = times)
for (num in 2:(ncol(tempm) - 1)) {
  # loop through every column of the data (which is every node)
  tempmhDF <- cbind(tempmhDF, NODE = tempm[1:nrow(tempm), num] ^ 3 * temph[1:nrow(temph), num]) # add the next column of the data to the data frame
  names(tempmhDF)[num] <- paste("Node", num - 1, sep = "") # label the new column ("Node<num>")
}
mh10DF <- tempmhDF
ggplot(m3h1DF, aes(Time, Node50)) + geom_point(color = "#999922")

ggplot(nacur5DF, aes(Time, Node51))+geom_point(color="#55EE55")+geom_point(data = kcur5DF, aes(Time,Node51), color="#5555EE")

# Increase text size
theme_update(text = element_text(size = 20))


# Plot m and h on top of eachother
ggplot(mgate1DF, aes(Time, Node50)) + geom_point(color = "#33FF66") + geom_point(data = hgate1DF, aes(Time, Node50), color = "#3333CC") + ggtitle("MGATE 1 Node 50") # sample plot
p <- ggplot(mgate1DF, aes(Time, Node50)) + geom_point(color = "#33FF66") + geom_point(data = hgate1DF, aes(Time, Node50), color = "#3333CC") + ggtitle("M and H Gate Node 50") + labs(x = "Time (ms)", y = "Gate Value")


# Plot m from two different nodes
ggplot(mgate2DF, aes(Time, Node50)) + geom_point(color = "#11FF11") + geom_point(data = mgate2DF, aes(Time, Node49), color = "#EE1122") + ggtitle("Trial6 MGate nodes 50 and 49")


# Plot m and h from two different trials
ggplot(mgate1DF, aes(Time, Node50)) + geom_point(color = "#33FF66") + geom_point(data = mgate2DF, aes(Time, Node50), color = "#3333CC") + geom_point(data = hgate1DF, aes(Time, Node50), color = "#33FF66") + geom_point(data = hgate2DF, aes(Time, Node50), color = "#3333CC") + ggtitle("Trial7 and Trial 5")

tempDF <- h1DF
ggplot(data = tempDF, aes(Time, Node51)) + geom_line(color = "#55FF44", size = 2) +
  geom_line(data = tempDF, aes(Time, Node42), color = "#4400FF", size = 2) +
  geom_line(data = tempDF, aes(Time, Node43), color = "#4400BB", size = 2) +
  geom_line(data = tempDF, aes(Time, Node44), color = "#110088", size = 2) +
  geom_line(data = tempDF, aes(Time, Node45), color = "#110044", size = 2) +
  geom_line(data = tempDF, aes(Time, Node46), color = "#000000", size = 2) +
  geom_line(data = tempDF, aes(Time, Node47), color = "#440000", size = 2) +
  geom_line(data = tempDF, aes(Time, Node48), color = "#990000", size = 2) +
  geom_line(data = tempDF, aes(Time, Node49), color = "#CC4444", size = 2) +
  geom_line(data = tempDF, aes(Time, Node50), color = "#FF8888", size = 2) +
  ggtitle("Na Current AVG Slope Trial 8 Center (GREEN) and next 9 nodes (Pink to Blue)") +
  xlim(90, 45)