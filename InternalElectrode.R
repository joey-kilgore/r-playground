v1 <- read.csv(file = "C:/Users/Joey/Desktop/InternalElectrode/trial1/Voltage.csv", FALSE)
v2 <- read.csv(file = "C:/Users/Joey/Desktop/InternalElectrode/trial2/Voltage.csv", FALSE)
v3 <- read.csv(file = "C:/Users/Joey/Desktop/InternalElectrode/trial3/Voltage.csv", FALSE)
v4 <- read.csv(file = "C:/Users/Joey/Desktop/InternalElectrode/trial4/Voltage.csv", FALSE)
m1 <- read.csv(file = "C:/Users/Joey/Desktop/InternalElectrode/trial1/MGate.csv", FALSE)
m2 <- read.csv(file = "C:/Users/Joey/Desktop/InternalElectrode/trial2/MGate.csv", FALSE)
m3 <- read.csv(file = "C:/Users/Joey/Desktop/InternalElectrode/trial3/MGate.csv", FALSE)
m4 <- read.csv(file = "C:/Users/Joey/Desktop/InternalElectrode/trial4/MGate.csv", FALSE)
h1 <- read.csv(file = "C:/Users/Joey/Desktop/InternalElectrode/trial1/HGate.csv", FALSE)
h2 <- read.csv(file = "C:/Users/Joey/Desktop/InternalElectrode/trial2/HGate.csv", FALSE)
h3 <- read.csv(file = "C:/Users/Joey/Desktop/InternalElectrode/trial3/HGate.csv", FALSE)
h4 <- read.csv(file = "C:/Users/Joey/Desktop/InternalElectrode/trial4/HGate.csv", FALSE)

s4 <- read.csv(file = "C:/Users/Joey/Desktop/InternalElectrode/trial4/SGate.csv", FALSE)
mp4 <- read.csv(file = "C:/Users/Joey/Desktop/InternalElectrode/trial4/MPGate.csv", FALSE)

v1DF <- csvToDF(v1)
v2DF <- csvToDF(v2)
v3DF <- csvToDF(v3)
v4DF <- csvToDF(v4)
m1DF <- csvToDF(m1)
m2DF <- csvToDF(m2)
m3DF <- csvToDF(m3)
m4DF <- csvToDF(m4)
h1DF <- csvToDF(h1)
h2DF <- csvToDF(h2)
h3DF <- csvToDF(h3)
h4DF <- csvToDF(h4)

s4DF <- csvToDF(s4)
mp4DF <- csvToDF(mp4)

rm(v1)
rm(v2)
rm(v3)
rm(v4)
rm(m1)
rm(m2)
rm(m3)
rm(m4)
rm(h1)
rm(h2)
rm(h3)
rm(h4)

rm(s4)
rm(mp4)

v1AVGDF <- movingAverageDF(v1DF, 20)
v2AVGDF <- movingAverageDF(v2DF, 20)
v3AVGDF <- movingAverageDF(v3DF, 20)
v4AVGDF <- movingAverageDF(v4DF, 20)
m1AVGDF <- movingAverageDF(m1DF, 20)
m2AVGDF <- movingAverageDF(m2DF, 20)
m3AVGDF <- movingAverageDF(m3DF, 20)
m4AVGDF <- movingAverageDF(m4DF, 20)
h1AVGDF <- movingAverageDF(h1DF, 20)
h2AVGDF <- movingAverageDF(h2DF, 20)
h3AVGDF <- movingAverageDF(h3DF, 20)
h4AVGDF <- movingAverageDF(h4DF, 20)

s4AVGDF <- movingAverageDF(s4DF, 20)
mp4AVGDF <- movingAverageDF(mp4DF, 20)

plotMultipleNodes(v1AVGDF, "Trial 1 AVG Voltage", 1,99,-100,40)
plotMultipleNodes(v2AVGDF, "Trial 2 AVG Voltage", 1,99,-100,40)
plotMultipleNodes(v3AVGDF, "Trial 3 AVG Voltage", 1,99,-100,40)
plotMultipleNodes(v4AVGDF, "Trial 4 AVG Voltage", 1,99,-100,40)
plotBlock(v4AVGDF, "Trial 4 AVG Voltage", 1,99,-100,40)

tempvDF <- v4AVGDF
tempmDF <- m4AVGDF
temphDF <- h4AVGDF
tempsDF <- s4AVGDF
tempmpDF <- mp4AVGDF
times <- tempmDF[1:nrow(tempmDF), 1]
tempAllDF <- data.frame("Time" = times)
for (num in 2:(ncol(tempmDF))){
  tempAllDF <- cbind(tempAllDF, NODE = tempmDF[1:nrow(tempmDF), num])
  names(tempAllDF)[num] <- paste("M", num-1, sep="")
}
for (num in 2:(ncol(temphDF))){
  tempAllDF <- cbind(tempAllDF, NODE = temphDF[1:nrow(temphDF), num])
  names(tempAllDF)[num+101] <- paste("H", num-1, sep="")
}
for (num in 2:(ncol(tempvDF))){
  tempAllDF <- cbind(tempAllDF, NODE = tempvDF[1:nrow(tempvDF), num])
  names(tempAllDF)[num+202] <- paste("V", num-1, sep="")
}
for (num in 2:(ncol(tempsDF))){
  tempAllDF <- cbind(tempAllDF, NODE = tempsDF[1:nrow(tempsDF), num])
  names(tempAllDF)[num+303] <- paste("S", num-1, sep="")
}
for (num in 2:(ncol(tempmpDF))){
  tempAllDF <- cbind(tempAllDF, NODE = tempmpDF[1:nrow(tempmpDF), num])
  names(tempAllDF)[num+404] <- paste("MP", num-1, sep="")
}

all4AVGDF <- tempAllDF
head(all4AVGDF)

ggplot(all1AVGDF)+
  geom_point(aes(x=M51,y=H51,colour=Time))+
  scale_colour_gradient2(low = "blue", 
                         high = "red", 
                         mid="purple",
                         midpoint = 30)+
  xlim(0,1)+
  ylim(0,1)+
  ggtitle("Trial 1 Node 51 H vs M")


plotly::plot_ly(x=all4AVGDF$M51, y=all4AVGDF$H51, z=all4AVGDF$V51, type="scatter3d", model="markers")

ggplot(all4DF[all4DF$Time>50 & all4DF$Time<50.1,])+
  geom_path(aes(x=M46,y=H46), color="blue", size=1, linetype="solid")+
  geom_path(aes(x=M47,y=H47), color="green", size=1, linetype="solid")+
  geom_path(aes(x=M48,y=H48), color="yellow", size=1, linetype="solid")+
  geom_path(aes(x=M49,y=H49), color="orange", size=1, linetype="solid")+
  geom_path(aes(x=M50,y=H50), color="red", size=1, linetype="solid")+
  geom_path(aes(x=M51,y=H51), color="purple", size=1, linetype="solid")+
  geom_path(aes(x=M52,y=H52), color="blue", size=1, linetype="solid")+
  geom_path(aes(x=M53,y=H53), color="green", size=1, linetype="solid")+
  geom_path(aes(x=M54,y=H54), color="yellow", size=1, linetype="solid")+
  geom_path(aes(x=M55,y=H55), color="orange", size=1, linetype="solid")+
  geom_path(aes(x=M56,y=H56), color="red", size=1, linetype="solid")+
  xlim(0,1)+
  ylim(0,1)+
  ggtitle("Trial 4 H vs M")

ggplot(all1AVGDF)+
  geom_point(aes(x=M53,y=H53), color="#CC4444")+
  geom_point(aes(x=M59,y=H59), color="#4400BB")+
  geom_point(aes(x=M52,y=H52), color="#FF8888")+
  geom_point(aes(x=M51,y=H51), color="#55FF44")+
  xlim(0,1)+
  ylim(0,1)+
  ggtitle("Trial 1 H vs M")
