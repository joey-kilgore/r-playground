m1 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/DCAmpComparison/trial1/MGate.csv", FALSE)
m2 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/DCAmpComparison/trial2/MGate.csv", FALSE)
m3 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/DCAmpComparison/trial3/MGate.csv", FALSE)
m4 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/DCAmpComparison/trial4/MGate.csv", FALSE)
m5 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/DCAmpComparison/trial5/MGate.csv", FALSE)
m6 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/DCAmpComparison/trial6/MGate.csv", FALSE)
m7 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/DCAmpComparison/trial7/MGate.csv", FALSE)
m8 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/DCAmpComparison/trial8/MGate.csv", FALSE)
m9 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/DCAmpComparison/trial9/MGate.csv", FALSE)
m10 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/DCAmpComparison/trial10/MGate.csv", FALSE)
h1 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/DCAmpComparison/trial1/HGate.csv", FALSE)
h2 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/DCAmpComparison/trial2/HGate.csv", FALSE)
h3 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/DCAmpComparison/trial3/HGate.csv", FALSE)
h4 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/DCAmpComparison/trial4/HGate.csv", FALSE)
h5 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/DCAmpComparison/trial5/HGate.csv", FALSE)
h6 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/DCAmpComparison/trial6/HGate.csv", FALSE)
h7 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/DCAmpComparison/trial7/HGate.csv", FALSE)
h8 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/DCAmpComparison/trial8/HGate.csv", FALSE)
h9 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/DCAmpComparison/trial9/HGate.csv", FALSE)
h10 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/DCAmpComparison/trial10/HGate.csv", FALSE)
m1DF <- csvToDF(m1)
m2DF <- csvToDF(m2)
m3DF <- csvToDF(m3)
m4DF <- csvToDF(m4)
m5DF <- csvToDF(m5)
m6DF <- csvToDF(m6)
m7DF <- csvToDF(m7)
m8DF <- csvToDF(m8)
m9DF <- csvToDF(m9)
m10DF <- csvToDF(m10)
h1DF <- csvToDF(h1)
h2DF <- csvToDF(h2)
h3DF <- csvToDF(h3)
h4DF <- csvToDF(h4)
h5DF <- csvToDF(h5)
h6DF <- csvToDF(h6)
h7DF <- csvToDF(h7)
h8DF <- csvToDF(h8)
h9DF <- csvToDF(h9)
h10DF <- csvToDF(h10)

m1AVGDF <- movingAverageDF(m1DF, 10)
m2AVGDF <- movingAverageDF(m2DF, 10)
m3AVGDF <- movingAverageDF(m3DF, 10)
m4AVGDF <- movingAverageDF(m4DF, 10)
m5AVGDF <- movingAverageDF(m5DF, 10)
m6AVGDF <- movingAverageDF(m6DF, 10)
m7AVGDF <- movingAverageDF(m7DF, 10)
m8AVGDF <- movingAverageDF(m8DF, 10)
m9AVGDF <- movingAverageDF(m9DF, 10)
m10AVGDF <- movingAverageDF(m10DF, 10)
h1AVGDF <- movingAverageDF(h1DF, 10)
h2AVGDF <- movingAverageDF(h2DF, 10)
h3AVGDF <- movingAverageDF(h3DF, 10)
h4AVGDF <- movingAverageDF(h4DF, 10)
h5AVGDF <- movingAverageDF(h5DF, 10)
h6AVGDF <- movingAverageDF(h6DF, 10)
h7AVGDF <- movingAverageDF(h7DF, 10)
h8AVGDF <- movingAverageDF(h8DF, 10)
h9AVGDF <- movingAverageDF(h9DF, 10)
h10AVGDF <- movingAverageDF(h10DF, 10)

tempmDF <- m3AVGDF
temphDF <- h3AVGDF
times <- tempmDF[1:nrow(tempmDF), 1]
tempAllDF <- data.frame("Time" = times)
for (num in 2:(ncol(tempmDF) -1)){
  tempAllDF <- cbind(tempAllDF, NODE = tempmDF[1:nrow(tempmDF), num])
  names(tempAllDF)[num] <- paste("M", num-1, sep="")
}
for (num in 2:(ncol(temphDF)-1)){
  tempAllDF <- cbind(tempAllDF, NODE = temphDF[1:nrow(temphDF), num])
  names(tempAllDF)[num+100] <- paste("H", num-1, sep="")
}
all3AVGDF <- tempAllDF
head(all3AVGDF)

preDC <- all3AVGDF[(all3AVGDF$Time>10 & all3AVGDF$Time<30),]
head(preDC)
ggplot(preDC)+
  geom_point(aes(x=M51,y=H51, colour="#0000FF", size="Time"))+
  geom_point(aes(x=M52,y=H52, colour="#4400BB", size="Time"))+
  geom_point(aes(x=M53,y=H53, colour="#110088", size="Time"))+
  geom_point(aes(x=M54,y=H54, colour="#110044", size="Time"))+
  geom_point(aes(x=M55,y=H55, colour="#000000", size="Time"))+
  geom_point(aes(x=M56,y=H56, colour="#440000", size="Time"))+
  geom_point(aes(x=M57,y=H57, colour="#990000", size="Time"))+
  geom_point(aes(x=M58,y=H58, colour="#CC4444", size="Time"))+
  xlim(0,1)+
  ylim(0,1)

ggplot(all3AVGDF, aes(x=M52,y=H52))+geom_point(aes(colour=Time))+scale_colour_gradient2(low = "blue", high = "red", mid="purple",midpoint = 30)+xlim(0,1)+ylim(0,1)
ggplot(all3AVGDF, aes(x=M53,y=H53))+geom_point(aes(colour=Time))+scale_colour_gradient2(low = "blue", high = "red", mid="purple",midpoint = 30)+xlim(0,1)+ylim(0,1)
ggplot(all3AVGDF, aes(x=M54,y=H54))+geom_point(aes(colour=Time))+scale_colour_gradient2(low = "blue", high = "red", mid="purple",midpoint = 30)+xlim(0,1)+ylim(0,1)
ggplot(all3AVGDF, aes(x=M55,y=H55))+geom_point(aes(colour=Time))+scale_colour_gradient2(low = "blue", high = "red", mid="purple",midpoint = 30)+xlim(0,1)+ylim(0,1)
ggplot(all3AVGDF, aes(x=M56,y=H56))+geom_point(aes(colour=Time))+scale_colour_gradient2(low = "blue", high = "red", mid="purple",midpoint = 30)+xlim(0,1)+ylim(0,1)
ggplot(all3AVGDF, aes(x=M57,y=H57))+geom_point(aes(colour=Time))+scale_colour_gradient2(low = "blue", high = "red", mid="purple",midpoint = 30)+xlim(0,1)+ylim(0,1)
ggplot(all3AVGDF, aes(x=M58,y=H58))+geom_point(aes(colour=Time))+scale_colour_gradient2(low = "blue", high = "red", mid="purple",midpoint = 30)+xlim(0,1)+ylim(0,1)


