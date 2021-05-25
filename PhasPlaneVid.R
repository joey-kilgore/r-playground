v1 <- read.csv(file = "C:/Users/Joey/Desktop/Block/trial1/Voltage.csv", FALSE)
m1 <- read.csv(file = "C:/Users/Joey/Desktop/Block/trial1/MGate.csv", FALSE)
h1 <- read.csv(file = "C:/Users/Joey/Desktop/Block/trial1/HGate.csv", FALSE)
s1 <- read.csv(file = "C:/Users/Joey/Desktop/Block/trial1/SGate.csv", FALSE)
mp1 <- read.csv(file = "C:/Users/Joey/Desktop/Block/trial1/MPGate.csv", FALSE)

v2 <- read.csv(file = "C:/Users/Joey/Desktop/Block/trial2/Voltage.csv", FALSE)
m2 <- read.csv(file = "C:/Users/Joey/Desktop/Block/trial2/MGate.csv", FALSE)
h2 <- read.csv(file = "C:/Users/Joey/Desktop/Block/trial2/HGate.csv", FALSE)
s2 <- read.csv(file = "C:/Users/Joey/Desktop/Block/trial2/SGate.csv", FALSE)
mp2 <- read.csv(file = "C:/Users/Joey/Desktop/Block/trial2/MPGate.csv", FALSE)

v1DF <- csvToDF(v1)
m1DF <- csvToDF(m1)
h1DF <- csvToDF(h1)
s1DF <- csvToDF(s1)
mp1DF <- csvToDF(mp1)
v2DF <- csvToDF(v2)
m2DF <- csvToDF(m2)
h2DF <- csvToDF(h2)
s2DF <- csvToDF(s2)
mp2DF <- csvToDF(mp2)

rm(v1)
rm(v2)
rm(m1)
rm(m2)
rm(h1)
rm(h2)
rm(s1)
rm(s2)
rm(mp1)
rm(mp2)


tempvDF <- v2DF
tempmDF <- m2DF
temphDF <- h2DF
tempsDF <- s2DF
tempmpDF <- mp2DF
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
all2DF <- tempAllDF
head(all2DF)

for (num in seq(10,30,by=.1)){
  plot <- ggplot(all1DF[all1DF$Time>num & all1DF$Time<num+.1,])+
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
            ylim(0,.3)+
            ggtitle(paste("Trial 1 H vs M", num, sep=" ", collapse = NULL))
  ggsave(filename = paste0("./phaseplane/",num*100,".png"), plot, width = 8, height = 6, dpi=150)
}
list.files(path = "./phaseplane/", pattern = "*.png", full.names = T) %>%
  map(image_read) %>%
  image_join() %>%
  image_animate(fps=5) %>%
  image_write("phaseplane1.gif")



ggplot(all2DF[all2DF$Time>50 & all2DF$Time<52,])+
  geom_path(aes(x=M46,y=H46), color="blue", size=1, linetype="solid")+
  geom_path(aes(x=M47,y=H47), color="green", size=1, linetype="solid")+
  geom_path(aes(x=M48,y=H48), color="black", size=1, linetype="solid")+
  geom_path(aes(x=M49,y=H49), color="orange", size=1, linetype="solid")+
  geom_path(aes(x=M50,y=H50), color="red", size=1, linetype="solid")+
  geom_path(aes(x=M51,y=H51), color="purple", size=1, linetype="solid")+
  geom_path(aes(x=M52,y=H52), color="blue", size=1, linetype="solid")+
  geom_path(aes(x=M53,y=H53), color="green", size=1, linetype="solid")+
  geom_path(aes(x=M54,y=H54), color="yellow", size=1, linetype="solid")+
  geom_path(aes(x=M55,y=H55), color="orange", size=1, linetype="solid")+
  geom_path(aes(x=M56,y=H56), color="red", size=1, linetype="solid")+
  xlim(0.5,1)+
  ylim(0,.1)+
  ggtitle("Trial 2 H vs M")