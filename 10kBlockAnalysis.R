# needed libraries
# ggplot2
# magick
# magrittr
# purrr

# all50DF <- getDataAllDF("C:/Users/Joey/Desktop/Testdata/10kBlock/50BT/")



m50 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/10kBlock/50BT/MGate.csv", FALSE)
h50 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/10kBlock/50BT/HGate.csv", FALSE)
s50 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/10kBlock/50BT/SGate.csv", FALSE)
mp50 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/10kBlock/50BT/MPGate.csv", FALSE)
v50 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/10kBlock/50BT/Voltage.csv", FALSE)
m90 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/10kBlock/90BT/MGate.csv", FALSE)
h90 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/10kBlock/90BT/HGate.csv", FALSE)
s90 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/10kBlock/90BT/SGate.csv", FALSE)
mp90 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/10kBlock/90BT/MPGate.csv", FALSE)
v90 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/10kBlock/90BT/Voltage.csv", FALSE)
m97 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/10kBlock/97BT/MGate.csv", FALSE)
h97 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/10kBlock/97BT/HGate.csv", FALSE)
s97 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/10kBlock/97BT/SGate.csv", FALSE)
mp97 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/10kBlock/97BT/MPGate.csv", FALSE)
v97 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/10kBlock/97BT/Voltage.csv", FALSE)
m100 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/10kBlock/100BT/MGate.csv", FALSE)
h100 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/10kBlock/100BT/HGate.csv", FALSE)
s100 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/10kBlock/100BT/SGate.csv", FALSE)
mp100 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/10kBlock/100BT/MPGate.csv", FALSE)
v100 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/10kBlock/100BT/Voltage.csv", FALSE)
m103 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/10kBlock/103BT/MGate.csv", FALSE)
h103 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/10kBlock/103BT/HGate.csv", FALSE)
s103 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/10kBlock/103BT/SGate.csv", FALSE)
mp103 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/10kBlock/103BT/MPGate.csv", FALSE)
v103 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/10kBlock/103BT/Voltage.csv", FALSE)
m110 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/10kBlock/110BT/MGate.csv", FALSE)
h110 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/10kBlock/110BT/HGate.csv", FALSE)
s110 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/10kBlock/110BT/SGate.csv", FALSE)
mp110 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/10kBlock/110BT/MPGate.csv", FALSE)
v110 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/10kBlock/110BT/Voltage.csv", FALSE)
m150 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/10kBlock/150BT/MGate.csv", FALSE)
h150 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/10kBlock/150BT/HGate.csv", FALSE)
s150 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/10kBlock/150BT/SGate.csv", FALSE)
mp150 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/10kBlock/150BT/MPGate.csv", FALSE)
v150 <- read.csv(file = "C:/Users/Joey/Desktop/TestData/10kBlock/150BT/Voltage.csv", FALSE)

m50DF <- csvToDF(m50)
h50DF <- csvToDF(h50)
s50DF <- csvToDF(s50)
mp50DF <- csvToDF(mp50)
v50DF <- csvToDF(v50)
m90DF <- csvToDF(m90)
h90DF <- csvToDF(h90)
s90DF <- csvToDF(s90)
mp90DF <- csvToDF(mp90)
v90DF <- csvToDF(v90)
m97DF <- csvToDF(m97)
h97DF <- csvToDF(h97)
s97DF <- csvToDF(s97)
mp97DF <- csvToDF(mp97)
v97DF <- csvToDF(v97)
m100DF <- csvToDF(m100)
h100DF <- csvToDF(h100)
s100DF <- csvToDF(s100)
mp100DF <- csvToDF(mp100)
v100DF <- csvToDF(v100)
m103DF <- csvToDF(m103)
h103DF <- csvToDF(h103)
s103DF <- csvToDF(s103)
mp103DF <- csvToDF(mp103)
v103DF <- csvToDF(v103)
m110DF <- csvToDF(m110)
h110DF <- csvToDF(h110)
s110DF <- csvToDF(s110)
mp110DF <- csvToDF(mp110)
v110DF <- csvToDF(v110)
m150DF <- csvToDF(m150)
h150DF <- csvToDF(h150)
s150DF <- csvToDF(s150)
mp150DF <- csvToDF(mp150)
v150DF <- csvToDF(v150)

all50DF <- buildAllDF(m50DF, h50DF, s50DF, mp50DF, v50DF)
all90DF <- buildAllDF(m90DF, h90DF, s90DF, mp90DF, v90DF)
all97DF <- buildAllDF(m97DF, h97DF, s97DF, mp97DF, v97DF)
all100DF <- buildAllDF(m100DF, h100DF, s100DF, mp100DF, v100DF)
all103DF <- buildAllDF(m103DF, h103DF, s103DF, mp103DF, v103DF)
all110DF <- buildAllDF(m110DF, h110DF, s110DF, mp110DF, v110DF)
all150DF <- buildAllDF(m150DF, h150DF, s150DF, mp150DF, v150DF)

rm(m50)
rm(h50)
rm(s50)
rm(mp50)
rm(v50)
rm(m90)
rm(h90)
rm(s90)
rm(mp90)
rm(v90)
rm(m97)
rm(h97)
rm(s97)
rm(mp97)
rm(v97)
rm(m100)
rm(h100)
rm(s100)
rm(mp100)
rm(v100)
rm(m103)
rm(h103)
rm(s103)
rm(mp103)
rm(v103)
rm(m110)
rm(h110)
rm(s110)
rm(mp110)
rm(v110)
rm(m150)
rm(h150)
rm(s150)
rm(mp150)
rm(v150)

rm(m50DF)
rm(h50DF)
rm(s50DF)
rm(mp50DF)
rm(v50DF)
rm(m90DF)
rm(h90DF)
rm(s90DF)
rm(mp90DF)
rm(v90DF)
rm(m97DF)
rm(h97DF)
rm(s97DF)
rm(mp97DF)
rm(v97DF)
rm(m100DF)
rm(h100DF)
rm(s100DF)
rm(mp100DF)
rm(v100DF)
rm(m103DF)
rm(h103DF)
rm(s103DF)
rm(mp103DF)
rm(v103DF)
rm(m110DF)
rm(h110DF)
rm(s110DF)
rm(mp110DF)
rm(v110DF)
rm(m150DF)
rm(h150DF)
rm(s150DF)
rm(mp150DF)
rm(v150DF)

for (num in seq(10,40,by=.1)){
  plot <- ggplot()+
    geom_path(data = all97DF[all97DF$Time>num & all97DF$Time<num+.1,], aes(x=M47,y=H47), color="red", size=1)+
    geom_path(data = all97DF[all97DF$Time>num & all97DF$Time<num+.1,], aes(x=M48,y=H48), color="red", size=1)+
    geom_path(data = all97DF[all97DF$Time>num & all97DF$Time<num+.1,], aes(x=M49,y=H49), color="red", size=1)+
    geom_path(data = all97DF[all97DF$Time>num & all97DF$Time<num+.1,], aes(x=M50,y=H50), color="red", size=1)+
    geom_path(data = all97DF[all97DF$Time>num & all97DF$Time<num+.1,], aes(x=M51,y=H51), color="red", size=1)+
    
    geom_path(data = all100DF[all100DF$Time>num & all100DF$Time<num+.1,], aes(x=M47,y=H47), color="green", size=1)+
    geom_path(data = all100DF[all100DF$Time>num & all100DF$Time<num+.1,], aes(x=M48,y=H48), color="green", size=1)+
    geom_path(data = all100DF[all100DF$Time>num & all100DF$Time<num+.1,], aes(x=M49,y=H49), color="green", size=1)+
    geom_path(data = all100DF[all100DF$Time>num & all100DF$Time<num+.1,], aes(x=M50,y=H50), color="green", size=1)+
    geom_path(data = all100DF[all100DF$Time>num & all100DF$Time<num+.1,], aes(x=M51,y=H51), color="green", size=1)+

    geom_path(data = all103DF[all103DF$Time>num & all103DF$Time<num+.1,], aes(x=M47,y=H47), color="blue", size=1)+
    geom_path(data = all103DF[all103DF$Time>num & all103DF$Time<num+.1,], aes(x=M48,y=H48), color="blue", size=1)+
    geom_path(data = all103DF[all103DF$Time>num & all103DF$Time<num+.1,], aes(x=M49,y=H49), color="blue", size=1)+
    geom_path(data = all103DF[all103DF$Time>num & all103DF$Time<num+.1,], aes(x=M50,y=H50), color="blue", size=1)+
    geom_path(data = all103DF[all103DF$Time>num & all103DF$Time<num+.1,], aes(x=M51,y=H51), color="blue", size=1)+
    xlim(0,1)+
    ylim(0,.3)+
    ggtitle(paste("H vs M", num, sep=" ", collapse = NULL))
  ggsave(filename = paste0("./phaseplane/",num*100,".png"), plot, width = 8, height = 6, dpi=150)
}
list.files(path = "./phaseplane/", pattern = "*.png", full.names = T) %>%
  map(image_read) %>%
  image_join() %>%
  image_animate(fps=5) %>%
  image_write("97vs100vs103.gif")
