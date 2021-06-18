# analysis for comparison waveform paper
source("UsefulFunctions.R")
library(ggplot2)
library(gridExtra)
library(vioplot)
#----------------------------------
# first we need to collect the data
#----------------------------------
sine <- getDataAllDF("L://Work//Laptop_Desktop_5_28_2021//Comparison Waveform Data new Complete//10kHz//SINE//")
tri <- getDataAllDF("L://Work//Laptop_Desktop_5_28_2021//Comparison Waveform Data new Complete//10kHz//TRI//")
squ <- getDataAllDF("L://Work//Laptop_Desktop_5_28_2021//Comparison Waveform Data new Complete//10kHz//SQU//")
sineAVG <- movingAverageDF(sine, 12)
triAVG <- movingAverageDF(tri, 12)
squAVG <- movingAverageDF(squ, 12)

#---------------------------
# central node voltage plots
#---------------------------
sineVPlot <- ggplot()+geom_line(aes(x=sine$Time, y=sine$V51))+
              geom_point(aes(x=sine$Time, y=sine$V51), shape=2)+
              geom_line(aes(x=sine$Time, y=sineAVG$V51), color="red")+
              xlim(50,51)+
              ylim(-105,10)+
              xlab("")+
              ylab("SINE V (mV)")
triVPlot <- ggplot()+geom_line(aes(x=tri$Time, y=tri$V51))+
              geom_line(aes(x=tri$Time, y=triAVG$V51), color="red")+
              xlim(50,51)+
              ylim(-105,10)+
              xlab("")+
              ylab("TRIANGLE V (mV)")
squVPlot <- ggplot()+geom_line(aes(x=squ$Time, y=squ$V51))+
              geom_line(aes(x=squ$Time, y=squAVG$V51), color="red")+
              xlim(50,51)+
              ylim(-105,10)+
              xlab("Time (ms)")+
              ylab("SQUARE V (mV)")
zoomVPlot <- grid.arrange(sineVPlot, triVPlot, squVPlot, ncol=1)

ggplot()+
  xlim(50,50.5)+
  ylim(-105,10)+
  xlab("Time (ms)")+
  geom_line(aes(x=sine$Time, y=sine$V51))+
  geom_point(aes(x=sine$Time, y=sine$V51), shape=1)+
  geom_line(aes(x=tri$Time, y=tri$V51))+
  geom_point(aes(x=tri$Time, y=tri$V51), shape=2)+
  geom_line(aes(x=squ$Time, y=squ$V51))+
  geom_point(aes(x=squ$Time, y=squ$V51), shape=7)

#--------------
# profile plots
#--------------
# we are going to specifically get the voltage data
t = 6000
print(sine[t,1])
x <- seq(1,101,by=1)  # create vector of node indices
sineVProfile <- as.vector(unlist(sineAVG[t,x+203])) # vector of voltage data
triVProfile <- as.vector(unlist(triAVG[t,x+203]))
squVProfile <- as.vector(unlist(squAVG[t,x+203]))
sineMProfile <- as.vector(unlist(sineAVG[t,x+1])) # vector of voltage data
triMProfile <- as.vector(unlist(triAVG[t,x+1]))
squMProfile <- as.vector(unlist(squAVG[t,x+1]))
sineHProfile <- as.vector(unlist(sineAVG[t,x+102])) # vector of voltage data
triHProfile <- as.vector(unlist(triAVG[t,x+102]))
squHProfile <- as.vector(unlist(squAVG[t,x+102]))

x <- x-1
sineVProfilePlot <- ggplot()+geom_line(aes(x=x,y=sineVProfile))+geom_point(aes(x=x,y=sineVProfile))
triVProfilePlot <- ggplot()+geom_line(aes(x=x,y=triVProfile))
squVProfilePlot <- ggplot()+geom_line(aes(x=x,y=squVProfile))
sineMProfilePlot <- ggplot()+geom_line(aes(x=x,y=sineMProfile))+geom_point(aes(x=x,y=sineMProfile))
triMProfilePlot <- ggplot()+geom_line(aes(x=x,y=triMProfile))
squMProfilePlot <- ggplot()+geom_line(aes(x=x,y=squMProfile))
sineHProfilePlot <- ggplot()+geom_line(aes(x=x,y=sineHProfile))+geom_point(aes(x=x,y=sineHProfile))
triHProfilePlot <- ggplot()+geom_line(aes(x=x,y=triHProfile))
squHProfilePlot <- ggplot()+geom_line(aes(x=x,y=squHProfile))


# voltage plots
sineVProfilePlot <- ggplot()+geom_line(aes(x=x,y=sineVProfile, color='black'))+#geom_point(aes(x=x,y=sineVProfile))+
                      ylab("Voltage (mV)")+
                      ylim(-85,-45)+
                      scale_colour_manual(values =c('black'='black'), labels = c('○'))+
                      theme(legend.position = c(0.9, 0.9), legend.title = element_blank(), 
                            legend.background = element_rect(fill=alpha('white', 0.8)),
                            legend.text = element_text(size=12))
  
triSineVProfile <- triVProfile - sineVProfile
squSineVProfile <- squVProfile - sineVProfile
diffVProfilePlot <- ggplot()+geom_line(aes(x=x,y=triSineVProfile, color="blue"))+#geom_point(aes(x=x,y=triSineVProfile), shape=2, color="blue")+
                        geom_line(aes(x=x,y=squSineVProfile, color="red"))+#geom_point(aes(x=x,y=squSineVProfile), shape=0, color="red")+
                        ylab("Voltage Difference (mV)")+
                        ylim(-20,20)+
                        scale_colour_manual(values =c('blue'='blue','red'='red'), labels = c('□ - ○','△ - ○'))+
                      theme(legend.position = c(0.9, 0.8), legend.title = element_blank(), 
                            legend.background = element_rect(fill=alpha('white', 0.8)),
                            legend.text = element_text(size=12))

diffVProfilePlotZoom <- ggplot()+geom_line(aes(x=x,y=triSineVProfile, color="blue"))+#geom_point(aes(x=x,y=triSineVProfile), shape=2, color="blue")+
  geom_line(aes(x=x,y=squSineVProfile, color="red"))+#geom_point(aes(x=x,y=squSineVProfile), shape=0, color="red")+
  ylab("Voltage Difference (mV)")+
  ylim(-0.2,0.2)+
  scale_colour_manual(values =c('blue'='blue','red'='red'), labels = c('□ - ○','△ - ○'))+
  theme(legend.position = c(0.9, 0.8), legend.title = element_blank(),
        legend.background = element_rect(fill=alpha('white', 0.5)),
        legend.text = element_text(size=12))

# m gate plots
sineMProfilePlot <- ggplot()+geom_line(aes(x=x,y=sineMProfile))+#geom_point(aes(x=x,y=sineMProfile))+
                      ylim(0,1)+
                      ylab("m Gate")
triSineMProfile <- triMProfile - sineMProfile
squSineMProfile <- squMProfile - sineMProfile
diffMProfilePlot <- ggplot()+geom_line(aes(x=x,y=triSineMProfile), color="blue")+#geom_point(aes(x=x,y=triSineMProfile), shape=2, color="blue")+
                        geom_line(aes(x=x,y=squSineMProfile), color="red")+#geom_point(aes(x=x,y=squSineMProfile), shape=0, color="red")+
                        #ylim(-0.002,0.007)+
    ylim(-0.5,0.5)+                    
    ylab("m Value Difference")

diffMProfilePlotZoom <- ggplot()+geom_line(aes(x=x,y=triSineMProfile), color="blue")+#geom_point(aes(x=x,y=triSineMProfile), shape=2, color="blue")+
  geom_line(aes(x=x,y=squSineMProfile), color="red")+#geom_point(aes(x=x,y=squSineMProfile), shape=0, color="red")+
  ylim(-0.003,0.007)+
  #ylim(-0.5,0.5)+                    
  ylab("m Value Difference")

# h gate plots
sineHProfilePlot <- ggplot()+geom_line(aes(x=x,y=sineHProfile))+#geom_point(aes(x=x,y=sineHProfile))+
                      ylim(0,1)+
                      ylab("h Gate")
triSineHProfile <- triHProfile - sineHProfile
squSineHProfile <- squHProfile - sineHProfile
diffHProfilePlot <- ggplot()+geom_line(aes(x=x,y=triSineHProfile), color="blue")+#geom_point(aes(x=x,y=triSineHProfile), shape=2, color="blue")+
  geom_line(aes(x=x,y=squSineHProfile), color="red")+#geom_point(aes(x=x,y=squSineHProfile), shape=0, color="red")+
  #ylim(-0.002,0.007)+
  ylim(-0.5,0.5)+
  ylab("h Value Difference")

diffHProfilePlotZoom <- ggplot()+geom_line(aes(x=x,y=triSineHProfile), color="blue")+#geom_point(aes(x=x,y=triSineHProfile), shape=2, color="blue")+
  geom_line(aes(x=x,y=squSineHProfile), color="red")+#geom_point(aes(x=x,y=squSineHProfile), shape=0, color="red")+
  ylim(-0.003,0.007)+
  #ylim(-0.5,0.5)+
  ylab("h Value Difference")


grid.arrange(sineVProfilePlot, diffVProfilePlot, diffVProfilePlotZoom,
             sineMProfilePlot, diffMProfilePlot, diffMProfilePlotZoom,
             sineHProfilePlot, diffHProfilePlot, diffHProfilePlotZoom,ncol=3)

# compined profile plots
vAvg <- (sineVProfile + triVProfile + squVProfile)/3
sineAvgDiff <- sineVProfile - vAvg
squAvgDiff <- squVProfile - vAvg
triAvgDiff <- triVProfile - vAvg
vAvgPlot <- ggplot()+geom_line(aes(x=x,y=vAvg))+
  geom_point(aes(x=x,y=vAvg))+
  ylab("Voltage (mV)")+
  xlab("Node")
vDiffPlot <- ggplot()+ geom_violin(aes(x=factor(1),y=triAvgDiff))+
  geom_violin(aes(x=factor(2),y=sineAvgDiff))+
  geom_violin(aes(x=factor(3),y=squAvgDiff))+
  ylab("Voltage Difference (mV)")+
  scale_x_discrete(labels=c("1" = "Triangle", "2" = "Sine", "3" = "Square"))+
  xlab("")

mAvg <- (sineMProfile + triMProfile + squMProfile)/3
sineAvgDiffM <- sineMProfile - mAvg
squAvgDiffM <- squMProfile - mAvg
triAvgDiffM <- triMProfile - mAvg
mAvgPlot <- ggplot()+geom_line(aes(x=x,y=mAvg))+
  geom_point(aes(x=x,y=mAvg))+
  ylab("m Gate")+
  xlab("Node")
mDiffPlot <- ggplot()+ geom_violin(aes(x=factor(1),y=triAvgDiffM))+
  geom_violin(aes(x=factor(2),y=sineAvgDiffM))+
  geom_violin(aes(x=factor(3),y=squAvgDiffM))+
  ylab("m Gate Difference")+
  scale_x_discrete(labels=c("1" = "Triangle", "2" = "Sine", "3" = "Square"))+
  xlab("")

hAvg <- (sineHProfile + triHProfile + squHProfile)/3
sineAvgDiffH <- sineHProfile - hAvg
squAvgDiffH <- squHProfile - hAvg
triAvgDiffH <- triHProfile - hAvg
hAvgPlot <- ggplot()+geom_line(aes(x=x,y=hAvg))+
  geom_point(aes(x=x,y=hAvg))+
  ylab("h Gate")+
  xlab("Node")
hDiffPlot <- ggplot()+ geom_violin(aes(x=factor(1),y=triAvgDiffH))+
  geom_violin(aes(x=factor(2),y=sineAvgDiffH))+
  geom_violin(aes(x=factor(3),y=squAvgDiffH))+
  ylab("h Gate Difference")+
  scale_x_discrete(labels=c("Triangle", "Sine", "Square"))+
  xlab("")
    
grid.arrange(sineVProfilePlot,
             triVProfilePlot,
             squVProfilePlot,
             vDiffPlot,
             sineMProfilePlot,
             triMProfilePlot,
             squMProfilePlot,
             mDiffPlot,
             sineHProfilePlot,
             triHProfilePlot,
             squHProfilePlot,
             hDiffPlot,ncol=4)

grid.arrange(vAvgPlot,vDiffPlot,
             mAvgPlot,mDiffPlot,
             hAvgPlot,hDiffPlot, ncol=2)

ggplot()+geom_abline(color="red")+
  geom_point(aes(x=sineVProfile,y=triVProfile))
  

#-------------
# Sample block
#-------------
sineSB <- getDataAllDF("L://Work//Laptop_Desktop_5_28_2021//Comparison Waveform Data new Complete//10kHzSampleBlock//SINE//")
sineNode0 <- ggplot(data=sineSB)+geom_line(aes(x=Time,y=V1))+ylim(-150,70)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
sineNode50 <- ggplot(data=sineSB)+geom_line(aes(x=Time,y=V51))+ylim(-150,70)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
sineNode100 <- ggplot(data=sineSB)+geom_line(aes(x=Time,y=V101))+ylim(-150,70)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
grid.arrange(sineNode0,sineNode50,sineNode100, ncol=3)

triSB <- getDataAllDF("L://Work//Laptop_Desktop_5_28_2021//Comparison Waveform Data new Complete//10kHzSampleBlock//TRI//")
triNode0 <- ggplot(data=triSB)+geom_line(aes(x=Time,y=V1))+ylim(-150,70)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
triNode50 <- ggplot(data=triSB)+geom_line(aes(x=Time,y=V51))+ylim(-150,70)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
triNode100 <- ggplot(data=triSB)+geom_line(aes(x=Time,y=V101))+ylim(-150,70)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
grid.arrange(triNode0,triNode50,triNode100, ncol=3)

squSB <- getDataAllDF("L://Work//Laptop_Desktop_5_28_2021//Comparison Waveform Data new Complete//10kHzSampleBlock//SQU//")
squNode0 <- ggplot(data=squSB)+geom_line(aes(x=Time,y=V1))+ylim(-150,70)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
squNode50 <- ggplot(data=squSB)+geom_line(aes(x=Time,y=V51))+ylim(-150,70)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
squNode100 <- ggplot(data=squSB)+geom_line(aes(x=Time,y=V101))+ylim(-150,70)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
grid.arrange(squNode0,squNode50,squNode100, ncol=3)

all_volt_plots <- grid.arrange(triNode0,triNode50,triNode100,
             sineNode0,sineNode50,sineNode100,
             squNode0,squNode50,squNode100, ncol=3)

ggsave("L://Work//Manuscripts//Comparison Waveform Paper//simSetupFull//voltPlots.eps", all_volt_plots, height=12,width=16)

# -----------------------
# discrete waveform plots
# -----------------------

x <- seq(0,2*3.14, by=3.14/6)
y <- sin(x)
discrete_sine <- ggplot()+geom_point(aes(x=x,y=y),size=4)+geom_line(aes(x=x,y=y),size=1)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
ggsave("L://Work//Manuscripts//Comparison Waveform Paper//simSetupFull//discrete_sine.eps", discrete_sine, height=4,width=8)

x <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)
y <- c(-1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,1)
discrete_square <- ggplot()+geom_point(aes(x=x,y=y),size=4)+geom_line(aes(x=x,y=y),size=1)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
ggsave("L://Work//Manuscripts//Comparison Waveform Paper//simSetupFull//discrete_square.eps", discrete_square, height=4,width=8)

x <- c(0,1,2,3,4,5,6,7,8,9,10,11,12)
y <- c(0,1,2,3,2,1,0,-1,-2,-3,-2,-1,0)
discrete_tri <- ggplot()+geom_point(aes(x=x,y=y),size=4)+geom_line(aes(x=x,y=y),size=1)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
ggsave("L://Work//Manuscripts//Comparison Waveform Paper//simSetupFull//discrete_tri.eps", discrete_tri, height=4,width=8)
