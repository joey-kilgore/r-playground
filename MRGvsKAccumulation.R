#--------------------
# Libraries/Functions
#--------------------
calcCol <- function(type, index){
  # given a compartment type and index
  #  return the column number of the KO data set
  
  # Compartment ordering is as follows
  # IS - IS - IS - PS - MS - Node - MS - PS - IS -IS -IS
  # and repeats 101 times total
  
  col = -1 # default invalid column
  if(type=="IS"){
    nodeNum = floor(index/6)+1 # for every 6 IS segments we have 1 node
    col = ((nodeNum-1)*11)+6
    if(index%%6<3){
      # left side of node
      col = col - 2 - (3-(index%%6))
    }else{
      col = col + 2 + (index%%6 -2)
    }
  }
  if(type=="PS"){
    nodeNum = floor(index/2)+1 # for every 6 IS segments we have 1 node
    col = ((nodeNum-1)*11)+6
    if(index%%2<1){
      # left side of node
      col = col - 2
    }else{
      col = col + 2
    }
  }
  if(type=="MS"){
    nodeNum = floor(index/2)+1 # for every 6 IS segments we have 1 node
    col = ((nodeNum-1)*11)+6
    if(index%%2<1){
      # left side of node
      col = col - 1
    }else{
      col = col + 1
    }
  }
  if(type=="Node"){
    col = ((index)*11)+6
  }
  
  #return column (offset for Time column)
  col+1
}
library(ggplot2)
library(grid)
library(gridExtra)
source("UsefulFunctions.R")
#----------
# Load data
#----------
mrg <- getDataAllDF("C:/Users/Joey/Desktop/TestData/DC/Cathode_1xBT/")
k <-   getDataAllDF("C:/Users/Joey/Desktop/TestData/kaccumulation/CathodicDC/1xBT/")

#----------------
# Plots over time
#----------------
# gate variable plots
# central node
kGates <- ggplot(data=k)+
  geom_path(aes(x=Time,y=M51, color="blue"))+
  geom_path(aes(x=Time,y=H51, color="red"))+
  geom_path(aes(x=Time,y=S51, color="green"))+
  geom_path(aes(x=Time,y=MP51, color="purple"))+
  xlim(40,140)+
  xlab("Time (ms)")+
  ylab("Gate Probability")+
  ggtitle("Central Node K+ Accumulation Model DC 1xBT")+
  scale_color_identity(name = "Gate",
                       breaks = c("blue","red","purple","green"),
                       labels = c("M","H","MP","S"),
                       guide = "legend")+
  theme(legend.position = c(0.03,0.75), plot.title = element_text(hjust=0.5))
kGates

mrgGates <- ggplot(data=mrg)+
  geom_path(aes(x=Time,y=M51, color="blue"))+
  geom_path(aes(x=Time,y=H51, color="red"))+
  geom_path(aes(x=Time,y=S51, color="green"))+
  geom_path(aes(x=Time,y=MP51, color="purple"))+
  xlim(0, 100)+
  xlab("Time (ms)")+
  ylab("Gate Probability")+
  ggtitle("Central Node MRG Model DC 1xBT")+
  scale_color_identity(name = "Gate",
                       breaks = c("blue","red","purple","green"),
                       labels = c("M","H","MP","S"),
                       guide = "legend")+
  theme(legend.position = c(0.03,0.75), plot.title = element_text(hjust=0.5))
mrgGates

gates <- grid.arrange(kGates,mrgGates,ncol=1)
ggsave("C:/Users/Joey/Desktop/TestData/kaccumulation/CathodicDC/plots/mrgVsk_centerGates.jpg", gates, width=12,heigh=6)


# 3 from center
kGates <- ggplot(data=k)+
  geom_path(aes(x=Time,y=M48, color="blue"))+
  geom_path(aes(x=Time,y=H48, color="red"))+
  geom_path(aes(x=Time,y=S48, color="green"))+
  geom_path(aes(x=Time,y=MP48, color="purple"))+
  xlim(40,140)+
  xlab("Time (ms)")+
  ylab("Gate Probability")+
  ggtitle("Left 3 from center Node K+ Accumulation Model DC 1xBT")+
  scale_color_identity(name = "Gate",
                       breaks = c("blue","red","purple","green"),
                       labels = c("M","H","MP","S"),
                       guide = "legend")+
  theme(legend.position = c(0.03,0.75), plot.title = element_text(hjust=0.5))
kGates

mrgGates <- ggplot(data=mrg)+
  geom_path(aes(x=Time,y=M48, color="blue"))+
  geom_path(aes(x=Time,y=H48, color="red"))+
  geom_path(aes(x=Time,y=S48, color="green"))+
  geom_path(aes(x=Time,y=MP48, color="purple"))+
  xlim(0, 100)+
  xlab("Time (ms)")+
  ylab("Gate Probability")+
  ggtitle("Left 3 from center Node MRG Model DC 1xBT")+
  scale_color_identity(name = "Gate",
                       breaks = c("blue","red","purple","green"),
                       labels = c("M","H","MP","S"),
                       guide = "legend")+
  theme(legend.position = c(0.03,0.75), plot.title = element_text(hjust=0.5))
mrgGates

gates <- grid.arrange(kGates,mrgGates,ncol=1)
ggsave("C:/Users/Joey/Desktop/TestData/kaccumulation/CathodicDC/plots/mrgVsk_left3Gates.jpg", gates, width=12,heigh=6)


kVolt <- ggplot(data=k)+
  geom_line(aes(x=Time, y=V51, color="red"))+
  geom_line(aes(x=Time, y=V48, color="purple"))+
  geom_line(aes(x=Time, y=V41, color="blue"))+
  xlim(40,140)+
  xlab("Time (ms)")+
  ylab("Transmembrane Potential (mV)")+
  ggtitle("K+ Accumulation Model Node Voltages DC 1xBT")+
  scale_color_identity(name = "Node",
                       breaks = c("red","purple","blue"),
                       labels = c("Center","3 Left", "10 Left"),
                       guide = "legend")+
  theme(legend.position = c(0.03,0.75), plot.title = element_text(hjust=0.5))
kVolt

mrgVolt <- ggplot(data=mrg)+
  geom_line(aes(x=Time, y=V51, color="red"))+
  geom_line(aes(x=Time, y=V48, color="purple"))+
  geom_line(aes(x=Time, y=V41, color="blue"))+
  xlim(0,100)+
  xlab("Time (ms)")+
  ylab("Transmembrane Potential (mV)")+
  ggtitle("MRG Model Node Voltages")+
  scale_color_identity(name = "Node",
                       breaks = c("red","purple","blue"),
                       labels = c("Center","3 Left", "10 Left"),
                       guide = "legend")+
  theme(legend.position = c(0.03,0.75), plot.title = element_text(hjust=0.5))
mrgVolt

volts <- grid.arrange(kVolt, mrgVolt, ncol=1)
ggsave("C:/Users/Joey/Desktop/TestData/kaccumulation/CathodicDC/plots/mrgVsk_voltages.jpg", volts, width=12,heigh=6)



kGates1 <- ggplot(data=k)+
  geom_path(aes(x=Time,y=M51, color="blue"))+
  geom_path(aes(x=Time,y=H51, color="red"))+
  geom_path(aes(x=Time,y=S51, color="green"))+
  geom_path(aes(x=Time,y=MP51, color="purple"))+
  xlim(549,650)+
  xlab("Time (ms)")+
  ylab("Gate Probability")+
  ggtitle("Central Node K+ Accumulation Model DC 1xBT")+
  scale_color_identity(name = "Gate",
                       breaks = c("blue","red","purple","green"),
                       labels = c("M","H","MP","S"),
                       guide = "legend")+
  theme(legend.position = c(0.03,0.75), plot.title = element_text(hjust=0.5))
kGates1
kGates2 <- ggplot(data=k)+
  geom_path(aes(x=Time,y=M48, color="blue"))+
  geom_path(aes(x=Time,y=H48, color="red"))+
  geom_path(aes(x=Time,y=S48, color="green"))+
  geom_path(aes(x=Time,y=MP48, color="purple"))+
  xlim(549,650)+
  xlab("Time (ms)")+
  ylab("Gate Probability")+
  ggtitle("3 from center Node K+ Accumulation Model DC 1xBT")+
  scale_color_identity(name = "Gate",
                       breaks = c("blue","red","purple","green"),
                       labels = c("M","H","MP","S"),
                       guide = "legend")+
  theme(legend.position = c(0.03,0.75), plot.title = element_text(hjust=0.5))
kGates2

grid.arrange(kGates1,kGates2,ncol=1)
