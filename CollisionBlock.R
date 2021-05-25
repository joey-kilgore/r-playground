# Getting plots for collision block of MRG model
# this will simply generate space plots of the nodes
# and look at the APs collide

library(ggplot2)
library(grid)
library(gridExtra)
source("UsefulFunctions.R")

v <- csvToDF(read.csv("C:/Users/Joey/Desktop/TestData/Collision Block/SimpleSample/Voltage.csv"))
     
for(t in seq(999,1299,by=10)){
  p <- ggplot()+
    geom_line(aes(x=seq(0,100),y=unlist(v[t,seq(2,102)])))+
    xlab("Node")+
    ylab("Transmembrane Potential (mV)")+
    ylim(-85,45)+
    ggtitle(sprintf("Time = %2.3f ms",v[t,1]-5))
  ggsave(filename = sprintf("C:/Users/Joey/Desktop/TestData/Collision Block/SamplePlots/%04d.png", floor((v[t,1]-5)*1000)), p, width=10, height=3)
}

# Stacked plot
p1 <- ggplot()+
  geom_line(aes(x=seq(0,100),y=unlist(v[999,seq(2,102)])))+
  labs(x=NULL, y=NULL)+
  ylim(-85,45)+
  ggtitle(sprintf("Time = %2.3f ms",v[999,1]-5))+
  theme(plot.title = element_text(size = 10, face="plain"))

p2 <- ggplot()+
  geom_line(aes(x=seq(0,100),y=unlist(v[1024,seq(2,102)])))+
  labs(x=NULL, y=NULL)+
  ylim(-85,45)+
  ggtitle(sprintf("Time = %2.3f ms",v[1024,1]-5))+
  theme(plot.title = element_text(size = 10, face="plain"))  

p3 <- ggplot()+
  geom_line(aes(x=seq(0,100),y=unlist(v[1159,seq(2,102)])))+
  labs(x=NULL, y=NULL)+
  ylim(-85,45)+
  ggtitle(sprintf("Time = %2.3f ms",v[1159,1]-5))+
  theme(plot.title = element_text(size = 10, face="plain"))

p4 <- ggplot()+
  geom_line(aes(x=seq(0,100),y=unlist(v[1199,seq(2,102)])))+
  labs(x=NULL, y=NULL)+
  ylim(-85,45)+
  ggtitle(sprintf("Time = %2.3f ms",v[1199,1]-5))+
  theme(plot.title = element_text(size = 10, face="plain"))

p5 <- ggplot()+
  geom_line(aes(x=seq(0,100),y=unlist(v[1299,seq(2,102)])))+
  labs(x=NULL, y=NULL)+
  ylim(-85,45)+
  ggtitle(sprintf("Time = %2.3f ms",v[1299,1]-5))+
  theme(plot.title = element_text(size = 10, face="plain"))

p <- grid.arrange(p1,p2,p3,p4,p5,ncol=1, 
                  left = textGrob("Membrane Potential (mV)", rot = 90, vjust = 1), 
                  bottom = textGrob("Node", vjust=0, hjust=0.2))

ggsave(filename = "C:/Users/Joey/Desktop/TestData/Collision Block/SamplePlots/stacked.tiff", p, width=8, height=8)
