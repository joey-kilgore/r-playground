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

# Load data
source("UsefulFunctions.R")
ko <- csvToDF(read.csv("C:/Users/Joey/Desktop/TestData/kaccumulation/CathodicDC/1xBT/KO.csv"))
v <- csvToDF(read.csv("C:/Users/Joey/Desktop/TestData/kaccumulation/CathodicDC/1xBT/Voltage.csv"))

ko15x <- csvToDF(read.csv("L:/Work/data/Carryover/trials/1_5xBT_1500ms/KO.csv"))
v15x <- csvToDF(read.csv("L:/Work/data/Carryover/trials/1_5xBT_1500ms/Voltage.csv"))
ko1x <- csvToDF(read.csv("L:/Work/data/Carryover/trials/1xBT_1500ms/KO.csv"))
v1x <- csvToDF(read.csv("L:/Work/data/Carryover/trials/1xBT_1500ms/Voltage.csv"))
koInc <- csvToDF(read.csv("L:/Work/data/Carryover/trials/1xBT_1500ms_5K/KO.csv"))
vInc <- csvToDF(read.csv("L:/Work/data/Carryover/trials/1xBT_1500ms_5K/Voltage.csv"))
# check data set
head(koInc)
plot(koInc$Time, koInc$Node548)

# load graphing libraries
library(ggplot2)
library(grid)
library(gridExtra)
library(RColorBrewer)

# -------------------------------------------------------
# Potassium accumulation for set of adjacent compartments
# -------------------------------------------------------
print(calcCol("Node",10))
ggplot(data=ko)+
  geom_path(aes(x=Time,y=ko[,227]),color="green")+
  geom_path(aes(x=Time,y=ko[,226]),color="cyan")+
  geom_path(aes(x=Time,y=ko[,225]),color="blue")+
  geom_path(aes(x=Time,y=ko[,224]),color="purple")+
  geom_path(aes(x=Time,y=ko[,223]),color="red")

print(calcCol("Node",50))
ggplot(data=ko)+
  geom_path(aes(x=Time,y=ko[,557]),color="green")+
  geom_path(aes(x=Time,y=ko[,556]),color="cyan")+
  geom_path(aes(x=Time,y=ko[,555]),color="blue")+
  geom_path(aes(x=Time,y=ko[,554]),color="purple")+
  geom_path(aes(x=Time,y=ko[,553]),color="red")

print(calcCol("Node",90))
ggplot(data=ko)+
  geom_path(aes(x=Time,y=ko[,997]),color="green")+
  geom_path(aes(x=Time,y=ko[,996]),color="cyan")+
  geom_path(aes(x=Time,y=ko[,995]),color="blue")+
  geom_path(aes(x=Time,y=ko[,994]),color="purple")+
  geom_path(aes(x=Time,y=ko[,993]),color="red")

# -----------------------------
# looking at MS and PS segments
# -----------------------------
calcCol("Node",10)
calcCol("PS",20)
calcCol("MS",20)
pm1 <- ggplot()+
  geom_abline(slope=1,color="#000000", linetype="dashed", size=1)+
  geom_path(aes(x=ko[,calcCol("PS",180)],ko[,calcCol("MS",180)]),color="blue")+
  xlab("PS [K]o (mM)")+
  ylab("MS [K]o (mM)")+
  ggtitle("Left Node 90")
  
pm2 <- ggplot()+
  geom_abline(slope=1,color="#000000", linetype="dashed", size=1)+
  geom_path(aes(x=ko[,calcCol("PS",100)],ko[,calcCol("MS",100)]), color="purple")+
  xlab("PS [K]o (mM)")+
  ylab("MS [K]o (mM)")+
  ggtitle("Left Node 50")

pm3 <- ggplot()+
  geom_abline(slope=1,color="#000000", linetype="dashed", size=1)+
    geom_path(aes(x=ko[,calcCol("PS",20)],ko[,calcCol("MS",20)]), color="red")+
  xlab("PS [K]o (mM)")+
  ylab("MS [K]o (mM)")+
  ggtitle("Left Node 10")

ps1 <- ggplot()+
  geom_path(aes(x=ko$Time,y=ko[,calcCol("PS",180)]),color="blue")+
  geom_path(aes(x=ko$Time,y=ko[,calcCol("PS",100)]),color="purple")+
  geom_path(aes(x=ko$Time,y=ko[,calcCol("PS",20)]),color="red")+
  ylab("PS [K]o (mM)")+
  xlab("")

ms1 <- ggplot()+
  geom_path(aes(x=ko$Time,y=ko[,calcCol("MS",180)]),color="blue")+
  geom_path(aes(x=ko$Time,y=ko[,calcCol("MS",100)]),color="purple")+
  geom_path(aes(x=ko$Time,y=ko[,calcCol("MS",20)]),color="red")+
  ylab("MS [K]o (mM)")+
  xlab("Time")

lay <- cbind(c(3,4,5),
             c(2,4,5),
             c(1,4,5))

p <- grid.arrange(pm1,pm2,pm3,ps1,ms1,layout_matrix=lay)
ggsave("C:/Users/Joey/Desktop/TestData/kaccumulation/CathodicDC/plots/psms1xBT.jpg", p, width=10, height=8)

# -------------------
# Basic Voltage plots
# -------------------
v10 <- ggplot()+
    geom_path(aes(x=v$Time,y=v$Node10))+
    xlab("")+
    ylab("Node 10 (mV)")

v51 <- ggplot()+
  geom_path(aes(x=v$Time,y=v$Node51))+
  xlab("")+
  ylab("Center Node (mV)")

v90 <- ggplot()+
  geom_path(aes(x=v$Time,y=v$Node90))+
  xlab("Time (ms)")+
  ylab("Node 90 (mV)")

vAll <- grid.arrange(v10,v51,v90, layout_matrix=cbind(c(1,2,3),c(1,2,3),c(1,2,3)))
ggsave("C:/Users/Joey/Desktop/TestData/kaccumulation/CathodicDC/plots/voltage1xBT.jpg", vAll, width=10, height=8)

# ----------------------------------
# large rainow of values across time
# ----------------------------------
psRainbow <- ggplot()+
  geom_path(aes(x=ko$Time,y=ko[,calcCol("PS",89)]),color="orange")+
  geom_path(aes(x=ko$Time,y=ko[,calcCol("PS",91)]),color="#ffa3f1")+
  geom_path(aes(x=ko$Time,y=ko[,calcCol("PS",93)]),color="red")+
  geom_path(aes(x=ko$Time,y=ko[,calcCol("PS",95)]),color="purple")+
  geom_path(aes(x=ko$Time,y=ko[,calcCol("PS",97)]),color="blue")+
  geom_path(aes(x=ko$Time,y=ko[,calcCol("PS",99)]),color="cyan")+
  geom_path(aes(x=ko$Time,y=ko[,calcCol("PS",101)]),color="green")+
  geom_path(aes(x=ko$Time,y=ko[,calcCol("PS",151)]),color="black")+
  ylab("PS [K]o (mM)")+
  xlab("")+
  xlim(549,650)
  ylim(3,6)
show(psRainbow)

vRainbow <- ggplot()+
  geom_path(aes(x=v$Time,y=v$Node45),color="orange")+
  geom_path(aes(x=v$Time,y=v$Node46),color="#ffa3f1")+
  geom_path(aes(x=v$Time,y=v$Node47),color="red")+
  geom_path(aes(x=v$Time,y=v$Node48),color="purple")+
  geom_path(aes(x=v$Time,y=v$Node49),color="blue")+
  geom_path(aes(x=v$Time,y=v$Node50),color="cyan")+
  geom_path(aes(x=v$Time,y=v$Node51),color="green")+
  geom_path(aes(x=v$Time,y=v$Node75),color="black")+
  xlab("Time (ms)")+
  ylab("Node Voltage (mV)")+
  xlim(549,650)
show(vRainbow)

gridRainbow <- grid.arrange(psRainbow,vRainbow, ncol=1)

ggsave("C:/Users/Joey/Desktop/TestData/kaccumulation/CathodicDC/plots/PSVoltageCarryover.jpg", gridRainbow, width=12, height=8)

# -------------------------------------------------
# compare node voltage to PS potassium accumulation
# -------------------------------------------------
plot(ko[,calcCol("PS",100)],v$Node51)

kNew <- ko[ko[,1]<550,]
plot(kNew[,1],kNew[,calcCol("Node",50)])

vNew <- v[v$Time<550,]
plot(vNew$Time,vNew$Node51)

vAndK <- ggplot()+
  geom_path(aes(x=kNew[,calcCol("PS",89)],y=vNew$Node45),color="orange")+
  geom_path(aes(x=kNew[,calcCol("PS",91)],y=vNew$Node46),color="#ffa3f1")+
  geom_path(aes(x=kNew[,calcCol("PS",93)],y=vNew$Node47),color="red")+
  geom_path(aes(x=kNew[,calcCol("PS",95)],y=vNew$Node48),color="purple")+
  geom_path(aes(x=kNew[,calcCol("PS",97)],y=vNew$Node49),color="blue")+
  geom_path(aes(x=kNew[,calcCol("PS",99)],y=vNew$Node50),color="cyan")+
  geom_path(aes(x=kNew[,calcCol("PS",101)],y=vNew$Node51),color="green")+
  geom_path(aes(x=kNew[,calcCol("PS",151)],y=vNew$Node75),color="black")+
  xlab("K Accumulation (mM)")+
  ylab("Voltage (mV)")

vAndK <- ggplot()+
  geom_path(aes(x=ko[,calcCol("PS",89)],y=vNew$Node45),color="orange")+
  geom_path(aes(x=ko[,calcCol("PS",91)],y=vNew$Node46),color="#ffa3f1")+
  geom_path(aes(x=ko[,calcCol("PS",93)],y=vNew$Node47),color="red")+
  geom_path(aes(x=ko[,calcCol("PS",95)],y=vNew$Node48),color="purple")+
  geom_path(aes(x=ko[,calcCol("PS",97)],y=vNew$Node49),color="blue")+
  geom_path(aes(x=ko[,calcCol("PS",99)],y=vNew$Node50),color="cyan")+
  geom_path(aes(x=ko[,calcCol("PS",101)],y=vNew$Node51),color="green")+
  geom_path(aes(x=ko[,calcCol("PS",151)],y=vNew$Node75),color="black")+
  xlab("K Accumulation (mM)")+
  ylab("Voltage (mV)")

ggsave("C:/Users/Joey/Desktop/TestData/kaccumulation/CathodicDC/plots/VandPSBlock.jpg", vAndK, width=12, height=8)

# ------------------------------------
# saving voltage profile to make a gif
# ------------------------------------
v <- vInc
k <- koInc
x <- seq(2,102)
xZoom <- seq(47,57)
kx <- seq(2,1111)
kxZoom <- seq(502,612)
vNew <- v[,x]
vZoom <- v[,xZoom]
kNew <- k[,kx]
kZoom <- k[,kxZoom]
timeStep <- 23500
for(timeStep in seq(200,23500,by=10)){ #max 18000
    vPlot <- ggplot()+geom_path(aes(x=x-2, y=unname(unlist(vNew[timeStep,]))))+
      geom_point(aes(x=x-2, y=unname(unlist(vNew[timeStep,]))))+
      xlab("Node")+
      ylab("Voltage (mV)")+
      ylim(-80,40)+
      ggtitle(sprintf("%4.4f",v[timeStep,1]))
    
    vZoomPlot <- ggplot()+geom_path(aes(x=xZoom-2, y=unname(unlist(vZoom[timeStep,]))))+
      geom_point(aes(x=xZoom-2, y=unname(unlist(vZoom[timeStep,]))))+
      xlab("Node")+
      ylab("Voltage (mV)")+
      xlim(45,55)+
      ylim(-80,40)+
      ggtitle(sprintf("%4.4f",v[timeStep,1]))
    
    kPlot <- ggplot()+geom_path(aes(x=(kx-7)/11, y=unname(unlist(kNew[timeStep,]))))+
      xlab("Node")+
      ylab("K (mM)")+
      ylim(0,30)
    
    kZoomPlot <- ggplot()+geom_path(aes(x=(kxZoom-7)/11, y=unname(unlist(kZoom[timeStep,]))))+
      geom_point(aes(x=(kxZoom-7)/11, y=unname(unlist(kZoom[timeStep,]))))+
      xlab("Node")+
      ylab("K (mM)")+
      xlim(45,55)+
      ylim(0,30)
    
    combinedPlot <- grid.arrange(vPlot,vZoomPlot,kPlot,kZoomPlot,ncol=2)
    
    ggsave(paste("C:/Users/Joey Kilgore/Desktop/outputGraphs/",sprintf("%05d",timeStep),".png", sep=""),combinedPlot,width=16,height=10)
}
# ------------------------------------
# Comparing two DC Carryvover profiles
# ------------------------------------
ko1x <- csvToDF(read.csv("L:/Work/data/Carryover/trials/1xBT_1500ms/KO.csv"))
v1x <- csvToDF(read.csv("L:/Work/data/Carryover/trials/1xBT_1500ms/Voltage.csv"))
x <- seq(2,102)
kx <- seq(2,1111)
v1 <- v1x[,x]
v2 <- vInc[,x]
k1 <- ko1x[,kx]
k2 <- koInc[,kx]
timeStep <- 19990
for(timeStep in seq(200,19990,by=10)){ #max 18000
  vPlot1 <- ggplot()+geom_path(aes(x=x-2, y=unname(unlist(v1[timeStep,]))))+
    geom_point(aes(x=x-2, y=unname(unlist(v1[timeStep,]))))+
    xlab("Node")+
    ylab("Voltage (mV)")+
    ylim(-80,40)+
    ggtitle(sprintf("%4.4f 1xBT 1500ms",v1x[timeStep,1]))
  
  vPlot2 <- ggplot()+geom_path(aes(x=x-2, y=unname(unlist(v2[timeStep,]))))+
    geom_point(aes(x=x-2, y=unname(unlist(v2[timeStep,]))))+
    xlab("Node")+
    ylab("Voltage (mV)")+
    ylim(-80,40)+
    ggtitle(sprintf("%4.4f 1xBT 1500ms 5%% K+ Increase",vInc[timeStep,1]))
  
  kPlot1 <- ggplot()+geom_path(aes(x=(kx-7)/11, y=unname(unlist(k1[timeStep,]))))+
    xlab("Node")+
    ylab("K (mM)")+
    ylim(0,30)
  
  kPlot2 <- ggplot()+geom_path(aes(x=(kx-7)/11, y=unname(unlist(k2[timeStep,]))))+
    xlab("Node")+
    ylab("K (mM)")+
    ylim(0,30)
  
  combinedPlot <- grid.arrange(vPlot1,vPlot2,kPlot1,kPlot2,ncol=2)
  
  ggsave(paste("C:/Users/Joey Kilgore/Desktop/outputGraphs/",sprintf("%05d",timeStep),".png", sep=""),combinedPlot,width=16,height=10)
}
# -------------------------------------
# Compare two trials voltages over time
# -------------------------------------
ggplot() + geom_path(aes(x=v1x$Time, y=v1x$Node51),color="#FF0000")+
  geom_path(aes(x=v15x$Time, y=v15x$Node51),color="#0000FF")+
  geom_path(aes(x=v1x$Time, y=v1x$Node21),color="#FF0000")+
  geom_path(aes(x=v15x$Time, y=v15x$Node21),color="#0000FF")+
  geom_path(aes(x=v1x$Time, y=v1x$Node81),color="#FF0000")+
  geom_path(aes(x=v15x$Time, y=v15x$Node81),color="#0000FF")+
  geom_hline(yintercept = -53, color = "black", linetype="dashed")+
  xlim(1625,1725)+
  xlab("Time (ms)")+
  ylab("Voltage (mV)")+
  ggtitle("Comparing 1xBT (RED) and 1.5xBT (BLUE) at nodes 20, 50, 80")

# --------------------
# sodium channel plots
# --------------------
all1xBT <- getDataAllDF("L:/Work/data/Carryover/trials/1xBT_1500ms/")
all15xBT <- getDataAllDF("L:/Work/data/Carryover/trials/1_5xBT_1500ms/")

timeStep <- 1660
for(timeStep in seq(1501,1740,by=2)){ #max 18000
  all1xBTtime <- all1xBT[all1xBT$Time>timeStep & all1xBT$Time<(timeStep+10),]
  all15xBTtime <- all15xBT[all15xBT$Time>timeStep & all15xBT$Time<(timeStep+10),]
  
  mh201 <- ggplot()+geom_path(aes(x=all1xBTtime$M21, y=all1xBTtime$H21),size=1)+
    geom_point(aes(x=all1xBTtime$M21, y=all1xBTtime$H21),size=1)+
    xlim(0,1)+
    ylim(0,1)+
    xlab("M20")+
    ylab("H20")
  
  mh451 <- ggplot()+geom_path(aes(x=all1xBTtime$M46, y=all1xBTtime$H46),size=1)+
    geom_point(aes(x=all1xBTtime$M46, y=all1xBTtime$H46),size=1)+
    xlim(0,1)+
    ylim(0,1)+
    xlab("M45")+
    ylab("H45")
  
  mh501 <- ggplot()+geom_path(aes(x=all1xBTtime$M51, y=all1xBTtime$H51),size=1)+
    geom_point(aes(x=all1xBTtime$M51, y=all1xBTtime$H51),size=1)+
    xlim(0,1)+
    ylim(0,1)+
    xlab("M50")+
    ylab("H50")
  
  mh551 <- ggplot()+geom_path(aes(x=all1xBTtime$M56, y=all1xBTtime$H56),size=1)+
    geom_point(aes(x=all1xBTtime$M56, y=all1xBTtime$H56),size=1)+
    xlim(0,1)+
    ylim(0,1)+
    xlab("M55")+
    ylab("H55")
  
  mh801 <- ggplot()+geom_path(aes(x=all1xBTtime$M81, y=all1xBTtime$H81),size=1)+
    geom_point(aes(x=all1xBTtime$M81, y=all1xBTtime$H81),size=1)+
    xlim(0,1)+
    ylim(0,1)+
    xlab("M80")+
    ylab("H80")
  
  # plots for second dataset (1.5xBT)
  mh202 <- ggplot()+geom_path(aes(x=all15xBTtime$M21, y=all15xBTtime$H21),size=1)+
    geom_point(aes(x=all15xBTtime$M21, y=all15xBTtime$H21),size=1)+
    xlim(0,1)+
    ylim(0,1)+
    xlab("M20")+
    ylab("H20")
  
  mh452 <- ggplot()+geom_path(aes(x=all15xBTtime$M46, y=all15xBTtime$H46),size=1)+
    geom_point(aes(x=all15xBTtime$M46, y=all15xBTtime$H46),size=1)+
    xlim(0,1)+
    ylim(0,1)+
    xlab("M45")+
    ylab("H45")
  
  mh502 <- ggplot()+geom_path(aes(x=all15xBTtime$M51, y=all15xBTtime$H51),size=1)+
    geom_point(aes(x=all15xBTtime$M51, y=all15xBTtime$H51),size=1)+
    xlim(0,1)+
    ylim(0,1)+
    xlab("M50")+
    ylab("H50")
  
  mh552 <- ggplot()+geom_path(aes(x=all15xBTtime$M56, y=all15xBTtime$H56),size=1)+
    geom_point(aes(x=all15xBTtime$M56, y=all15xBTtime$H56),size=1)+
    xlim(0,1)+
    ylim(0,1)+
    xlab("M55")+
    ylab("H55")
  
  mh802 <- ggplot()+geom_path(aes(x=all15xBTtime$M81, y=all15xBTtime$H81),size=1)+
    geom_point(aes(x=all15xBTtime$M81, y=all15xBTtime$H81),size=1)+
    xlim(0,1)+
    ylim(0,1)+
    xlab("M80")+
    ylab("H80")
  
  combinedPlot <- grid.arrange(mh201,mh451,mh501,mh551,mh801,
                               mh202,mh452,mh502,mh552,mh802,ncol=5)
  
  
  ggsave(paste("C:/Users/Joey Kilgore/Desktop/outputGraphs/",sprintf("%05d",timeStep),".png", sep=""),combinedPlot,width=16,height=10)
}
# ---------------------------------------------------------------
# looking at pulse trains with 3.0mM potassium bath or 3.3mM bath
# ---------------------------------------------------------------
train30 <- csvToDF(read.csv("C:/Users/Joey/Desktop/TestData/kaccumulation/APTrain/K_3_0mM/KO.csv"))
train30v <- csvToDF(read.csv("C:/Users/Joey/Desktop/TestData/kaccumulation/APTrain/K_3_0mM/Voltage.csv"))
train33 <- csvToDF(read.csv("C:/Users/Joey/Desktop/TestData/kaccumulation/APTrain/K_3_3mM/KO.csv"))
train33v <- csvToDF(read.csv("C:/Users/Joey/Desktop/TestData/kaccumulation/APTrain/K_3_3mM/Voltage.csv"))

p <- ggplot()+
  geom_path(aes(x=train30$Time,y=train30[,calcCol("PS",101)], color="red"))+
  geom_path(aes(x=train33$Time,y=train33[,calcCol("PS",101)], color="blue"))+
  xlab("Time (ms)")+ ylab("PS K+ Concentration (mM)")+
  scale_color_identity(name = "Concentration",
                       breaks = c("blue","red"),
                       labels = c("+10%", "Normal"),
                       guide = "legend")+
  theme(legend.position = c(0.9,0.25))

v <- ggplot()+
  geom_path(aes(x=train30v$Time,y=train30v$Node51), color="red")+
  geom_path(aes(x=train33v$Time,y=train33v$Node51), color="blue")+
  xlab("Time (ms)")+ ylab("Node Voltage (mV)")+
  scale_color_identity(name = "Concentration",
                       breaks = c("blue","red"),
                       labels = c("+10%", "Normal"),
                       guide = "legend")+
  theme(legend.position = c(0.9,0.25))

trains <- grid.arrange(p,v,ncol=1, top=textGrob("Pulse Trains and Potassium Accumulation",gp=gpar(fontsize=20,font=1)))

ggsave("C:/Users/Joey/Desktop/TestData/kaccumulation/CathodicDC/plots/PulseTrains.jpg", trains, width=10, height=5)

# ------------------
# longitudinal plots
# ------------------
length(ko[1,])
xPos <- seq(from=2,to=1111,by=1)
length(xPos)
plot(xPos,ko[10000,xPos])
koVec <- unlist(ko[1,xPos])
plot(xPos,koVec)
vPos <- seq(from=2,to=101,by=1)

timeStep <- 8960
for(timeStep in seq(from=8950,to=9100,by=10)){
  koLong <- ggplot()+
    geom_point(aes(x=xPos,y=unlist(ko[timeStep,xPos])))+
    geom_path(aes(x=xPos,y=unlist(ko[timeStep,xPos])))+
    xlim(450,600)+
    ylab("K Concentration (mM)")+
    xlab("")

  vLong <- ggplot()+
    geom_point(aes(x=vPos,y=unlist(v[timeStep,vPos])))+
    geom_path(aes(x=vPos,y=unlist(v[timeStep,vPos])))+
    ylim(-80,20)+
    xlim(35,56)+
    ylab("Voltage (mV)")+
    xlab("")

  longCombined <- grid.arrange(koLong, vLong, ncol=1, top=textGrob(paste("t=",ko[timeStep,1],sep=""),gp=gpar(fontsize=15,font=1)))
  
  ggsave(paste("./kaccumulation/",sprintf("%05d",timeStep),".png", sep=""),longCombined,width=16,height=10)
}

# -------------------------------
# looking at PS voltage over time
# -------------------------------
ko30 <- csvToDF(read.csv("C:/Users/Joey/Desktop/TestData/kaccumulation/APTrain/K_3_0mM/KO.csv"))
ps30 <- csvToDF(read.csv("C:/Users/Joey/Desktop/TestData/kaccumulation/APTrain/K_3_0mM/FLUT.csv"))
s <- 0.2
offset <- 80
ggplot()+geom_line(aes(x=ko30$Time,y=ko30$Node554))+
  geom_line(aes(x=ps30$Time,y=(ps30$Node100+offset)*s))+
  scale_y_continuous(
    name="Potassium Concentration",
    sec.axis = sec_axis(~.+offset.*s,name="Voltage")
  )

# -------------------------------------------------
# Looking at all compartment potassium accumulation
# -------------------------------------------------
plots <- vector(mode = "list", length = 110)

make_plots = function(index){
  ggplot()+geom_path(aes(x=ko1x$Time, y=ko1x[,index+501]), color="#FF0000")+
    geom_path(aes(x=ko15x$Time, y=ko15x[,index+501]), color="#0000FF")+
    xlim(1500,1750)+
    ylim(0,30)+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
}
plots <- lapply(seq(1,110), make_plots)

combinedPlot <- grid.arrange(grobs =plots,ncol=11)
ggsave("C:/Users/Joey Kilgore/Desktop/outputGraphs/ko.png",combinedPlot,width = 40,height =36)

