sine <- getDataAllDF("C:/Users/Joey/Desktop/Comparison Waveform/Data/Sine10kBlock/")
square <- getDataAllDF("C:/Users/Joey/Desktop/Comparison Waveform/Data/Square10kBlock/")
tri <- getDataAllDF("C:/Users/Joey/Desktop/Comparison Waveform/Data/Tri10kBlock/")

nodeNum <- seq(1,101, by=1)
print(nodeNum)


# Plots for 10kHz Sine wave voltage
BT1 <- getDataAllDF("D:/Raw Data/1xBTSine/")
BT3 <- getDataAllDF("D:/Raw Data/3xBTSine/")
HF <- getDataAllDF("C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/DCAmpComparison/trial0/")
DC1 <- getDataAllDF("C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/DCAmpComparison/trial2/")
DC2 <- getDataAllDF("C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/DCAmpCOmparison/trial11/")
p <- ggplot(data=BT3[BT3$Time>0 & BT3$Time<50,],)+
  geom_path(aes(x=Time,y=V51), color="#111111", size=2)+
  geom_hline(yintercept=-48, linetype="dashed", color = "red", size=1)+
    ylab("Average Voltage (mV)")+ylim(-120,40)+
    xlab("Time (ms)")+
    ggtitle("10kHz Voltage Center Node (3xBT)")+
    theme(plot.title = element_text(hjust = 0.5, size=18,face="bold"),axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"))

# voltage profile code!
t = 3900
x <- seq(1,101,by=1)  # create vector of node indices
vDataDC2 <- as.vector(unlist(DC2[t,x+203])) # vector of voltage data
x <- x-1

plot4 <- ggplot() + geom_path(aes(x=x,y=vDataHF), color="black", size=1)+
  geom_path(aes(x=x,y=vDataDC1), color="blue",size=1)+
  #geom_point(aes(x=x,y=vData), color="black",size=2)+
  geom_hline(yintercept=-48, linetype="dashed", color = "red", size=1)+
  xlab("Node Number")+ylab("Transmembrane Voltage (mV)")+
  xlim(0,100)+ylim(-105,30)+
  #ggtitle(sprintf("Voltage Profile @t=%f",HFAVG[t,1]))
  theme(plot.title = element_text(hjust = 0.5, size=18),axis.text=element_text(size=12),
        axis.title=element_text(size=14))

HFAVG <- movingAverageDF(HF, 10)
plot(HFAVG$Time,HFAVG$V51)
p <- ggplot(data=HFAVG[HFAVG$Time>0 & sineV$Time<30,],)+
  geom_path(aes(x=Time, y=Node51), color="#111111", size=2)+
  geom_hline(yintercept=-48, linetype="dashed", color = "red", size=1)+
  ylab("Average Voltage (mV)")+ylim(-120,40)+
  xlab("Time (ms)")+
  ggtitle("10kHz AVG Voltage Center Node (2xBT)")+
  theme(plot.title = element_text(hjust = 0.5, size=18,face="bold"),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
# siTrDiff <- sine

crowDC <- getDataAllDF("C:/Users/Joey/Desktop/TestData/CROW/No HF/")
p <- ggplot(data=crowDC[crowDC$Time>5 & crowDC$Time<50,],)+
  geom_path(aes(x=Time, y=H51), color="#111111", size=2)+
  #xlim(0,1)+xlab("M Gate")+
  ylim(0,1)+ylab("H Gate")+
  ggtitle("CROW DC H Gate Recovery")+
  theme(plot.title = element_text(hjust = 0.5))
p

ppms200 <- getDataAllDF("D:/Raw Data/dtaffect/ppms200/")
ppms300 <- getDataAllDF("D:/Raw Data/dtaffect/ppms300/")
ppms400 <- getDataAllDF("D:/Raw Data/dtaffect/ppms400/")

for (num in seq(40,50,by=.005)) {
  siTrDiff[num*200,"M51"] <- (siTrDiff[num*200,"M51"] - tri[num*200,"M51"])/siTrDiff[num*200,"M51"]
  siTrDiff[num*200,"M50"] <- (siTrDiff[num*200,"M50"] - tri[num*200,"M50"])/siTrDiff[num*200,"M50"]
  siTrDiff[num*200,"M49"] <- (siTrDiff[num*200,"M49"] - tri[num*200,"M49"])/siTrDiff[num*200,"M49"]
  siTrDiff[num*200,"M48"] <- (siTrDiff[num*200,"M48"] - tri[num*200,"M48"])/siTrDiff[num*200,"M48"]
  siTrDiff[num*200,"M47"] <- (siTrDiff[num*200,"M47"] - tri[num*200,"M47"])/siTrDiff[num*200,"M47"]
  siTrDiff[num*200,"M46"] <- (siTrDiff[num*200,"M46"] - tri[num*200,"M46"])/siTrDiff[num*200,"M46"]
  siTrDiff[num*200,"M45"] <- (siTrDiff[num*200,"M45"] - tri[num*200,"M45"])/siTrDiff[num*200,"M45"]
  siTrDiff[num*200,"M44"] <- (siTrDiff[num*200,"M44"] - tri[num*200,"M44"])/siTrDiff[num*200,"M44"]
  siTrDiff[num*200,"H51"] <- (siTrDiff[num*200,"H51"] - tri[num*200,"H51"])/siTrDiff[num*200,"H51"]
  siTrDiff[num*200,"H50"] <- (siTrDiff[num*200,"H50"] - tri[num*200,"H50"])/siTrDiff[num*200,"H50"]
  siTrDiff[num*200,"H49"] <- (siTrDiff[num*200,"H49"] - tri[num*200,"H49"])/siTrDiff[num*200,"H49"]
  siTrDiff[num*200,"H48"] <- (siTrDiff[num*200,"H48"] - tri[num*200,"H48"])/siTrDiff[num*200,"H48"]
  siTrDiff[num*200,"H47"] <- (siTrDiff[num*200,"H47"] - tri[num*200,"H47"])/siTrDiff[num*200,"H47"]
  siTrDiff[num*200,"H46"] <- (siTrDiff[num*200,"H46"] - tri[num*200,"H46"])/siTrDiff[num*200,"H46"]
  siTrDiff[num*200,"H45"] <- (siTrDiff[num*200,"H45"] - tri[num*200,"H45"])/siTrDiff[num*200,"H45"]
  siTrDiff[num*200,"H44"] <- (siTrDiff[num*200,"H44"] - tri[num*200,"H44"])/siTrDiff[num*200,"H44"]
  if((num%%.1)==0){
    print(num)
  }
}

registerDoParallel(1)
foreach(num=seq(55,56,by=.05), .packages = "ggplot2") %dopar%{
  plot <- ggplot(data=ppms400[ppms400$Time >num & ppms400$Time < num+.5,])+
    geom_path(aes(x=M51, y=H51), color="#00ff00", size=1)+  #green
    geom_path(aes(x=M50, y=H50), color="#ffff00", size=1)+  #yellow
    geom_path(aes(x=M49, y=H49), color="#ff9000", size=1)+  #orange
    geom_path(aes(x=M48, y=H48), color="#ff0000", size=1)+  #red
    geom_path(aes(x=M47, y=H47), color="#sff00b6", size=1)+  #pink
    geom_path(aes(x=M46, y=H46), color="#c700ff", size=1)+  #purple
    geom_path(aes(x=M45, y=H45), color="#0026ff", size=1)+  #blue
    geom_path(aes(x=M44, y=H44), color="#00faff", size=1)+  #cyan
    xlim(0,1)+xlab("M Gate")+
    ylim(0,1)+ylab("H Gate")+
    ggtitle(sprintf("Sine %.2f", num))
  ggsave(filename = sprintf("./phaseplane/%05d.png",num*100), plot, width = 5, height = 5, dpi=150)
}

for (num in seq(29.5,45,by=.05)){
  plot <- ggplot(data=DC2[DC2$Time >num-.1 & DC2$Time < num,])+
    geom_path(aes(x=M51, y=H51), color="#00ff00", size=1)+  #green
    geom_path(aes(x=M50, y=H50), color="#ffff00", size=1)+  #yellow
    geom_path(aes(x=M49, y=H49), color="#ff9000", size=1)+  #orange
    geom_path(aes(x=M48, y=H48), color="#ff0000", size=1)+  #red
    geom_path(aes(x=M47, y=H47), color="#ff00b6", size=1)+  #pink
    geom_path(aes(x=M46, y=H46), color="#c700ff", size=1)+  #purple
    geom_path(aes(x=M45, y=H45), color="#0026ff", size=1)+  #blue
    geom_path(aes(x=M44, y=H44), color="#00faff", size=1)+  #cyan
    xlim(0,1)+xlab("M Gate")+
    ylim(0,1)+ylab("H Gate")+
    ggtitle(sprintf("Sine %.2f", num))+
    theme(plot.title = element_text(hjust = 0.5))
  ggsave(filename = sprintf("./phaseplane/%05.0f.png",num*100), plot, width = 5, height = 5, dpi=150)
}

list.files(path = "./phaseplane/", pattern = "*.png", full.names = T) %>%
  map(image_read) %>%
  image_join() %>%
  image_animate(fps=10) %>%
  image_write("ppms200.gif")


derivSine <- slopeDF(sineDF, c("M51", "M50", "M49", "M48", "M47", "M46", "M45", "H51", "H50", "H49", "H48", "H47", "H46", "H45"))


ggplot(data=derivSine[derivSine$Time >num & derivSine$Time < num+.1,])+
  geom_path(aes(x=Time, y=H51), color="green", size=1)


ggplot(data=all8DF[all8DF$Time > 36 & all8DF$Time <55,])+
  geom_path(aes(x=M51, y=H51), color="#FF0000", size=1)+#RED
  geom_path(aes(x=M50, y=H50), color="#FF8000", size=1)+#orange 
  geom_path(aes(x=M49, y=H49), color="#FFFF00", size=1)+#yellow
  geom_path(aes(x=M48, y=H48), color="#00FF00", size=1)+#green
  geom_path(aes(x=M47, y=H47), color="#00FFFF", size=1)+#light blue
  geom_path(aes(x=M46, y=H46), color="#0000FF", size=1)+#blue
  geom_path(aes(x=M45, y=H45), color="#FF00FF", size=1)+#purple pink
  geom_path(aes(x=M44, y=H44), color="#FF007F", size=1)+#pink
  geom_path(aes(x=M43, y=H43), color="#FF0000", size=1)+#red
  geom_path(aes(x=M42, y=H42), color="#FF8000", size=1)+#orange 
  geom_path(aes(x=M41, y=H41), color="#FFFF00", size=1)+#yellow
  geom_path(aes(x=M40, y=H40), color="#00FF00", size=1)+#green
  geom_path(aes(x=M39, y=H39), color="#00FFFF", size=1)+#light blue
  geom_path(aes(x=M38, y=H38), color="#0000FF", size=1)+#blue
  geom_path(aes(x=M37, y=H37), color="#FF00FF", size=1)+#purple pink
  geom_path(aes(x=M36, y=H36), color="#FF007F", size=1)+#pink
  geom_path(aes(x=M35, y=H35), color="#FF0000", size=1)+#red
  xlim(0,1)+
  ylim(0,1)+
  ggtitle(paste("CROW Trial 8 H vs M", num, sep=" ", collapse = NULL))

sineDF <- getDataAllDF("C:/Users/Joey/Desktop/Comparison Waveform/Data/Sine10kBlock/")







all0DF <- getDataAllDF("C:/Users/Joey/Desktop/TestData/CROW/Best Trials CROW/DCAmpComparison/trial0/")
for (num in seq(10,70,by=.05)){
  # if (num < 30){
    plot <- ggplot(data=ppms300[ppms300$Time>num & ppms300$Time < num+.15,])+
      # This is meant to give a rainbow similar order to ROYGBIV with 99% accessibility
      # see https://sashat.me/2017/01/11/list-of-20-simple-distinct-colors/ for more info
      geom_point(aes(x=M46, y=H46), color="#E6194B", size=1)+# Red
      geom_point(aes(x=M47, y=H47), color="#A00000", size=1)+# Maroon
      geom_point(aes(x=M48, y=H48), color="#F58231", size=1)+# Orange
      geom_point(aes(x=M49, y=H49), color="#9A6324", size=1)+# Brown
      geom_point(aes(x=M50, y=H50), color="#FFE119", size=1)+# Yellow
      geom_point(aes(x=M51, y=H51), color="#3CB44B", size=1)+# Green
      geom_point(aes(x=M52, y=H52), color="#469990", size=1)+# Teal
      geom_point(aes(x=M53, y=H53), color="#42D4f4", size=1)+# Cyan
      geom_point(aes(x=M54, y=H54), color="#000075", size=1)+# Navy
      geom_point(aes(x=M55, y=H55), color="#4363D8", size=1)+# Blue
      geom_point(aes(x=M56, y=H56), color="#F032E6", size=1)+# Magenta
      xlim(0,1)+xlab("M Gate")+
      ylim(0,1)+ylab("H Gate")+
      ggtitle(sprintf("Sine %.2f", num))+
      theme(plot.title = element_text(hjust = 0.5))
   
  # } else {
    # plot <- ggplot(data=allBlock1DF[allBlock1DF$Time>num & allBlock1DF$Time < num+.1,])+
    #   geom_path(aes(x=M51, y=H51), color="#FF0000", size=1)+#RED
    #   geom_path(aes(x=M50, y=H50), color="#FF8000", size=1)+#orange 
    #   geom_path(aes(x=M49, y=H49), color="#FFFF00", size=1)+#yellow
    #   geom_path(aes(x=M48, y=H48), color="#00FF00", size=1)+#green
    #   geom_path(aes(x=M47, y=H47), color="#00FFFF", size=1)+#light blue
    #   geom_path(aes(x=M46, y=H46), color="#0000FF", size=1)+#blue
    #   geom_path(aes(x=M45, y=H45), color="#FF00FF", size=1)+#purple pink
    #   geom_path(aes(x=M44, y=H44), color="#FF007F", size=1)+#pink
    #   geom_path(aes(x=M43, y=H43), color="#FF0000", size=1)+#red
    #   geom_path(aes(x=M42, y=H42), color="#FF8000", size=1)+#orange 
    #   geom_path(aes(x=M41, y=H41), color="#FFFF00", size=1)+#yellow
    #   geom_path(aes(x=M40, y=H40), color="#00FF00", size=1)+#green
    #   geom_path(aes(x=M39, y=H39), color="#00FFFF", size=1)+#light blue
    #   geom_path(aes(x=M38, y=H38), color="#0000FF", size=1)+#blue
    #   geom_path(aes(x=M37, y=H37), color="#FF00FF", size=1)+#purple pink
    #   geom_path(aes(x=M36, y=H36), color="#FF007F", size=1)+#pink
    #   geom_path(aes(x=M35, y=H35), color="#FF0000", size=1)+#red
    #   geom_path(aes(x=M34, y=H34), color="#FF8000", size=1)+#orange 
    #   geom_path(aes(x=M33, y=H33), color="#FFFF00", size=1)+#yellow
    #   geom_path(aes(x=M32, y=H32), color="#00FF00", size=1)+#green
    #   geom_path(aes(x=M31, y=H31), color="#00FFFF", size=1)+#light blue
    #   geom_path(aes(x=M30, y=H30), color="#0000FF", size=1)+#blue
    #   geom_path(aes(x=M29, y=H29), color="#FF00FF", size=1)+#purple pink
    #   geom_path(aes(x=M28, y=H28), color="#FF007F", size=1)+#pink
    #   xlim(0,1)+xlab("M Gate")+
    #   ylim(0,1)+ylab("H Gate")+
    #   ggtitle(sprintf("Sine %.2f", num))+
    #   theme(plot.title = element_text(hjust = 0.5))
     ggsave(filename = sprintf("./phaseplane/%05.0f.png",num*100), plot, width = 5, height = 5, dpi=150)
  # }
 # ggsave(filename = paste0("./phaseplane/",num*100,".png"), plot, width = 8, height = 6, dpi=150)
}



# sample code for comparing initial conditions of HF and CROW, and then plot the standard 
# steady-state of the HF
HF <- getDataAllDF("E:/Raw Data/Sample/10kBlock/")
CROW <- getDataAllDF("E:/Raw Data/Sample/10kCROW/")

ggplot(data=HF[HF$Time>5 & HF$Time<10.3,],)+
  geom_path(aes(x=M51, y=H51), color="#0000FF", size=1)+
  geom_path(aes(x=M50, y=H50), color="#8800FF", size=1)+
  geom_path(aes(x=M49, y=H49), color="#FF0000", size=1)+
  geom_path(aes(x=M48, y=H48), color="orange", size=1)+
  xlim(0,1)+xlab("M Gate")+
  ylim(0,1)+ylab("H Gate")+
  ggtitle("Without CROW")+
  theme(plot.title = element_text(hjust = 0.5, size=18),axis.text=element_text(size=12),
        axis.title=element_text(size=14))

ggplot(data=CROW[CROW$Time>30 & CROW$Time<30.3,],)+
  geom_path(aes(x=M51, y=H51), color="#0000FF", size=1)+
  geom_path(aes(x=M50, y=H50), color="#8800FF", size=1)+
  geom_path(aes(x=M49, y=H49), color="#FF0000", size=1)+
  geom_path(aes(x=M48, y=H48), color="orange", size=1)+
  xlim(0,1)+xlab("M Gate")+
  ylim(0,1)+ylab("H Gate")+
  ggtitle("CROW")+
  theme(plot.title = element_text(hjust = 0.5, size=18),axis.text=element_text(size=12),
        axis.title=element_text(size=14))

ggplot(data=HF[HF$Time>95 & HF$Time<95.3,],)+
  geom_path(aes(x=M51, y=H51), color="#0000FF", size=1)+
  geom_path(aes(x=M50, y=H50), color="#8800FF", size=1)+
  geom_path(aes(x=M49, y=H49), color="#FF0000", size=1)+
  geom_path(aes(x=M48, y=H48), color="orange", size=1)+
  xlim(0,1)+xlab("M Gate")+
  ylim(0,1)+ylab("H Gate")+
  ggtitle("10kHz Steady State")+
  theme(plot.title = element_text(hjust = 0.5, size=18),axis.text=element_text(size=12),
        axis.title=element_text(size=14))

