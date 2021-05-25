for (num in seq(10,30,by=.1)){
  plot <- ggplot()+
    geom_path(data = all2DF[all2DF$Time>num & all2DF$Time<num+.1,], aes(x=M47,y=H47, color=V47), size=1)+
    geom_path(data = all2DF[all2DF$Time>num & all2DF$Time<num+.1,], aes(x=M48,y=H48, color=V48), size=1)+
    geom_path(data = all2DF[all2DF$Time>num & all2DF$Time<num+.1,], aes(x=M49,y=H49, color=V49), size=1)+
    geom_path(data = all2DF[all2DF$Time>num & all2DF$Time<num+.1,], aes(x=M50,y=H50, color=V50), size=1)+
    geom_path(data = all2DF[all2DF$Time>num & all2DF$Time<num+.1,], aes(x=M51,y=H51, color=V51), size=1)+
    scale_colour_gradient2(low = "black", high = "red", mid="orange", midpoint = -50)+
    xlim(0,1)+
    ylim(0,1)+
    ggtitle(paste("H vs M", num, sep=" ", collapse = NULL))
  ggsave(filename = paste0("./phaseplane/",num*100,".png"), plot, width = 8, height = 6, dpi=150)
}
list.files(path = "./phaseplane/", pattern = "*.png", full.names = T) %>%
  map(image_read) %>%
  image_join() %>%
  image_animate(fps=5) %>%
  image_write("DCAmpTrial2.gif")