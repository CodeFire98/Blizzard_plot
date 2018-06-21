library(ggplot2)
library(reshape)
library(scales)
library(reshape2)

maitri = Final_File
#setwd("/root/R-project/data/polar data/Aws/IIG/Bharati")
#maitri<-read.csv("imd_maitri_2015.csv")
head(maitri)
maitri$obstime<-strptime(maitri$obstime,format="%d/%m/%Y %H:%M")
#maitri$obstime<-strptime(maitri$obstime,format="%m/%d/%Y %H:%M")

gg<-subset(maitri,obstime>='2012-01-01 00:00:00' & obstime<='2015-12-30 23:59:59')
gg<-gg[-c(5)]
# gg$obstime<-as.Date(gg$obstime,format="%Y-%m-%d")
# min<-min(gg$obstime)
# max<-max(gg$obstime)
gg<-gg[,c("obstime","tempr","ap","ws","rh")]
names(gg)<-c("obstime","Temperature","Air Pressure","Wind Speed","Relative Humidity")
dt<-"Full"
#names(gg)<-c("obstime","Temperature","Relative Humudity","Air Pressure","Wind Speed")
gg$obstime = as.POSIXct(gg$obstime)
gg <- melt(gg, "obstime")
jpeg(filename =paste("imd_bharati2",dt,".jpeg",sep = ""),width = 1200,height = 600)
ggplot(gg, aes(obstime, value, colour = variable,group==1)) + geom_line() +
  #scale_x_date(breaks = date_breaks("year"),labels = date_format("%Y"))+
  facet_wrap(~ variable, ncol = 1, scales = "free_y")+
  ggtitle("Bharati-AWS data Full")+
  theme(plot.title = element_text(family = "Trebuchet MS", color="red", face="bold", size=15, hjust=0)) +
  theme(axis.title = element_text(family = "Trebuchet MS", color="red", face="bold", size=12))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.background = element_rect(fill = "#f6f6f6",colour = "#f6f6f6",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "lightblue"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "white"))+
  theme(plot.background = element_rect(fill = "#fafafa"))+
  theme(strip.text.x = element_text(colour = "red", size = 15,hjust = 0.5, vjust = 0.5))
#theme(axis.text.x = element_text(angle = 90, hjust = 1))

dev.off()
