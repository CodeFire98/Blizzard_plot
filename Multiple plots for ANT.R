library(ggplot2)
library(dplyr)
library(reshape2)
library(scales)
library(grid)
install.packages("grid")

maitri=Book1
maitri$tempr=maitri$DBT
maitri$ws=maitri$WS
maitri$mslp=maitri$MSLP
maitri$date=strptime(paste(maitri$YEAR,"-",maitri$MN,"-",maitri$DT," ",maitri$HR,":00:00"), "%Y - %m - %d %H :%M:%S")
subs=subset(maitri, maitri$MSLP<1033 & maitri$MSLP>940 & maitri$WS<60 & maitri$MSLP>940)
subs=subs[-c(1:18)]
#subs$obstime<-strptime(subs$date,format="%Y-%m-%d %H:%M:%S")
#subs=subs[-c(4)]
#k<-k[,c("obstime","tempr","ap","ws", "rh")]
#names(subs3)<-c("Year","Temperature","Wind Speed","Mean Sea Level Pressure")
#dt<-"FULL"
#names(gg)<-c("obstime","Temperature","Relative Humudity","Air Pressure","Wind Speed")

#write.csv(dat.m, file = "blah.csv")
#subs2 = subset(subs, as.character(as.Date(subs$date, "%Y-%m-%d")) >= "1995-01-01" & as.character(as.Date(subs$date, "%Y-%m-%d")) <= "2005-12-31")

subs$temprm = rollmean(subs$tempr, k=8, fill=NA)
subs$wsm = rollmean(subs$ws, k=8, fill=NA)
subs$mslpm = rollmean(subs$mslp, k=8, fill=NA)

subs3 = subs[,-c(1,2,3)]
subs3 = subset(subs3, format(subs2$date, "%H:%M:%S") == "12:00:00")

subs3$date=as.POSIXct(subs3$date) 
names(subs3)<-c("Year","Temperature","Wind Speed","Mean Sea Level Pressure")

line1tempr = paste("Temperature")
line1ws = paste("Wind Speed")
line1mslp = paste("Mean Sea Level Pressure")
line2 = paste("Dureation: 1985 - 2010")
line3 = paste("Dakshin Gangotri: 1985-1989 and Maitri: 1990-2010")


subtempr = subs3[,-c(3,4)]
subws = subs3[,-c(2,4)]
submslp = subs3[,-c(2,3)]

dat.m <- melt(subtempr, "Year")
ggplot(dat.m, aes(Year, value, colour = variable,group==1)) + geom_line() +
  facet_wrap(~ variable, ncol = 1, scales = "free_y")+
  scale_x_datetime(date_breaks = "1 year", labels = date_format("%Y")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1.0, hjust = 1.0)) +
  labs(title = "Temperature\nDuration: 1985-2010", subtitle = "Dakshin Gangotri: 1985-1989 and Maitri: 1990-2010") +
  theme(plot.title = element_text(family = "Aileron", color="red", face="bold", size=15, hjust=0)) +
  theme(plot.subtitle = element_text(family = "Aileron", color="green", face="bold", size=10, hjust=0)) +
  theme(axis.title = element_text(family = "Aileron", color="red", face="bold", size=12))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.background = element_rect(fill = "#f6f6f6",colour = "#f6f6f6",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "lightblue"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "white"))+
  theme(plot.background = element_rect(fill = "#fafafa"))+
  theme(strip.text.x = element_text(colour = "red", size = 15,hjust = 0.5, vjust = 0.5))

dat.m <- melt(subws, "Year")
ggplot(dat.m, aes(Year, value, colour = variable,group==1)) + geom_line() +
  facet_wrap(~ variable, ncol = 1, scales = "free_y")+
  #annotation_custom(grob = textGrob(line1tempr, just = "center", gp = gpar(fontsize = 12, fontface = "bold", colour = "red"))) +
  #annotation_custom(grob = textGrob(line2, just = "center", gp = gpar(fontsize = 12, fontface = "bold", colour = "green"))) +
  ggtitle("Wind Speed\nDuration: 1985-2010\nDakshin Gangotri: 1985-1989 and Maitri: 1990-2010")+
  theme(plot.title = element_text(family = "Aileron", color="red", face="bold", size=15, hjust=0)) +
  theme(axis.title = element_text(family = "Aileron", color="red", face="bold", size=12))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.background = element_rect(fill = "#f6f6f6",colour = "#f6f6f6",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "lightblue"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "white"))+
  theme(plot.background = element_rect(fill = "#fafafa"))+
  theme(strip.text.x = element_text(colour = "red", size = 15,hjust = 0.5, vjust = 0.5))

dat.m <- melt(submslp, "Year")
ggplot(dat.m, aes(Year, value, colour = variable,group==1)) + geom_line() +
  facet_wrap(~ variable, ncol = 1, scales = "free_y")+
  ggtitle("Mean Sea Level Pressure\nDuration: 1985-2010\nDakshin Gangotri: 1985-1989 and Maitri: 1990-2010")+
  theme(plot.title = element_text(family = "Aileron", color="red", face="bold", size=15, hjust=0)) +
  theme(axis.title = element_text(family = "Aileron", color="red", face="bold", size=12))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.background = element_rect(fill = "#f6f6f6",colour = "#f6f6f6",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "lightblue"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "white"))+
  theme(plot.background = element_rect(fill = "#fafafa"))+
  theme(strip.text.x = element_text(colour = "red", size = 15,hjust = 0.5, vjust = 0.5))

subs10 = subset(subtempr, format(subtempr$Year, "%Y") == "1987" | format(subtempr$Year, "%Y") == "1986")
qplot(subs10$Year, subs10$Temperature, geom = "line", xlab ="Time", ylab = "Temperature", 
      main = "Temperature vs Time") +
  scale_x_datetime(date_breaks = "1 month", labels = date_format("%m %Y %d")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1.0, hjust = 1.0))

