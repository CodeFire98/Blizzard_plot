library(ggplot2)
library(scales)
library(reshape)
library(dplyr)

############################ 

#convert to blizzard-friendly file

mytable = imd_maitri  #copy the original

mytable$bliz = 0
for (i in 1:nrow(mytable)) {
  if(is.na(mytable$ws[i])==1) {} 
  else if(mytable$ws[i] > 16.2) {
    mytable$bliz[i] = 1
    }
}

mytable$bliz1 = mytable$bliz
for (i in 2:nrow(mytable)) {
  if((mytable$bliz[i] != 0) && (mytable$bliz[i-1] != 0)) {
    mytable$bliz1[i] = mytable$bliz1[i-1] + 1
  }
}

mytable$time_only=strptime(mytable$obstime, format="%d/%m/%Y %H:%M")
subs=mytable[,-c(1,3,5,6)]   #NEED NOT BE THE SAME.
subs=subs[,c("time_only", "tempr", "bliz", "ws", "bliz1")]
names(myimdm)=c("time_only", "Temperature(C)", "Blizzard Occurence", "Wind Speed (knots)", "Duration of Blizzard (hrs)")

#(NAme as myiigb, myiigm, myimdm, mysase resp.)
myimdm = subs #copy the processed.

############################
station = c("iig_bharati", "iig_maitri", "imd_maitri", "sankalp_sase")
duration = c("(2012-2016)", "(2012-2015)", "(1985-2016)", "(2006-2015)")
input = c(0,"0000")

inputter = function(station, duration, input, myiigb, myiigm, myimdm, mysase) {
  input[1] = readline(prompt = "1. iig_bharati 2. iig_maitri 3. imd_maitri 4. sankalp_sase. Choice: ")
  stn = as.integer(input[1])
  input[2]=readline(prompt=paste("Enter Year for ", station[stn], duration[stn], " : "))
  if(input[1] == 1) {
    subs = myiigb
  } else
  if(input[1] == 2) {
    subs = myiigm
  } else
  if(input[1] == 3) {
    subs = myimdm     
  } else
  if(input[1] == 4) {
    subs = mysase
  }
  subs=subset(subs, format(time_only, format="%Y")==input[2])
  subs$time_only=as.POSIXct(subs$time_only)
  dat.m <- melt(subs, "time_only")
  names(dat.m)=c("Months", "variable", "value")
  plotter = ggplot(dat.m, aes(Months, value, colour = variable,group==1)) + geom_line() +
    facet_wrap(~ variable, ncol = 1, scales = "free_y")+
    scale_x_datetime(date_breaks = "1 month", labels = date_format("%b")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 1.0, hjust = 1.0)) +
    ggtitle(paste(station[stn], input[2]))+
    theme(plot.title = element_text(family = "Aileron", color="blue", face="bold", size=15, hjust=0))+
    theme(axis.title = element_text(family = "Aileron", color="red", face="bold", size=12))+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(panel.background = element_rect(fill = "#f6f6f6",colour = "#f6f6f6",size = 0.5, linetype = "solid"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "lightblue"),
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "white"))+
    theme(plot.background = element_rect(fill = "#fafafa"))+
    theme(strip.text.x = element_text(colour = "red", size = 15,hjust = 0.5, vjust = 0.5))
  
  return(plotter)
}

inputter(station, duration, input, myiigb, myiigm, myimdm, mysase)
