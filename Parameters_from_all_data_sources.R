library(ggplot2)
library(scales)
library(reshape)
library(dplyr)

#imd_maitri=imd_maitri[,c("obstime", "tempr", "rh", "ws", "wd", "ap")]
#sankalp_sase=sankalp_sase[,c("obstime", "tempr", "rh", "ws", "wd", "ap")]

station = c("iig_bharati", "iig_maitri", "imd_maitri", "sankalp_sase", "dozer")
duration = c("(2012-2016)", "(2012-2015)", "(1985-2016)", "(2006-2015)", "(2007-2015)")

input = c(0,"0000")
inputter = function(station, duration, input, iig_bharati, iig_maitri, imd_maitri, sankalp_sase, dozer_cleaned) {
  input[1] = readline(prompt = "1. iig_bharati 2. iig_maitri 3. imd_maitri 4. sankalp_sase. 5. dozer. Choice: ")
  stn = as.integer(input[1])
  input[2]=readline(prompt=paste("Enter Year for ", station[stn], duration[stn], " : "))
  if(input[1] == 1) {
    subs = iig_bharati
  } else
    if(input[1] == 2) {
      subs = iig_maitri
    } else
      if(input[1] == 3) {
        subs = imd_maitri 
      } else
        if(input[1] == 4) {
          subs = sankalp_sase
        } else
          if(input[1] == 5) {
            subs = dozer_cleaned
          } 
  if(input[1]==5)
  {
    subs$time_only=strptime(subs$obstime, format="%d/%m/%Y %H:%M")
  } else
  {
    subs$time_only=strptime(subs$obstime, format="%m/%d/%Y %H:%M")
  }
  subs=subset(subs, format(time_only, format="%Y")==input[2] & ap>900 & ws<1000)
  subs$time_only=as.POSIXct(subs$time_only)
  subs=subs[,-c(1,5)]
  names(subs)=c("Temperature(Degree Celsius)", "Humidity", "Wind Speed(knots)", "Air Pressure", "time_only")
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

inputter(station, duration, input, iig_bharati, iig_maitri, imd_maitri, sankalp_sase, dozer_cleaned)
