#imd_maitri=imd_maitri[,c("obstime", "tempr", "rh", "ws", "wd", "ap")]
#sankalp_sase=sankalp_sase[,c("obstime", "tempr", "rh", "ws", "wd", "ap")]

#station = c("iig_bharati", "iig_maitri", "imd_maitri", "sankalp_sase", "dozer")
#stn=1
#subs=iig_bharati

inputter = function(date1, date2, valtempr, valrh, valws, valap, choice) {
  station = c("iig_bharati", "iig_maitri", "imd_maitri", "sankalp_sase", "dozer")
  stn=1
  if(choice=="iigb") {
    subs = iig_bharati
    stn=1
  } else
    if(choice=="iigm") {
      subs = iig_maitri
      stn=2
    } else
      if(choice=="imdm") {
        imd_maitri=imd_maitri[,c("obstime", "tempr", "rh", "ws", "wd", "ap")]
        subs = imd_maitri
        stn=3
      } else
        if(choice=="ssase") {
          sankalp_sase=sankalp_sase[,c("obstime", "tempr", "rh", "ws", "wd", "ap")]
          subs = sankalp_sase
          stn=4
        } else
          if(choice=="dozer") {
            subs = dozer_cleaned
            stn=5
          } 
  if(choice=="dozer")
  {
    subs$time_only=strptime(subs$obstime, format="%d/%m/%Y %H:%M")
  } else
  {
    subs$time_only=strptime(subs$obstime, format="%m/%d/%Y %H:%M")
  }
  subs = subset(subs, as.character(as.Date(subs$time_only, "%Y-%m-%d"))>=date1 & as.character(as.Date(subs$time_only, "%Y-%m-%d"))<=date2 & ap>900 & ws<1000)
  subs$time_only=as.POSIXct(subs$time_only)
  subs=subs[,-c(1,5)]
  names(subs)=c("Temperature(Degree Celsius)", "Humidity", "Wind Speed(knots)", "Air Pressure", "time_only")
  if(!valap) {
    subs = subs[,-c(4)] 
  }
  if(!valws) {
    subs = subs[,-c(3)]
  }
  if(!valrh) {
    subs = subs[,-c(2)]
  }
  if(!valtempr) {
    subs = subs[,-c(1)]
  }
  subs$grp=factor(c(0, cumsum(diff(subs$time_only)>1)))
  dat.m=melt(as.data.frame(subs), id.vars=c("time_only", "grp"))
  names(dat.m)=c("Months", "Group", "variable", "value")
  plotter = ggplot(dat.m, aes(Months, value, colour = variable,group==1)) + geom_line(aes(group=Group)) +
    facet_wrap(~ variable, ncol = 1, scales = "free_y")+
    scale_x_datetime(date_breaks = "1 month", labels = date_format("%b")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 1.0, hjust = 1.0)) +
    ggtitle(paste(station[stn], date1, "TO", date2))+
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
