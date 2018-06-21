########################
queplot = function(date1, date2, valtempr, valrh, valws, valap) {
  subs = subset(mytable, as.character(as.Date(mytable$obstime, "%d/%m/%Y")) >= date1 & as.character(as.Date(mytable$obstime, "%d/%m/%Y")) <= date2)
  subs$time_only = strptime(subs$obstime, "%d/%m/%Y %H:%M")
  format(subs$time_only, "%H:%M:%S")
  
  if(valtempr) {
    return(qplot(subs$time_only, subs$tempr, geom = "line", xlab = "Time", ylab = "Temperature", 
                 main = "Temperature vs Time"))
  }
  if(valrh) {
    return(qplot(subs$time_only, subs$rh, geom = "line", xlab = "Time", ylab = "Humidity", 
                 main = "Humidity vs Time"))
  }
  if(valws) {
    return(qplot(subs$time_only, subs$ws, geom = "line", xlab = "Time", ylab = "Wind Speed", 
                 main = "Wind Speed vs Time"))
  }
  if(valap) {
    return(qplot(subs$time_only, subs$ap, geom = "line", xlab = "Time", ylab = "Air Pressure", 
                 main = "Air Pressure vs Time"))
  }
}

########################
queplotuser = function(date1, date2, valtempr, valrh, valws, valap) {
  subs = subset(mytable, as.character(as.Date(mytable$obstime, "%m/%d/%Y")) >= date1 & 
                as.character(as.Date(mytable$obstime, "%m/%d/%Y")) <= date2)
  subs$obstime=strptime(subs$obstime,format="%m/%d/%Y %H:%M")
  subs = subs[,-c(5)]
  names(subs)<-c("obstime","Temperature","Relative Humidity","Wind Speed","Air Pressure")
  if(!valap) {
    subs = subs[,-c(5)] 
  }
  if(!valws) {
    subs = subs[,-c(4)]
  }
  if(!valrh) {
    subs = subs[,-c(3)]
  }
  if(!valtempr) {
    subs = subs[,-c(2)]
  }
  subs$obstime = as.POSIXct(subs$obstime)
  subs = melt(subs, "obstime")
  return(ggplot(subs, aes(obstime, value, colour = variable, group==1)) + geom_line() +
           facet_wrap(~ variable, ncol = 1, scales = "free_y")+
           theme(plot.title = element_text(family = "Trebuchet MS", color="red", face="bold", size=15, hjust=0)) +
           theme(axis.title = element_text(family = "Trebuchet MS", color="red", face="bold", size=12))+
           theme(plot.title = element_text(hjust = 0.5))+
           theme(panel.background = element_rect(fill = "#f6f6f6",colour = "#f6f6f6",size = 0.5, linetype = "solid"),
                 panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "lightblue"), 
                 panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "white"))+
           theme(plot.background = element_rect(fill = "#fafafa"))+
           theme(strip.text.x = element_text(colour = "red", size = 15,hjust = 0.5, vjust = 0.5)))
}

########################
queplotfixdat = function(valtempr, valrh, valws, valap, choice1, yea, mon, dat) {
  subs = subset(mytable, format(as.Date(mytable$obstime, format = "%d/%m/%Y"),"%d")==dat & format(as.Date(mytable$obstime, format = "%d/%m/%Y"),"%m")==mon & format(as.Date(mytable$obstime, format = "%d/%m/%Y"),"%Y")==yea)
  subs$obstime=strptime(subs$obstime,format="%d/%m/%Y %H:%M")
  subs = subs[,-c(5)]
  names(subs)<-c("obstime","Temperature","Relative Humidity","Wind Speed","Air Pressure")
  if(!valap) {
    subs = subs[,-c(5)] 
  }
  if(!valws) {
    subs = subs[,-c(4)]
  }
  if(!valrh) {
    subs = subs[,-c(3)]
  }
  if(!valtempr) {
    subs = subs[,-c(2)]
  }
  subs$obstime = as.POSIXct(subs$obstime)
  subs = melt(subs, "obstime")
  return(ggplot(subs, aes(obstime, value, colour = variable, group==1)) + geom_line() +
           facet_wrap(~ variable, ncol = 1, scales = "free_y")+
           theme(plot.title = element_text(family = "Trebuchet MS", color="red", face="bold", size=15, hjust=0)) +
           theme(axis.title = element_text(family = "Trebuchet MS", color="red", face="bold", size=12))+
           theme(plot.title = element_text(hjust = 0.5))+
           theme(panel.background = element_rect(fill = "#f6f6f6",colour = "#f6f6f6",size = 0.5, linetype = "solid"),
                 panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "lightblue"), 
                 panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "white"))+
           theme(plot.background = element_rect(fill = "#fafafa"))+
           theme(strip.text.x = element_text(colour = "red", size = 15,hjust = 0.5, vjust = 0.5)))
}

queplotfixmon = function(valtempr, valrh, valws, valap, choice1, yea, mon, dat) {
  subs = subset(mytable, format(as.Date(mytable$obstime, format = "%d/%m/%Y"),"%m") == mon & format(as.Date(mytable$obstime, format = "%d/%m/%Y"),"%Y") == yea)
  subs$obstime=strptime(subs$obstime,format="%d/%m/%Y %H:%M")
  subs = subs[,-c(5)]
  names(subs)<-c("obstime","Temperature","Relative Humidity","Wind Speed","Air Pressure")
  if(!valap) {
    subs = subs[,-c(5)] 
  }
  if(!valws) {
    subs = subs[,-c(4)]
  }
  if(!valrh) {
    subs = subs[,-c(3)]
  }
  if(!valtempr) {
    subs = subs[,-c(2)]
  }
  subs$obstime = as.POSIXct(subs$obstime)
  subs = melt(subs, "obstime")
  return(ggplot(subs, aes(obstime, value, colour = variable, group==1)) + geom_line() +
           facet_wrap(~ variable, ncol = 1, scales = "free_y")+
           theme(plot.title = element_text(family = "Trebuchet MS", color="red", face="bold", size=15, hjust=0)) +
           theme(axis.title = element_text(family = "Trebuchet MS", color="red", face="bold", size=12))+
           theme(plot.title = element_text(hjust = 0.5))+
           theme(panel.background = element_rect(fill = "#f6f6f6",colour = "#f6f6f6",size = 0.5, linetype = "solid"),
                 panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "lightblue"), 
                 panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "white"))+
           theme(plot.background = element_rect(fill = "#fafafa"))+
           theme(strip.text.x = element_text(colour = "red", size = 15,hjust = 0.5, vjust = 0.5)))
}

queplotfixyea = function(valtempr, valrh, valws, valap, choice1, yea, mon, dat) {
  subs = subset(mytable, format(as.Date(mytable$obstime, format = "%d/%m/%Y"),"%Y") == yea)
  subs$obstime=strptime(subs$obstime,format="%d/%m/%Y %H:%M")
  subs = subs[,-c(5)]
  names(subs)<-c("obstime","Temperature","Relative Humidity","Wind Speed","Air Pressure")
  if(!valap) {
    subs = subs[,-c(5)] 
  }
  if(!valws) {
    subs = subs[,-c(4)]
  }
  if(!valrh) {
    subs = subs[,-c(3)]
  }
  if(!valtempr) {
    subs = subs[,-c(2)]
  }
  subs$obstime = as.POSIXct(subs$obstime)
  subs = melt(subs, "obstime")
  return(ggplot(subs, aes(obstime, value, colour = variable, group==1)) + geom_line() +
           facet_wrap(~ variable, ncol = 1, scales = "free_y")+
           theme(plot.title = element_text(family = "Trebuchet MS", color="red", face="bold", size=15, hjust=0)) +
           theme(axis.title = element_text(family = "Trebuchet MS", color="red", face="bold", size=12))+
           theme(plot.title = element_text(hjust = 0.5))+
           theme(panel.background = element_rect(fill = "#f6f6f6",colour = "#f6f6f6",size = 0.5, linetype = "solid"),
                 panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "lightblue"), 
                 panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "white"))+
           theme(plot.background = element_rect(fill = "#fafafa"))+
           theme(strip.text.x = element_text(colour = "red", size = 15,hjust = 0.5, vjust = 0.5)))
}