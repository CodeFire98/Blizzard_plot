source("db_comm.R")

inputter = function(choice, choice1, choice2, valtemprb, valrhb, valwsb, valapb, valtemprant, valwsant, valmslpant, valtempr, valrh, valws, valap, date1, date2, date3, date4, date5, date6, date7) {
  library(ggplot2)
  library(scales)
  library(reshape2)
  library(dplyr)
  library(zoo)
  con = dbcon("Lakshith1", "localhost", 5444, "postgres", "ncaor@123")
  station = c("IIG_Bharati", "IMD_Bharati", "IIG_Maitri", "IMD_Maitri", "Sankalp_SASE Maitri", "Dozer_SASE Maitri", "Surface Data Maitri")
  date=c(0,"00")
  stn=1
  if(choice=="b") {
    if(choice1=="iigb")
    {
      subs = dbReadTable(con, "iig_bharati")
      stn=1
      date=date1
    }else
      if(choice1=="imdb")
      {
        imd_bharati = dbReadTable(con, "imd_bharati")
        imd_bharati=imd_bharati[,c("obstime", "tempr", "rh", "ws", "wd", "ap")]
        subs = imd_bharati
        stn=2
        date=date2
      } 
  } else
    if(choice=="m") {
      if(choice2=="iigm") {
        subs = dbReadTable(con, "iig_maitri")
        stn=3
        date=date3
        } else
          if(choice2=="imdm") {
            imd_maitri = dbReadTable(con, "imd_maitri")
            imd_maitri=imd_maitri[,c("obstime", "tempr", "rh", "ws", "wd", "ap")]
            subs = imd_maitri
            stn=4
            date=date4
            } else
              if(choice2=="ssasem") {
                sankalp_sase = dbReadTable(con, "Sankalp_SASE")
                sankalp_sase=sankalp_sase[,c("obstime", "tempr", "rh", "ws", "wd", "ap")]
                subs = sankalp_sase
                stn=5
                date=date5
                } else
                  if(choice2=="dozerm") {
                    subs = dbReadTable(con, "dozer")
                    stn=6
                    date=date6
                    } else
                      if(choice2=="ant_tb3m") {
                        subs = dbReadTable(con, "Surface_Data")       
                        stn=7
                        date=date7
                        subs$time_only=strptime(paste(subs$YEAR,"-",subs$MN,"-",subs$DT," ",subs$HR,":00:00"), "%Y - %m - %d %H :%M:%S")
                        subs = subset(subs, as.character(as.Date(subs$time_only, "%Y-%m-%d"))>=date[1] & as.character(as.Date(subs$time_only, "%Y-%m-%d"))<=date[2]) 
                        subs=subset(subs, MSLP<1033 & MSLP>940 & WS<60 & MSLP>940)
                        subs$tempr=subs$DBT
                        subs$ws=subs$WS
                        subs$mslp=subs$MSLP
                        subs=subs[-c(1:18)]
                        subs$temprm = rollmean(subs$tempr, k=8, fill=NA)
                        subs$wsm = rollmean(subs$ws, k=8, fill=NA)
                        subs$mslpm = rollmean(subs$mslp, k=8, fill=NA)
                        subs = subs[,-c(2,3,4)]
                        subs = subset(subs, format(time_only, "%H:%M:%S") == "12:00:00")
                        subs$time_only=as.POSIXct(subs$time_only)
                        names(subs)<-c("time_only","Temperature", "Wind Speed", "Mean Sea Level Pressure")
                        subs$grp=factor(c(0, cumsum(diff(subs$time_only)>3)))
                        if(!valmslpant) {
                          subs = subs[,-c(4)] 
                        }
                        if(!valwsant) {
                          subs = subs[,-c(3)]
                        }
                        if(!valtemprant) {
                          subs = subs[,-c(2)]
                        }
                      }
    }
  if(choice2!="ant_tb3m") {
    if(choice2 == "dozerm") {
      subs$time_only=strptime(subs$obstime, format="%d/%m/%Y %H:%M")
      subs = subset(subs, as.character(as.Date(subs$time_only, "%Y-%m-%d"))>=date[1] & as.character(as.Date(subs$time_only, "%Y-%m-%d"))<=date[2] & ap>900 & ws<50)  
    } else {
      subs$time_only=strptime(subs$obstime, format="%m/%d/%Y %H:%M")
      subs = subset(subs, as.character(as.Date(subs$time_only, "%Y-%m-%d"))>=date[1] & as.character(as.Date(subs$time_only, "%Y-%m-%d"))<=date[2] & ap>900 & ws<150)
    } #subs=subset(subs, ap>900 & ws<1000)
    subs$time_only=as.POSIXct(subs$time_only)
    subs=subs[,-c(1,5)]
    names(subs)=c("Temperature(Degree Celsius)", "Humidity", "Wind Speed(knots)", "Air Pressure", "time_only")
    if(choice=="b") {
      if(!valapb) {
        subs = subs[,-c(4)] 
      }
      if(!valwsb) {
        subs = subs[,-c(3)]
      }
      if(!valrhb) {
        subs = subs[,-c(2)]
      }
      if(!valtemprb) {
        subs = subs[,-c(1)]
      }
    } else {
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
    }
  }
  if(choice2=="dozerm") {
    dat.m=melt(as.data.frame(subs), id.vars="time_only")
    names(dat.m)=c("Months", "variable", "value")
    plotter = ggplot(dat.m, aes(Months, value, colour = variable,group==1)) + geom_line() +
    facet_wrap(~ variable, ncol = 1, scales = "free_y")+
    scale_x_datetime(date_breaks = "1 year", labels = date_format("%Y")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 1.0, hjust = 1.0)) +
    ggtitle(paste(station[stn], ": ", date[1], " TO ", date[2]))+
    theme(plot.title = element_text(family = "Aileron", color="blue", face="bold", size=15, hjust=0))+
    theme(axis.title = element_text(family = "Aileron", color="red", face="bold", size=12))+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(panel.background = element_rect(fill = "#f6f6f6",colour = "#f6f6f6",size = 0.5, linetype = "solid"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "lightblue"),
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "white"))+
    theme(plot.background = element_rect(fill = "#fafafa"))+
    theme(strip.text.x = element_text(colour = "red", size = 15,hjust = 0.5, vjust = 0.5))
  } else {
    subs$grp=factor(c(0, cumsum(diff(subs$time_only)>1)))
    dat.m=melt(as.data.frame(subs), id.vars=c("time_only", "grp"))
    names(dat.m)=c("Months", "Group", "variable", "value")
    plotter = ggplot(dat.m, aes(Months, value, colour = variable,group==1)) + geom_line(aes(group=Group)) +
      facet_wrap(~ variable, ncol = 1, scales = "free_y")+
      scale_x_datetime(date_breaks = "1 year", labels = date_format("%Y")) +
      theme(axis.text.x = element_text(angle = 90, vjust = 1.0, hjust = 1.0)) +
      ggtitle(paste(station[stn], ": ", date[1], " TO ", date[2]))+
      theme(plot.title = element_text(family = "Aileron", color="blue", face="bold", size=15, hjust=0))+
      theme(axis.title = element_text(family = "Aileron", color="red", face="bold", size=12))+
      theme(plot.title = element_text(hjust = 0.5))+
      theme(panel.background = element_rect(fill = "#f6f6f6",colour = "#f6f6f6",size = 0.5, linetype = "solid"),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "lightblue"),
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "white"))+
      theme(plot.background = element_rect(fill = "#fafafa"))+
      theme(strip.text.x = element_text(colour = "red", size = 15,hjust = 0.5, vjust = 0.5))
  } 
  dbdiscon(con)
  return(plotter)
}
