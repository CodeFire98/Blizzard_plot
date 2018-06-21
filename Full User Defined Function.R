library(ggplot2)
library(scales)

#Assigning File name(64601 observations) to variable mytable
setwd("C:/Users/Lakshith/Desktop/NCAOR/BigSample")

mytable=Final_File

#Input the choice: 1.Daily 2.Weekly 3.Bi-Monthly 4.Monthly 5.Yearly
print("1.Of a Day \n2.Of 15 days of a Month \n3.Of a Month \n4.Of a year \n5.Yearly")
Choice=readline(prompt="Choice: ")

#For a day's data
if(Choice==1)
{
  #read a day, month and year
  d=readline(prompt="Date of the month: ")
  mon = readline(prompt = "Month: ")
  yea = readline(prompt = "Year: ")
  #Make a subset of data, subs with the given day, month and year
  subs = subset(mytable, format(as.Date(mytable$obstime, format = "%d/%m/%Y"),"%d")==d & format(as.Date(mytable$obstime, format = "%d/%m/%Y"),"%m")==mon & format(as.Date(mytable$obstime, format = "%d/%m/%Y"),"%Y")==yea)
  #Make a array with standard form of dates given
  subs$time_only = strptime(subs$obstime, "%d/%m/%Y %H:%M")
  #Get the dates in hour:min:sec format
  format(subs$time_only, "%H:%M:%S")
  #assign the Date of the day to dat for labelling x-axis
  dat=as.Date(subs$time_only[1], "%d/%m/%Y")
  #Plot Temperature v/s Time for a day with breaks of 1 hour
  jpeg("Daily_highres.jpeg", width = 15, height = 10, units = 'in', res = 500)
  qplot(subs$time_only, subs$tempr, geom = "line", xlab = dat, ylab = "Temperature", 
        main = "Temperature vs Time") +
    scale_x_datetime(date_breaks = "1 hour", labels = date_format("%d, %H:%M")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 1.0, hjust = 1.0)) +
    geom_line(aes(y=subs$tempr), colour = "red") + 
    geom_line(aes(y=subs$ws), colour = "green") +
    geom_line(aes(y=subs$rh), colour = "blue")
  dev.off()
  #remove subset subs
  rm(subs)
  rm(d, mon, yea, dat)
}

#For 15 day's data
if(Choice==2)
{
  #read a day, month and year
  print("Part of the month: \n1st Part= 1-15 \n2nd Part= 16-30/31 \n")
  d=readline(prompt="Choice(1/2): ")
  mon = readline(prompt = "Month: ")
  yea = readline(prompt = "Year: ")
  subs = subset(mytable, d==1 & format(as.Date(mytable$obstime, format = "%d/%m/%Y"),"%d")<=15 & format(as.Date(mytable$obstime, format = "%d/%m/%Y"),"%m")==mon & format(as.Date(mytable$obstime, format = "%d/%m/%Y"),"%Y")==yea)
  subs = subset(mytable, d==2 & format(as.Date(mytable$obstime, format = "%d/%m/%Y"),"%d")>15 & format(as.Date(mytable$obstime, format = "%d/%m/%Y"),"%d")<=31 & format(as.Date(mytable$obstime, format = "%d/%m/%Y"),"%m")==mon & format(as.Date(mytable$obstime, format = "%d/%m/%Y"),"%Y")==yea)
  subs$time_only = strptime(subs$obstime, "%d/%m/%Y %H:%M")
  format(subs$time_only, "%H:%M:%S")
  month=format(as.Date(subs$time_only[1], format="%d/%m/%Y"), "%b") 
  #Plot Temperature v/s Time for 15 days with breaks of 1 day
  jpeg("Bi_monthly_highres.jpeg", width = 15, height = 10, units = 'in', res = 500)
  qplot(subs$time_only, subs$tempr, geom = "line", xlab =month, ylab = "Temperature", 
        main = "Temperature vs Time") +
    scale_x_datetime(date_breaks = "1 day", labels = date_format("%d")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 1.0, hjust = 1.0)) +
    geom_line(aes(y=subs$tempr), colour = "red") + 
    geom_line(aes(y=subs$ws), colour = "green") +
    geom_line(aes(y=subs$rh), colour = "blue")
  dev.off()
  rm(subs)
  rm(d, mon, yea, month)
}

#For a month's data
if(Choice==3)
{
  #read a month and year
  mon = readline(prompt = "Month: ")
  yea = readline(prompt = "Year: ")
  subs = subset(mytable, format(as.Date(mytable$obstime, format = "%d/%m/%Y"),"%m") == mon & format(as.Date(mytable$obstime, format = "%d/%m/%Y"),"%Y") == yea)
  subs$time_only = strptime(subs$obstime, "%d/%m/%Y %H:%M")
  format(subs$time_only, "%H:%M:%S")
  month=format(as.Date(subs$time_only[1], "%d/%m/%Y"), "%b")
  #Plot Temperature v/s Time for a month with breaks of 1 day
  jpeg("Monthly_highres.jpeg", width = 15, height = 10, units = 'in', res = 500)
  qplot(subs$time_only, subs$tempr, geom = "line", xlab =month, ylab = "Temperature", 
      main = "Temperature vs Time") +
    scale_x_datetime(date_breaks = "1 day", labels = date_format("%d")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 1.0, hjust = 1.0)) +
    geom_line(aes(y=subs$tempr), colour = "red") + 
    geom_line(aes(y=subs$ws), colour = "green") +
    geom_line(aes(y=subs$rh), colour = "blue")
  dev.off()
  rm(subs)
  rm(mon, yea, month)
}

#For a year's data
if(Choice==4)
{
  #read a year
  yea = readline(prompt = "Year: ")
  subs = subset(mytable, format(as.Date(mytable$obstime, format = "%d/%m/%Y"),"%Y") == yea)
  subs$time_only = strptime(subs$obstime, "%d/%m/%Y %H:%M")
  format(subs$time_only, "%H:%M:%S")
  #Plot Temperature v/s Time for a year with breaks of 1 month
  jpeg("Year_highres.jpeg", width = 15, height = 10, units = 'in', res = 500)
  qplot(subs$time_only, subs$tempr, geom = "line", xlab =yea, ylab = "Temperature", 
        main = "Temperature vs Time") +
    scale_x_datetime(date_breaks = "1 month", labels = date_format("%b")) +
    theme(axis.text.x = element_text(angle = 0, vjust = 1.0, hjust = 1.0)) +
    geom_line(aes(y=subs$tempr), colour = "red") + 
    geom_line(aes(y=subs$ws), colour = "green") +
    geom_line(aes(y=subs$rh), colour = "blue")
  dev.off()
  rm(subs)
  rm(yea)
}

if(Choice==5)
{
  #read a start year and end year
  start_yea = readline(prompt = "Start Year: ")
  end_yea = readline(prompt = "End Year: ")
  subs = subset(mytable, format(as.Date(mytable$obstime, format = "%d/%m/%Y"),"%y")>=start_yea & format(as.Date(mytable$obstime, format = "%d/%m/%Y"),"%Y")<=end_yea)
  subs$time_only = strptime(subs$obstime, "%d/%m/%Y %H:%M")
  format(subs$time_only, "%H:%M:%S")
  #Plot Temperature v/s Time for a year with breaks of 1 month
  jpeg("Yearly_highres.jpeg", width = 15, height = 10, units = 'in', res = 500)
  qplot(subs$time_only, subs$tempr, geom = "line", xlab ="Time", ylab = "Temperature", 
        main = "Temperature vs Time") +
    scale_x_datetime(date_breaks = "1 month", labels = date_format("%b %Y")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 1.0, hjust = 1.0)) +
    geom_line(aes(y=subs$tempr), colour = "red") + 
    geom_line(aes(y=subs$ws), colour = "green") +
    geom_line(aes(y=subs$rh), colour = "blue")
  dev.off()
  rm(subs)
  rm(start_yea, end_yea)
}

######################################################################################################

jpeg("Trialplot_highres.jpeg", width = 15, height = 10, units = 'in', res = 500)
qplot(subs$time_only, subs$tempr, geom = "line", xlab =yea, ylab = "Temperature", 
      main = "Temperature vs Time") +
  scale_x_datetime(date_breaks = "1 month", labels = date_format("%b")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1.0, hjust = 1.0)) +
  geom_line(aes(y=subs$tempr), colour = "red") + 
  geom_line(aes(y=subs$ws), colour = "green") +
  geom_line(aes(y=subs$rh), colour = "blue")
dev.off()

subs = mytable[i]

for (i in 2:6) {
  qplot(mytable$obstime, mytable[i], geom = "line", xlab =yea, ylab = "Temperature", 
        main = "Temperature vs Time") +
    scale_x_datetime(date_breaks = "1 month", labels = date_format("%b")) +
    theme(axis.text.x = element_text(angle = 0, vjust = 1.0, hjust = 1.0))
}

rm(dir)
dir = 1:8
for (i in 1:8) {
  dir[i] = 0
}

mytable = oneysample

for (i in 1:nrow(mytable)) {
  if((mytable$wd[i]>=0)&(mytable$wd[i]<45))
     dir[1] = dir[1] + 1
  if((mytable$wd[i]>=45)&(mytable$wd[i]<90))
    dir[2] = dir[2] + 1
  if((mytable$wd[i]>=90)&(mytable$wd[i]<135))
    dir[3] = dir[3] + 1
  if((mytable$wd[i]>=135)&(mytable$wd[i]<180))
    dir[4] = dir[4] + 1
  if((mytable$wd[i]>=180)&(mytable$wd[i]<225))
    dir[5] = dir[5] + 1
  if((mytable$wd[i]>=225)&(mytable$wd[i]<270))
    dir[6] = dir[6] + 1
  if((mytable$wd[i]>=270)&(mytable$wd[i]<315))
    dir[7] = dir[7] + 1
  if((mytable$wd[i]>=315)&(mytable$wd[i]<360))
    dir[8] = dir[8] + 1
}

dir

for (i in 1:nrow(mytable)) {
  print(mytable$wd[i])
}

install.packages("plotly")
library(plotly)

plot_ly(x=c("0-45","45-90","90-135","135-180","180-225","225-270","270-315","315-360"),
     y=c(dir[1],dir[2],dir[3],dir[4],dir[5],dir[6],dir[7],dir[8]),name = "Direction", type="bar")

x=c("0-45","45-90","90-135","135-180","180-225","225-270","270-315","315-360")
y=c(dir[1],dir[2],dir[3],dir[4],dir[5],dir[6],dir[7],dir[8])
barplot(y, names.arg = x, xlab="Degrees", ylab="Frequency", main = "Direction")

windRose(mytable, ws = "wind Speed", wd = "wind Dir", ws2 = NA, wd2 = NA, 
         ws.int = 1, angle = 10, type = "default", bias.corr = TRUE, cols
         = "default", grid.line = 10, width = 1, seg = NULL, breaks = 6, 
         offset = 10, normalise = FALSE, paddle = TRUE, 
         key.header = "Wind Rose", key.footer = "(km/hr)", key.position = "bottom", 
         key = TRUE, dig.lab = 2, statistic = "prop.count", pollutant = NULL,
         annotate = TRUE, angle.scale = 315)

install.packages("openair")
library(openair)

jpeg("roseplot1.jpg", width = 15, height = 15, units = "in", res = 500)
windRose(mytable, angle = 5, breaks = 8, key.footer = "(km/hr)", 
         key.position = "top", grid.line = 2)
dev.off()
