install.packages('rsconnect')

rsconnect::setAccountInfo(name='luckydin',
                          token='D69EDE518F169AE26ACD4820E6828C2F',
                          secret='M/m+WeM7RwMrNSM8EF9ukpO5IEq/vij0EyDg4ARP')

library(rsconnect)
rsconnect::deployApp('C:\\Users\\Lakshith\\Desktop\\NCAOR\\weatheranal')

install.packages("shiny")

rsconnect::setAccountInfo(name='vedashree',
                          token='549B681CA93010C689103BEF1D5D7C8F',
                          secret='19P6QUy1ROCWOk2dzJlPXP6H/J2WHVMIjKSwjzv4')
deployApp()
