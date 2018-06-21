require(RPostgreSQL)
require(RPostgres)
require(DBI)

#dbname = "Lakshith1", 
#host = "localhost", 
#port = 5444, 
#user = "postgres", 
#password = "ncaor@123"

dbcon = function() {
  drv = dbDriver("PostgreSQL")
  con = dbConnect(drv, dbname = "Lakshith1", 
                  host = "localhost", 
                  port = 5444, user = "postgres", 
                  password = "ncaor@123")
  return(con)
}

con = dbcon()

dbWriteTable(con, "sankalp_sase", sankalp_sase)
mytable = dbReadTable(con, "sankalp_sase")

my_data <- data.frame(carname = rownames(mtcars), mtcars, row.names = NULL)  
my_data$carname <- as.character(my_data$carname)  
rm(mtcars)

dbGetQuery(con, 'ALTER TABLE cars RENAME TO cars_info;')

dbDisconnect(con)
