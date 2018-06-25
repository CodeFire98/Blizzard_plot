dbcon = function(dbname1, host1, port1, user1, password1) {
  require(RPostgreSQL)
  require(RPostgres)
  require(DBI)
  drv = dbDriver("PostgreSQL")
  con = dbConnect(drv, dbname = dbname1, 
                  host = host1, 
                  port = port1, user = user1, 
                  password = password1)
  return(con)
}

dbdiscon = function(con) {
  dbDisconnect(con)
}

