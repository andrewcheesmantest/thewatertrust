####################################################################################################
# Testing the credentials that Evan Thomas provided for the SD API
# Jan 3, 2016

library(RMySQL)
library(DBI)

con <-  dbConnect(RMySQL::MySQL(), 
                  dbname = "TWT",
                  username = "TWT",
                  password = "uganda",
                  host = "sweetsensors.c20to7gqsth3.us-east-1.rds.amazonaws.com",
                  port = 3306)

dbReadTable(conn = con, name = "mdl_user")

dbDisconnect(con)












